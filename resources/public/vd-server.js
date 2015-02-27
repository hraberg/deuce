'use strict';

// The example from https://github.com/Matt-Esch/virtual-dom split into a server rendering / client patching spike.
// virtual-dom isn't really used well, as all patching is done in text and the tree is then recreated client side.

const diff = require('diff'),
      ws = require('ws'),
      fs = require('fs'),
      url = require('url'),
      path = require('path'),
      jsdom = require('jsdom').jsdom,
      DiffDom = require('diff-dom'),
      jsonpatch = require('fast-json-patch');

/*

potential view model as JSON;

{"frame":
    {"height":40,
     "width":139,
     "menu-bar":["File","Edit","Options","Tools","Buffers","Help"],
     "windows":[
        {"sequence-number":0,
         "height":39,
         "width":134,
         "start":1,
         "end":195,
         "point":195,
         "line-number-at-start":1,
         "line-number-at-end":30,
         "live-p":true,
         "selected":true,
         "buffer":
            {"name":"*scratch*",
             "point":195,
             "mark":null,
             "min":1,
             "max":195,
             "line-number-at-point":5,
             "current-column":0,
             "major-mode":"fundametal-mode",
             "minor-modes":["paredit-mode"],
             "current":true,
             "text":[";; This buffer ..\n",";; If you want\n"]},
         "mode-line":"-:-- *scratch    Top ----"},
        {"sequence-number":1,
         "height":1,
         "width":134,
         "start":1,
         "end":1,
         "point":1,
         "line-number-at-start":1,
         "line-number-at-end":1,
         "mini-p":true,
         "buffer":
            {"name":" *Minibuf-0*",
             "point":1,
             "mark":null,
             "min":1,
             "max":1,
             "line-number-at-point":1,
             "current-column":0,
             "major-mode":"minibuffer-inactive-mode",
             "minor-modes":[],
             "text":[""]}}]}}

similar view model, as custom elements:

<frame-d height="40" width="134">
  <menu-bar-d>
    <menu-d>File</menu-d>
    <menu-d>Edit</menu-d>
    <menu-d>Options</menu-d>
    <menu-d>Tools</menu-d>
    <menu-d>Buffers</menu-d>
    <menu-d>Help</menu-d>
  </menu-bar-d>
  <window-d height="39" width="134" start="1" end="400" point="10"
            line-number-at-start="1" line-number-at-end="30"
            live-p selected>
    <buffer-d name="*scratch*" point="10" mark="" min="1" max="1300"
              line-number-at-point="4" current-column="10"
              major-mode="fundamental-mode" minor-modes="paredit-mode" current>
      <line-d number="1"></line-d>
    </buffer-d>
    <mode-line-d height="1"></mode-line-d>
  </window-d>
  <window-d height="1" width="134" mini-p></window-d>
</frame-d>

*/

let serialize = JSON.stringify,
    deserialize = JSON.parse;

function toDom(root, h) {
    root.innerHTML = h;
    return root;
}

function render(s) {
    let count = s.count;
    return '<div style=\"text-align:center;line-height:' + (100 + count) +
        'px;border:1px solid red;width:' + (100 + count) + 'px;height:' +
        (100 + count) + 'px;\">' + count + '</div>';
}

let state = {count: 0},
    serializedState,
    document = jsdom(),
    html,
    tree,
    revision = 0,
    connections = [],
    dd = new DiffDom();

ws.createServer({port: 8080}, (ws) => {
    let parameters = url.parse(ws.upgradeReq.url, true).query;
    connections.push({ws: ws,
                      dom: JSON.parse(parameters.dom),
                      json: JSON.parse(parameters.json),
                      state: JSON.parse(parameters.state)});
    let id = connections.length - 1,
        onrefresh = () => {
            fs.open(path.join(__dirname, 'vd-client-bundle.js'), 'r', (err, fd) => {
                if (err) {
                    throw (err);
                }
                html = render(state);
                tree = toDom(document.createElement('body'), html);
                serializedState = serialize(state);
                let data = serialize(['r', revision, html, state, fs.fstatSync(fd).mtime, Date.now()]);
                console.log(' refresh:', data);
                ws.send(data);
            });
        };
    ws.on('close', () => {
        console.log('disconnect:', id);
        connections.splice(id, 1);
    });
    ws.on('message', (data) => {
        let message = deserialize(data);
        ({r: onrefresh})[message[0]].apply(null, message.slice(1));
    });
    console.log('new client:', id);
    onrefresh();
});

function toSimpleCharDiff(d) {
    if (d.added) {
        return d.value;
    }
    if (d.removed) {
        return -d.value.length;
    }
    return d.value.length;
}

setInterval(() => {
    let startTime = Date.now(),
        newState = {count: state.count + 1};

    if (connections.length > 0) {
        let charData, domData, jsonData, stateData;
        connections.forEach((c) => {
            let data;
            console.time('    diff');
            if (c.json) {
                if (!jsonData) {
                    jsonData = serialize(['j', revision, jsonpatch.compare(state, newState), startTime]);
                }
                data = jsonData;
            } else if (c.dom) {
                if (!domData) {
                    let diffs,
                        newHtml = render(newState),
                        newTree = toDom(document.createElement('body'), newHtml);
                    console.log('rendered:', newHtml);
                    diffs = dd.diff(tree, newTree);
                    domData = serialize(['d', revision, diffs, startTime]);
                    tree = newTree;
                    html = newHtml;
                }
                data = domData;
            } else if (c.state) {
                if (!stateData) {
                    let newSerializedState = serialize(newState),
                        diffs = diff.diffChars(serializedState, newSerializedState).map(toSimpleCharDiff);
                    stateData = serialize(['s', revision, diffs, startTime]);
                    serializedState = newSerializedState;
                }
                data = stateData;
            } else {
                if (!charData) {
                    let newHtml = render(newState),
                        diffs = diff.diffChars(html, newHtml).map(toSimpleCharDiff);
                    console.log('rendered:', newHtml);
                    charData = serialize(['c', revision, diffs, startTime]);
                    html = newHtml;
                }
                data = charData;
            }
            console.timeEnd('    diff');
            console.log(' sending:', data);
            c.ws.send(data);
        });
    }

    revision = revision + 1;
    state = newState;
}, 1000);
