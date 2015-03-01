'use strict';

const diff = require('diff'),
      ws = require('ws'),
      fs = require('fs'),
      path = require('path');

let state = {'frame':
             {'menu-bar': ['File', 'Edit', 'Options', 'Tools', 'Buffers', 'Help'],
              'minor-modes': ['blink-cursor-mode', 'menu-bar-mode'],
              'windows': [
                  {'sequence-number': 0,
                   'line-number-at-start': 1,
                   'live-p': true,
                   'selected': true,
                   'buffer':
                   {'name': '*scratch*',
                    'line-number-at-point-max': 5,
                    'line-number-at-point': 5,
                    'current-column': 0,
                    'major-mode': 'lisp-interaction-mode',
                    'minor-modes': [],
                    'current': true,
                    'text': [';; This buffer is for notes you don\'t want to save, and for Lisp evaluation.', ';; If you want to create a file, visit that file with C-x C-f,', ';; then enter the text in that file\'s own buffer.', '', '']},
                   'mode-line': '-UUU:----F1  <strong>*scratch*</strong>      All L1     (Lisp Interaction) ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------'},
                  {'sequence-number': 1,
                   'line-number-at-start': 1,
                   'live-p': true,
                   'mini-p': true,
                   'buffer':
                   {'name': ' *Minibuf-0*',
                    'line-number-at-point-max': 1,
                    'line-number-at-point': 1,
                    'current-column': 0,
                    'major-mode': 'minibuffer-inactive-mode',
                    'minor-modes': [],
                    'text': ['Welcome to GNU Emacs']}}]}},
    serializedState,
    revision = 0,
    connections = [];

ws.createServer({port: 8080}, (ws) => {
    connections.push({ws: ws});
    let id = connections.length - 1,
        onrefresh = () => {
            fs.open(path.join(__dirname, 'components.js'), 'r', (err, fd) => {
                if (err) {
                    throw (err);
                }
                serializedState = JSON.stringify(state);
                let data = JSON.stringify(['r', revision, state, fs.fstatSync(fd).mtime, Date.now()]);
                console.log(' refresh:', data);
                ws.send(data);
            });
        };
    ws.on('close', () => {
        console.log('disconnect:', id);
        connections.splice(id, 1);
    });
    ws.on('message', (data) => {
        let message = JSON.parse(data);
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
        newState = state;
    newState.frame.windows[1].buffer.text[0] = new Date().toString();

    if (connections.length > 0) {
        let stateData;
        connections.forEach((c) => {
            let data;
            console.time('    diff');

            if (!stateData) {
                let newSerializedState = JSON.stringify(newState),
                diffs = diff.diffChars(serializedState, newSerializedState).map(toSimpleCharDiff);
                stateData = JSON.stringify(['s', revision, diffs, startTime]);
                serializedState = newSerializedState;
            }
            data = stateData;

            console.timeEnd('    diff');
            console.log(' sending:', data);
            c.ws.send(data);
        });
    }

    revision = revision + 1;
    state = newState;
}, 1000);
