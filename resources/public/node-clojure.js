#!/usr/bin/env node
/*jslint node: true, stupid: true */

'use strict';

var java = require('java'),
    execSync = require('child_process').execSync;

var stdout = execSync('lein classpath');

stdout.toString().split(/:/).forEach(function (cpe) {
    java.classpath.push(cpe.trim());
});
console.log(java.classpath);

var Clojure = java.import('clojure.java.api.Clojure'),
    readString = Clojure.varSync('clojure.core/read-string'),
    evalClj = Clojure.varSync('clojure.core/eval');

module.exports.Clojure = Clojure;
module.exports.evalClj = function (s, f) {
    readString.invoke(s, function (form) {
        evalClj.invoke(form, f);
    });
};
module.exports.evalCljSync = function (s) {
    return evalClj.invokeSync(readString.invokeSync(s));
};
console.log(module.exports.evalCljSync('*clojure-version*').toString());
