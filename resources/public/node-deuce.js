#!/usr/bin/env node
/*jslint node: true, stupid: true */
/*eslint-env node */
/*eslint quotes: [0, "double"] */

'use strict';

process.env.TERM = 'dumb';

var evalCljSync = require('./node-clojure').evalCljSync;

evalCljSync('(require \'deuce.main)');
evalCljSync('(deuce.emacs.lread/load \"deuce-loadup.el\")');

console.log(evalCljSync('(deuce.emacs/emacs-version)').invokeSync());
