#!/usr/bin/env node
/*jslint node: true stupid: true nomen: true */

'use strict';

process.env.TERM = 'dumb';

var evalClj = require('./node-clojure').evalClj;

evalClj('(require \'deuce.main)');
evalClj('(deuce.emacs.lread/load \"deuce-loadup.el\")');

console.log(evalClj('(deuce.emacs/emacs-version)').invokeSync());
