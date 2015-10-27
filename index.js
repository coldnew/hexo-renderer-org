'use strict';
var renderer = require('./lib/renderer');
var assign = require('object-assign');

// Init option
// TODO: maybe we should move it to lib,because test use it,too
hexo.config.org =  assign({
  toc:false
}, hexo.config.org);

if(hexo.config.highlight == undefined) hexo.config.highlight = {}; // no `Cannot read property 'enable' of undefined`

hexo.extend.renderer.register('org', 'html', renderer(hexo.config));
