'use strict';
var renderer = require('./lib/renderer');
var assign = require('object-assign');

hexo.config.org =  assign({
  toc:false
}, hexo.config.org);

hexo.extend.renderer.register('org', 'html', renderer(hexo.config));
