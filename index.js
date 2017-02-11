'use strict';
var renderer = require('./lib/renderer');
var assign = require('object-assign');
var read_info = require('./lib/read-info');

// Init option
hexo.config.org = assign({
  toc: false,
  emacs: 'emacs',
  common: '',
  export_cfg: "(progn (package-initialize)(require 'org) (require 'org-clock) (require 'ox))",
  cachedir: './hexo-org-cache/'
}, hexo.config.org);

hexo.extend.renderer.register('org', 'html', renderer.bind(hexo), false);

hexo.extend.filter.register('before_post_render', read_info);
