'use strict';
var renderer = require('./lib/renderer');
var assign = require('object-assign');
var read_info = require('./lib/read-info');
var emacs = require('./lib/emacs');

var fs = require('fs');
var path = require('path');

// for detect if we use `hexo s'
var server_mode = false;
var emacs_server_start = false;

// Init option
hexo.config.org = assign({
  emacs: 'emacs',
  emacsclient: 'emacsclient',   // user should not setup this if renderer work correctly
  common: '#+OPTIONS: toc:nil num:nil\n#+BIND: org-html-postamble nil',
  export_cfg: "(progn (package-initialize)(require 'org) (require 'org-clock) (require 'ox))", // FIXME: why not remove this ?
  cachedir: './hexo-org-cache/',
  clean_cache: false,            // enable this to make 'hexo clean' also clean the cache
  theme: '',
  user_config: '',
  debug: false
}, hexo.config.org);

hexo.on('ready', function() {
  // detect if current is execute for server, we have different method to handle emacs server exit.
  server_mode = process.argv.indexOf('server') > 0 || process.argv.indexOf('s') > 0;

  // detect if we are going to clear all cache file (the 'cachedir/emacs.d' will not remove )
  if(process.argv.indexOf('clean') > 0 ) {
    var dir = hexo.config.org.cachedir;
    if (fs.existsSync(dir) && hexo.config.org.clean_cache) {
      var files = fs.readdirSync(dir);
      files.forEach(function (filename) {
        var fullname = path.join(dir, filename);
        var stats = fs.statSync(fullname);
        if (!stats.isDirectory())
          fs.unlink(fullname);
      });
    }
  }

  // start emacs server only on:
  //   hexo s
  //   hexo server
  //   hexo render
  //   hexo generator
  //   hexo g
  if (server_mode || process.argv.indexOf('render') > 0 || process.argv.indexOf('generate') > 0 || process.argv.indexOf('g') > 0) {
    // start emacs server
    emacs.server.start(hexo);
    emacs_server_start = true;
  }

});

// When time to exit hexo, kill emacs process
hexo.on('exit', function(err) {
  // If use `hexo server`, the hexo will first enter `.on(exit)` event then start the server.
  // that's why we skip emacs.server.stop() when first etner here with server mode.
  if (server_mode) {
    server_mode = false;
    return;
  }
  if (emacs_server_start)
    emacs.server.stop(hexo);
});

hexo.extend.renderer.register('org', 'html', renderer.bind(hexo), false);
hexo.extend.filter.register('before_post_render', read_info);
