'use strict';
let renderer = require('./lib/renderer');
let assign = require('object-assign');
let read_info = require('./lib/read-info');
let emacs = require('./lib/emacs');

let fs = require('fs');
let path = require('path');

// Init option
hexo.config.org = assign({
  emacs: 'emacs',
  emacsclient: 'emacsclient',   // user should not setup this if renderer work correctly
  common: '#+OPTIONS: toc:nil num:nil\n#+BIND: org-html-postamble nil',
  cachedir: './hexo-org-cache/',
  clean_cache: false,            // enable this to make 'hexo clean' also clean the cache
  theme: '',
  user_config: '',
  htmlize: false,
  line_number: false,
  daemonize: true,              // set true to use existing server
  debug: false,
  server_file: "~/.emacs.d/server/server"
}, hexo.config.org);

hexo.on('ready', function() {

  // detect if we are going to clear all cache file (the 'cachedir/emacs.d' will not remove )
  if(process.argv.indexOf('clean') > 0 ) {
    let dir = hexo.config.org.cachedir;
    if (fs.existsSync(dir) && hexo.config.org.clean_cache) {
      let files = fs.readdirSync(dir);
      files.forEach(function (filename) {
        let fullname = path.join(dir, filename);
        let stats = fs.statSync(fullname);
        if (!stats.isDirectory())
          fs.unlink(fullname, (err) => {
            if (err) throw err;
            console.log(fullname + " was deleted");
          });
      });
    }
  }

  emacs.server
    .check(hexo)
    .then(emacs.server.load_config)
    .catch(err => {
      console.error(err);
      process.exit(1);
    });
});

// When time to exit hexo, kill emacs process
hexo.on('exit', err => {
  if(err) console.error(err);
});

hexo.extend.filter.register('before_post_render', read_info);
hexo.extend.renderer.register('org', 'html', renderer.bind(hexo), false);
