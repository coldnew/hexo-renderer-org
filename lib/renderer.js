'use strict';

var cheerio = require('cheerio');
var util = require('hexo-util');
var os = require('os');
var iconv = require('iconv-lite');
var jsonfile = require('jsonfile');
var md5 = require('md5');
var fs = require('fs');
var mkdirp = require('mkdirp');
var tmp = require('tmp');
var path = require('path');
var highlight = util.highlight;

var emacs = require('./emacs');

function render_html(html, config) {
  return new Promise((reslove, reject) => {
    config.highlight = config.highlight || {};
    var $ = cheerio.load(html, {
      ignoreWhitespace: false,
      xmlMode: false,
      lowerCaseTags: false,
      decodeEntities: false
    });

    // check the option form hexo `_config.yml` file
    if (!config.highlight.enable)
      reslove(html);

    $('pre.src').each(function() {
      var text; // await highlight code text
      var lang = 'unknown';
      var code = $(this);
      text = code.text().replace(/\n$/g,'');
      var class_str = code.attr('class');
      if (class_str.startsWith('src src-')) {
        lang = class_str.substring('src src-'.length);
      }
      if(lang == "emacs-lisp") lang = "lisp"; // render emacs-lisp as lisp
      $(this).replaceWith(highlighted(text, lang, config));
    });
    reslove($);
  });
}

function renderer(data) {
  var config = this.config;

  // wait for emacs server ready
  emacs.server.wait();

  return new Promise((resolve, reject) => {
    // check cache

    var cachefile = null;
    if(config.org.cachedir){
      mkdirp.sync(config.org.cachedir);
      cachefile = config.org.cachedir + md5(data.path);
    }
    var cache = null;
    var content_md5 = null;
    if(cachefile){
      if(!fs.existsSync(cachefile)){
        cache = {};
      }else{
        cache = jsonfile.readFileSync(cachefile);
      }
      var content = fs.readFileSync(data.path);

      content_md5 = md5(content
                        + JSON.stringify(config.org)
                        + JSON.stringify(config.highlight));
      if(cache.md5 == content_md5){ // hit cache
        console.log(`${data.path} completed with cache`);
        resolve(cache.content);
        return;
      }
    }
    convert(data, config)
      .then((html) => {
        return render_html(html, config);
      })
      .then((result) => {
        console.log(`${data.path} completed`);
        if(cache !== null){
          cache.md5 = content_md5;
          cache.content = result;
          jsonfile.writeFileSync(cachefile, cache);
        }
        resolve(result);
      });
  });
}

function convert(data, config) {
  return new Promise((resolve, reject) => {
    emacs.client(config, data, resolve);
  });
}

function highlighted(code, lang, config) {
  /**
   * hexo highlight function for a code block.
   * @param {String} code
   * @param {String} options https://github.com/hexojs/hexo-util#highlightstr-options
   * @returns {String} result
   */
  return highlight(code, {
    gutter: config.highlight.number,
    lang: lang
  });
}

module.exports = renderer;
