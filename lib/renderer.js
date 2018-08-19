'use strict';

let cheerio = require('cheerio');
let util = require('hexo-util');
let os = require('os');
let iconv = require('iconv-lite');
let jsonfile = require('jsonfile');
let md5 = require('md5');
let fs = require('fs-extra');
let tmp = require('tmp');
let path = require('path');
let highlight = util.highlight;

let emacs = require('./emacs');

function get_content(elem) {
  elem('h1.title').remove();
  let r = "";
  let to_export = ['div#preamble', 'div#content', 'div#postamble'];
  for (let i = 0; i < to_export.length; i++) {
    let item = elem(to_export[i]);
    // http://stackoverflow.com/questions/31044/is-there-an-exists-function-for-jquery
    if (item.length) {
      r += item.html();
    }
  }
  return r;
}

function render_html(html, config) {
  return new Promise((reslove, reject) => {

    // for use 'htmlize' to syntax highlight code block
    if (config.org.htmlize)
      reslove(html);

    // for use 'highlight.js' to syntax highlight code block (default)
    config.highlight = config.highlight || {};
    let $ = cheerio.load(html, {
      ignoreWhitespace: false,
      xmlMode: false,
      lowerCaseTags: false,
      decodeEntities: true
    });


    $('pre.src').each(function() {
      let text; // await highlight code text
      let lang = 'unknown';
      let code = $(this);
      let class_str = code.attr('class');
      if (class_str.startsWith('src src-')) {
        lang = class_str.substring('src src-'.length);
      }
      // In ox-hexml.el, I return the line-number with code text, we need to remove first-line to
      // get line-number info

      // remove first line
      let lines = code.text().split('\n');

      let firstLine = parseInt(lines[0]) + 1;

      let gutter; // = config.org.line_number;
      if (config.org.line_number)
        gutter = true;
      else {
        gutter = (firstLine == 0) ? false : true;
      }

      // remove first line
      lines.splice(0, 1);
      // remove newline
      text = lines.join('\n').replace(/\n$/g, '');

      // USE hexo.utils to render highlight.js code block
      $(this).replaceWith(highlight(text, {
        gutter: gutter,
        autoDetect: config.highlight.auto_detect,
        firstLine: firstLine,
        lang: lang
      }));
    });

    //reslove(get_content($));
    reslove($.html());
  });
}

function cache_and_convert(data, hexo) {
  return new Promise((resolve, reject) => {
    let config = hexo.config;
    // wait for emacs server ready
    hexo.log.info("render data.path", data.path);
    // check cache
    let cachefile = null;
    if (config.org.cachedir) {
      fs.mkdirsSync(config.org.cachedir);
      cachefile = config.org.cachedir + md5(data.path);
    }
    let cache = null;
    let content_md5 = null;
    if (cachefile) {
      if (!fs.existsSync(cachefile)) {
        cache = {};
      } else {
        cache = jsonfile.readFileSync(cachefile);
      }
      let content = fs.readFileSync(data.path);

      content_md5 = md5(content +
        JSON.stringify(config.org) +
        JSON.stringify(config.highlight));
      if (cache.md5 == content_md5) { // hit cache
        console.log(`${data.path} completed with cache`);
        return resolve(cache.content);
      }
    }
    return convert(data, hexo)
      .then((html) => {
        if (config.org.debug) console.log(html);
        return render_html(html, config);
      })
      .then((result) => {
        console.log(`${data.path} completed`);
        if (cache !== null) {
          cache.md5 = content_md5;
          cache.content = result;
          jsonfile.writeFileSync(cachefile, cache);
        }
        resolve(result);
      });
  });
}

function renderer(data) {
  return cache_and_convert(data, this);
}

function convert(data, hexo) {
  let config = hexo.config;
  if (config.org.daemonize) {
    return emacs.client(hexo, data);
  } else {
    return emacs.process(hexo, data);
  }
}

module.exports = renderer;
