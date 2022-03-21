'use strict';

const cheerio = require('cheerio');
const util = require('hexo-util');
const os = require('os');
const iconv = require('iconv-lite');
const jsonfile = require('jsonfile');
const md5 = require('md5');
const fs = require('fs-extra');
const tmp = require('tmp');
const path = require('path');
const highlight = util.highlight;

const emacs = require('./emacs');

function get_content(elem) {
  elem('h1.title').remove();
  let r = '';
  const to_export = ['div#preamble', 'div#content', 'div#postamble'];
  for (let i = 0; i < to_export.length; i++) {
    const item = elem(to_export[i]);
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
    if (config.org.htmlize) { reslove(html); }

    // for use 'highlight.js' to syntax highlight code block (default)
    config.highlight = config.highlight || {};
    const $ = cheerio.load(html, {
      ignoreWhitespace: false,
      xmlMode: false,
      lowerCaseTags: false,
      decodeEntities: true
    });


    $('pre.src').each(function() {
      let text; // await highlight code text
      let lang = 'unknown';
      const code = $(this);
      const class_str = code.attr('class');
      if (class_str.startsWith('src src-')) {
        lang = class_str.substring('src src-'.length);
      }
      // In ox-hexml.el, I return the line-number with code text, we need to remove first-line to
      // get line-number info

      // remove first line
      const lines = code.text().split('\n');

      const firstLine = parseInt(lines[0]) + 1;

      let gutter;// = config.org.line_number;
      if (config.org.line_number) { gutter = true; } else {
        gutter = firstLine != 0;
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

    // reslove(get_content($));
    reslove($.html());
  });
}

function renderer(data) {
  return emacs.server.wait(this).then(() => {
    return new Promise((resolve, reject) => {
      const config = this.config;
      // wait for emacs server ready
      this.log.info('render data.path', data.path);
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
        const content = fs.readFileSync(data.path);

        content_md5 = md5(content
                                  + JSON.stringify(config.org)
                                  + JSON.stringify(config.highlight));
        if (cache.md5 == content_md5) { // hit cache
          console.log(`${data.path} completed with cache`);
          resolve(cache.content);
          return;
        }
      }
      convert(data, this)
        .then(html => {
          return render_html(html, config);
        })
        .then(result => {
          console.log(`${data.path} completed`);
          if (cache !== null) {
            cache.md5 = content_md5;
            cache.content = result;
            jsonfile.writeFileSync(cachefile, cache);
          }
          resolve(result);
        });
    });

  });

}

function convert(data, hexo) {
  return new Promise((resolve, reject) => {
    const config = hexo.config;
    if (config.org.daemonize) {
      emacs.client(hexo, data, resolve);
    } else {
      emacs.process(hexo, data, resolve);
    }
  });
}

module.exports = renderer;
