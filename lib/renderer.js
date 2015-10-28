'use strict';

var org = require('org');
var cheerio = require('cheerio');
var util = require('hexo-util');

var highlight = util.highlight;

function renderer (data) {
  var config = this.config;
  config.highlight = config.highlight || {};

  var parser = new org.Parser();
  var orgDocument = parser.parse(data.text);
  var orgHTMLDocument = orgDocument.convert(org.ConverterHTML, {
    headerOffset: 1,
    exportFromLineNumber: false,
    suppressSubScriptHandling: false,
    suppressAutoLink: false
  });
  var tocHTML = config.org.toc ? orgHTMLDocument.tocHTML + '\n' : '';
  var contentHTML = orgHTMLDocument.contentHTML;
  var html = tocHTML + contentHTML;

  var $ = cheerio.load(html, {
    ignoreWhitespace: false,
    xmlMode: false,
    lowerCaseTags: false
  });

  // remove heading number of TOC
  $('.section-number').remove();

  // check the option form hexo `_config.yml` file
  if (!config.highlight.enable)
    return $.html();

  $('pre').each(function () {
    var text;   // await highlight code text
    var lang = 'unknown';
    if (!$(this).children().length)
      text = $(this).text();
    else {
      var code = $(this).find('code');
      text = code.text();
      lang = code.attr('class').replace('language-','');
    }
    $(this).prev().after(highlighted(text, lang, config));
    $(this).remove();
  });

  return $.html();
}

function highlighted (code, lang, config) {
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
