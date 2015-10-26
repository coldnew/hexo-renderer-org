'use strict';
var org = require('org');
var cheerio = require('cheerio');
var util = require('hexo-util');
var highlight = util.highlight;

function render (data, options) {
  var parser = new org.Parser();
  var orgDocument = parser.parse(data.text);
  var orgHTMLDocument = orgDocument.convert(org.ConverterHTML, {
    headerOffset: 1,
    exportFromLineNumber: false,
    suppressSubScriptHandling: false,
    suppressAutoLink: false
  });
  var html = orgHTMLDocument.toString();

  var $ = cheerio.load(html, {
    ignoreWhitespace: false,
    xmlMode: false,
    lowerCaseTags: false
  });

  // remove heading number of TOC
  $('.section-number').remove();

  // check the option form hexo `_config.yml` file
  if (!hexo.config.highlight.enable)
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

    $(this).prev().after(highlighted(text, lang));
    $(this).remove();
  });


  // TODO check option in theme config


  return $.html();
}

function highlighted (code, lang) {
  /**
   * hexo highlight function for a code block.
   * @param {String} code
   * @param {String} options https://github.com/hexojs/hexo-util#highlightstr-options
   * @returns {String} result
   */
  return highlight(code, {
    gutter: hexo.config.highlight.number,
    lang: lang
  });
}

hexo.extend.renderer.register('org', 'html', render);
