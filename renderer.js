'use strict';

var org = require('org');
var cheerio = require('cheerio');
var util = require('hexo-util');

var spawnSync = require('child_process').spawnSync;

var highlight = util.highlight;

function renderer(data) {
  console.log(`Render ${data.path}...`);
  var config = this.config;
  config.highlight = config.highlight || {};

  var html = convert(data, config);

  var $ = cheerio.load(html, {
    ignoreWhitespace: false,
    xmlMode: false,
    lowerCaseTags: false
  });

  var yaml_foot = $("p:contains('—')")[0];

  // check the option form hexo `_config.yml` file
  if (!config.highlight.enable)
    return $.html();

  // not work for now
  $('pre').each(function() {
    var text; // await highlight code text
    var lang = 'unknown';
    if (!$(this).children().length)
      text = $(this).text();
    else {
      var code = $(this).find('code');
      text = code.text();
      lang = code.attr('class').replace('language-', '');
    }
    $(this).replaceWith(highlighted(text, lang, config));
  });

  return $.html();
}

function convert(data, config) {
  var emacs_path = config.org.emacs;
  var emacs_lisp =
      // org-html-export-as-html (&optional async subtreep visible-only body-only ext-plist)
      // http://orgmode.org/worg/doc.html
      // '+++' is a flag to split stderr and stdout
      `
(progn
  (defun string/starts-with (string prefix)
    "Return t if STRING starts with prefix."
    (and (string-match (rx-to-string \`(: bos ,prefix) t)
string)
         t))
  ;; Delete yaml header use elisp
  (goto-line 0)
  (while (not (string/starts-with (thing-at-point 'line) "---"))
    (kill-whole-line))
  (kill-whole-line)

  (org-html-export-as-html nil nil nil t nil)
  (message "+++%s" (buffer-string)))
  `;
  var exec_args = [data.path, '--batch', '--kill', '--execute', emacs_lisp];
  var proc = spawnSync(emacs_path, exec_args);
  // I dont know why it output to stderr..

  var err, out, whole;
  whole = proc.stderr.toString().split('+++');

  if(whole.length > 1){
    // has err or warning
    err = whole[0];
    out = whole[1];
    process.stderr.write(err);
  }else{
    out = whole[0];
  }

  return out;
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

