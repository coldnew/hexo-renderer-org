'use strict';

var cheerio = require('cheerio');
var util = require('hexo-util');
var os = require('os');
var spawn = require('child_process').spawn;
var iconv = require('iconv-lite');
var BufferHelper = require('bufferhelper');
var jsonfile = require('jsonfile');
var md5 = require('md5');
var fs = require('fs');
var mkdirp = require('mkdirp');

var highlight = util.highlight;

function get_content(elem){
  elem('h1.title').remove();
  var r = "";
  var to_export = ['div#preamble', 'div#content', 'div#postamble'];
  for(var i=0;i<to_export.length;i++){
    var item = elem(to_export[i]);
    // http://stackoverflow.com/questions/31044/is-there-an-exists-function-for-jquery
    if(item.length){
      r += item.html();
    }
  }
  return r;
}

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
      reslove(get_content($));


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
    reslove(get_content($));
  });
}

function htmlUnescape(str){
    return str
        .replace(/&quot;/g, '"')
        .replace(/&#39;/g, "'")
        .replace(/&lt;/g, '<')
        .replace(/&gt;/g, '>')
        .replace(/&amp;/g, '&');
}

function renderer(data) {
  var config = this.config;
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
        resolve(htmlUnescape(result));
    });
  });
}


function print_warning(err, path) {
  if (!err) return;
  var useless = [
    'Mark set',
    "Warning: arch-dependent data dir '/Users/build/workspace/Emacs-Multi-Build/label/mavericks/emacs-source/nextstep/Emacs.app/Contents/MacOS/libexec/': No such file or directory",
    'Cannot fontify src block (htmlize.el >= 1.34 required)',
    'Cannot fontify src block (htmlize.el >= 1.34 required)\r', // for Windows
      '',
      'Extension ignored',
      'Indentation variables are now local.',
      'Setting up indent for shell type bash',
      'Indentation setup for shell type bash'
  ];
  var lines = err.split('\n');
  var msg = '';
  for (var i = 0; i < lines.length; i++) {
      if (useless.indexOf(lines[i].trim()) < 0) {
      msg = msg + lines[i] + '\n';
    }
  }

  if (msg != '')
    process.stderr.write(`XXX==Error Here==>${path}:\n${msg}`);
}

function parse_output(data, flag) {
  var out, whole;
  whole = data.split(flag);
  if (data.endsWith(flag + '\n') || data.endsWith(flag + '\r\n')) {
    // has output
    out = whole[1];
  } else {
    // no output
    out = null;
  }
  return {
    out: out,
    err: whole[0]
  };
}


function convert(data, config) {

  return new Promise((resolve, reject) => {
    var emacs_path = config.org.emacs;
    var flag = '+=+=output_begin=+=+';
    var emacs_lisp =
      // org-html-export-as-html (&optional async subtreep visible-only body-only ext-plist)
      // http://orgmode.org/worg/doc.html
      // '+=+=output_begin=+=+' is a flag to split stderr and stdout
      `
(progn
  (setq org-export-allow-bind-keywords t)
  (goto-line 0)
  (insert "${config.org.common}\n")
  ;; reload org setting https://lists.gnu.org/archive/html/emacs-orgmode/2014-01/msg00648.html
  (normal-mode)
  (unless (derived-mode-p 'org-mode)
    (org-mode))
  (org-html-export-as-html nil nil nil nil nil)
  (message "${flag}%s${flag}" (buffer-string)))
  `;
    var exec_args = ['--batch', data.path, '--execute',emacs_lisp];
    if (config.org.export_cfg != '')
        exec_args.splice(1,0,'--execute', config.org.export_cfg)
    var proc = spawn(emacs_path, exec_args);
    // I dont know why it output to stderr..
    var bufferHelper = new BufferHelper();

    proc.stderr.on('data', function(data) {
      bufferHelper.concat(data);
    });

    proc.on('close', function(code) {
      var output;
      if(os.platform() == 'win32'){
        output = iconv.decode(bufferHelper.toBuffer(),'gbk');
      }else{
        output = iconv.decode(bufferHelper.toBuffer(),'utf8');
      }

      var result = parse_output(output, flag);
      print_warning(result.err, data.path);
      resolve(result.out);
    });
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
