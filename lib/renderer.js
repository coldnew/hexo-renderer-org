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
var tmp = require('tmp');
var pty = require('child_pty');
var path = require('path');

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
    // if (!config.highlight.enable)
    reslove(get_content($));

    // $('pre.src').each(function() {
    //   var text; // await highlight code text
    //   var lang = 'unknown';
    //   var code = $(this);
    //   text = code.text().replace(/\n$/g,'');
    //   var class_str = code.attr('class');
    //   if (class_str.startsWith('src src-')) {
    //     lang = class_str.substring('src src-'.length);
    //   }
    //   if(lang == "emacs-lisp") lang = "lisp"; // render emacs-lisp as lisp
    //   $(this).replaceWith(highlighted(text, lang, config));
    // });
    // reslove(get_content($));
  });
}

function htmlUnescape(str){
  // return str
  //   .replace(/&quot;/g, '"')
  //   .replace(/&#39;/g, "'")
  //   .replace(/&lt;/g, '<')
  //   .replace(/&gt;/g, '>')
  //   .replace(/&amp;/g, '&');
  return str;
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
        //return html;
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

// emacs -Q  -nw -l init.el --eval '(hexo-render-org "test/test.org" "test" "./a.html")'
function convert(data, config) {
  return new Promise((resolve, reject) => {
    var emacs_path = config.org.emacs;

    var init_el = path.join(process.env.PWD, "node_modules", "hexo-renderer-org", "emacs", "init.el" );
    var tmpobj = tmp.fileSync();

    var emacs_lisp = `(hexo-render-org "${data.path}" "${config.org.cachedir}" "${tmpobj.name}")`;
    var exec_args = ['-Q', '-nw', '-l', init_el, '--eval', emacs_lisp];

    if (config.org.export_cfg != '')
       exec_args.splice(1,0,'--execute', config.org.export_cfg);

    var proc = pty.spawn(emacs_path, exec_args, {
      name: 'xterm-color',
      cols: 100,
      rows: 30
    });

    proc.on('exit', function(code) {
      var output;
      //print_warning(result, data.path);
      var result = fs.readFileSync(tmpobj.name, 'utf8');
      resolve(result);
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
