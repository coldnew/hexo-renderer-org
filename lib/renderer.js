'use strict';

var cheerio = require('cheerio');
var util = require('hexo-util');
var os = require('os');
var iconv = require('iconv-lite');
var jsonfile = require('jsonfile');
var md5 = require('md5');
var fs = require('fs-extra');
var tmp = require('tmp');
var path = require('path');
var highlight = util.highlight;

var emacs = require('./emacs');

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

        // for use 'htmlize' to syntax highlight code block
        if (config.org.htmlize)
            reslove(html);

        // for use 'highlight.js' to syntax highlight code block (default)
        config.highlight = config.highlight || {};
        var $ = cheerio.load(html, {
            ignoreWhitespace: false,
            xmlMode: false,
            lowerCaseTags: false,
            decodeEntities: true
        });


        $('pre.src').each(function() {
            var text; // await highlight code text
            var lang = 'unknown';
            var code = $(this);
            var class_str = code.attr('class');
            if (class_str.startsWith('src src-')) {
                lang = class_str.substring('src src-'.length);
            }
            // In ox-hexml.el, I return the line-number with code text, we need to remove first-line to
            // get line-number info

            // remove first line
            var lines = code.text().split('\n');

            var firstLine = parseInt(lines[0]) + 1;

            var gutter;// = config.org.line_number;
            if (config.org.line_number)
                gutter = true;
            else {
                gutter = (firstLine == 0) ? false : true;
            }

            // remove first line
            lines.splice(0,1);
            // remove newline
            text = lines.join('\n').replace(/\n$/g,'');

            // USE hexo.utils to render highlight.js code block
            $(this).replaceWith( highlight(text, {
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

function renderer(data) {
    return emacs.server.wait(this).then(()=> {
        return new Promise((resolve, reject) => {
            var config = this.config;
            // wait for emacs server ready
            this.log.info("render data.path", data.path);
            // check cache
            var cachefile = null;
            if(config.org.cachedir){
                fs.mkdirsSync(config.org.cachedir);
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

    });

}

function convert(data, config) {
    return new Promise((resolve, reject) => {
        if (config.org.daemonize) {
            emacs.client(config, data, resolve);
        }
        else {
            emacs.process(config, data, resolve);
        }
    });
}

module.exports = renderer;
