"use strict";
var org = require("org");
var render = function(data,locals){
    var parser = new org.Parser();
    var orgDocument = parser.parse(data.text);
    var orgHTMLDocument = orgDocument.convert(org.ConverterHTML, {
        headerOffset: 1,
        exportFromLineNumber: false,
        suppressSubScriptHandling: false,
        suppressAutoLink: false
    });
    var HTML = orgHTMLDocument.contentHTML;
    HTML = HTML.replace(/<span class="section-number">[1-9.]*<\/span>/g,'');
    return HTML;
};

hexo.extend.renderer.register('org', 'html', render);

