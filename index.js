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
    var HTML = orgHTMLDocument.toString();
    HTML = HTML.replace(orgHTMLDocument.titleHTML,''); // just remove title,hexo will render title from yaml itself
    return HTML;
};

hexo.extend.renderer.register('org', 'html', render);
