"use strict";
const org = require("org");
const render = function(data,locals){
    const parser = new org.Parser();
    const orgDocument = parser.parse(data.text);
    const orgHTMLDocument = orgDocument.convert(org.ConverterHTML, {
        headerOffset: 1,
        exportFromLineNumber: false,
        suppressSubScriptHandling: false,
        suppressAutoLink: false
    });
    let HTML = orgHTMLDocument.toString();
    HTML = HTML.replace(orgHTMLDocument.titleHTML,''); // just remove title,hexo will render title from yaml itself
    return HTML;
};

hexo.extend.renderer.register('org', 'html', render);
