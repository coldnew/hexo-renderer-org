'use strict';

var should = require('chai').should();

describe('Org renderer', function() {
  this.timeout(100000);
  // in index.js
  var assign = require('object-assign');
  var ctx = {
    config: {
      org: {
        emacs: 'emacs',
        export_cfg: '',
        //common: '#+OPTIONS: html-postamble:nil'
        common: '',
        debug: true
      }
    }
  };

  var r = require('../lib/renderer').bind(ctx);
    // org-html-postamble contains time and version info which is not easy to test
    it('rendering simple-test.org with org-html-postamble', function(){
    var data = {
      path: `${process.cwd()}/test/org/simple-test.org`,
      text: 'test' // did nothing
    };

    return r(data).then((result) => {
      console.log(result);
      result.should.eql(
        `---
title: hexo-render-org test
author: coldnew
date: 2015-11-08 13:42:25
lang: zh-tw
permalink: blog/2015/11-08_iphone_dock1
tags: [ emacs,hexo ]
---
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<head>
</head>
<body>
<div id="content">
<p>
This is a hexo-render-org fork's test, which rewrite to make emacs can syntax highlight the render html.
</p>

<!--more-->

<div class="org-src-container">
<pre class="src src-emacs-lisp">(<span style="color: #b4fa70;">require</span> '<span style="color: #e9b2e3;">f</span>)
(<span style="color: #b4fa70;">let*</span> ((spacemacs-dir
        (directory-file-name (f-join user-emacs-directory <span style="color: #e9b96e;">"modules"</span> <span style="color: #e9b96e;">"spacemacs"</span>)))
       (spacemacs-init
        (concat (file-name-as-directory spacemacs-dir) <span style="color: #e9b96e;">"init.el"</span>))
       (user-emacs-directory (file-name-directory spacemacs-init)))
  <span style="color: #73d216;">;; </span><span style="color: #73d216;">Initial spacemacs, our emacs run on top of it</span>
  (load spacemacs-init))
</pre>
</div>
</div>
</body>
</html>
`); // I just copy right value here
    });
  });
});
