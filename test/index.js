'use strict';

let should = require('chai').should();

describe('Org renderer', function() {
  this.timeout(100000);
  // in index.js
  let assign = require('object-assign');
  let ctx = {
    config: {
      org: {
        emacs: 'emacs',
        export_cfg: '',
        common: '#+OPTIONS: html-postamble:nil',
        cachedir: './hexo-org-cache/',
        theme: 'tango',
        debug: true
      }
    }
  };

  let r = require('../lib/renderer').bind(ctx);
  // org-html-postamble contains time and version info which is not easy to test
  it('rendering simple-test.org with org-html-postamble', function(){
    let data = {
      path: `${process.cwd()}/test/org/simple-test.org`,
      text: 'test' // did nothing
    };

    // FIXME: this test can pass on org-mode 8.x, failed on 9.x :-S
    return r(data).then((result) => {
      console.log(result);
      result.should.eql(
        `
<p>
This is a hexo-render-org fork's test, which rewrite to make emacs can syntax highlight the render html.
</p>

<!--more-->

<div class="org-src-container">

<pre class="src src-emacs-lisp">(<span style="color: #346604;">require</span> '<span style="color: #204a87; font-weight: bold;">f</span>)
(<span style="color: #346604;">let*</span> ((spacemacs-dir
        (directory-file-name (f-join user-emacs-directory <span style="color: #5c3566;">"modules"</span> <span style="color: #5c3566;">"spacemacs"</span>)))
       (spacemacs-init
        (concat (file-name-as-directory spacemacs-dir) <span style="color: #5c3566;">"init.el"</span>))
       (user-emacs-directory (file-name-directory spacemacs-init)))
  <span style="color: #5f615c; font-style: italic;">;; </span><span style="color: #5f615c; font-style: italic;">Initial spacemacs, our emacs run on top of it</span>
  (load spacemacs-init))
</pre>
</div>
`); // I just copy right value here
    });
  });
});
