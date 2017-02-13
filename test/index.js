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
        common: '#+OPTIONS: html-postamble:nil'
      }
    }
  };

  var r = require('../lib/renderer').bind(ctx);
    // org-html-postamble contains time and version info which is not easy to test
    it('rendering simple-test.org with no org-html-postamble', function(){
    var data = {
      path: `${process.cwd()}/test/org/simple-test.org`,
      text: 'test' // did nothing
    };

    return r(data).then((result) => {
      console.log(result);
      result.should.eql(
        `

<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Orgmode</a>
<ul>
<li><a href="#sec-1-1">1.1. Why orgmode</a></li>
<li><a href="#sec-1-2">1.2. xml code test</a></li>
</ul>
</li>
</ul>
</div>
</div>
<div id="outline-container-sec-1" class="outline-2">
<h2 id="sec-1"><span class="section-number-2">1</span> Orgmode</h2>
<div class="outline-text-2" id="text-1">
</div><div id="outline-container-sec-1-1" class="outline-3">
<h3 id="sec-1-1"><span class="section-number-3">1.1</span> Why orgmode</h3>
<div class="outline-text-3" id="text-1-1">
<div class="org-src-container">

<pre class="src src-js">consol.log("hello");
</pre>
</div>
<p>
我编不下去了。。
</p>
</div>
</div>

<div id="outline-container-sec-1-2" class="outline-3">
<h3 id="sec-1-2"><span class="section-number-3">1.2</span> xml code test</h3>
<div class="outline-text-3" id="text-1-2">
<div class="org-src-container">

<pre class="src src-xml"><Command>
  <Order>1</Order>
</Command>
</pre>
</div>
</div>
</div>
</div>
`); // I just copy right value here
    });
  });
});
