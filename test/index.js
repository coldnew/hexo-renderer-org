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
        common: ''
      }
    }
  };

  var r = require('../lib/renderer').bind(ctx);

  it('rendering simple-test.org with default config', function(){
    var data = {
      path: `${process.cwd()}/test/org/simple-test.org`,
      text: 'test' // did nothing
    };

    return r(data).then((result) => {
      result.should.eql(
        `<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Orgmode</a>
<ul>
<li><a href="#sec-1-1">1.1. Why orgmode</a></li>
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

<pre class="src src-js">consol.log(&quot;hello&quot;);
</pre>
</div>
<p>
&#x6211;&#x7F16;&#x4E0D;&#x4E0B;&#x53BB;&#x4E86;&#x3002;&#x3002;
</p>
</div>
</div>
</div>
`); // I just copy right value here
    });
  });
});
