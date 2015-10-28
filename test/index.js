'use strict';

var should = require('chai').should();

describe('Org renderer', function(){

  // in index.js
  var assign = require('object-assign');
  var ctx = {
    config: {
      org: {}
    }
  };

  var r = require('../lib/renderer').bind(ctx);
  var body = [
    '* Orgmode',
    '** Why orgmode',
    '#+BEGIN_SRC js',
    '  consol.log("hello");',
    '#+END_SRC',
    '我编不下去了。。'
  ].join('\n');


  it('rendering with default config', function(){
    var result = r({text: body});
    result.should.eql(
      '<h2 id="header-1">Orgmode</h2>\n<h3 id="header-1-1">Why orgmode</h3>\n<pre class="prettyprint"><code class="language-js">  consol.log(&quot;hello&quot;);</code>\n</pre>\n&#x6211;&#x7F16;&#x4E0D;&#x4E0B;&#x53BB;&#x4E86;&#x3002;&#x3002;'
    );  // I just copy right value here
  });

  it('rendering with org TOC is true', function () {
    ctx.config.org.toc = true;

    var result = r({text: body});
    result.should.eql('<ul><li><a href="#header-1">Orgmode</a><ul><li><a href="#header-1-1">Why orgmode</a></li>\n</ul>\n</li>\n</ul>\n\n<h2 id="header-1">Orgmode</h2>\n<h3 id="header-1-1">Why orgmode</h3>\n<pre class="prettyprint"><code class="language-js">  consol.log(&quot;hello&quot;);</code>\n</pre>\n&#x6211;&#x7F16;&#x4E0D;&#x4E0B;&#x53BB;&#x4E86;&#x3002;&#x3002;');  // I just copy right value here
  });
});
