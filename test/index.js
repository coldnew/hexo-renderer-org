'use strict';

var should = require('chai').should();

describe('Org renderer', function(){

  // in index.js
  var assign = require('object-assign');
  var hexo = {
    'config':{}
  };
  hexo.config.org =  assign({
    toc:false
  }, hexo.config.org);
  if(hexo.config.highlight == undefined) hexo.config.highlight = {}; // no `Cannot read property 'enable' of undefined`


  var r = require('../lib/renderer')(hexo.config);

  it('default', function(){
    var body = [
      '* Orgmode',
      '** Why orgmode',
      '#+BEGIN_SRC js',
      '  consol.log("hello");',
      '#+END_SRC',
      '我编不下去了。。'
    ].join('\n');

    var result = r({text: body});
    result.should.eql(
      '<h2 id="header-1">Orgmode</h2>\n<h3 id="header-1-1">Why orgmode</h3>\n<pre class="prettyprint"><code class="language-js">  consol.log(&quot;hello&quot;);</code>\n</pre>\n&#x6211;&#x7F16;&#x4E0D;&#x4E0B;&#x53BB;&#x4E86;&#x3002;&#x3002;'
    );  // I just copy right value here
  });
});
