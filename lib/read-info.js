'use strict';

var moment = require('moment');

function read_info(data) {
  var _items = {};
  read_in();
  read_all();
  return data;

  function split2(str, delim) {
    var parts = str.split(delim);
    return [parts[0], parts.splice(1, parts.length).join(delim)];
  }

  function read_in() {
    var r = data.content.match(/#\+[a-zA-Z]*:.*\n/g);
    if (r) {
      for (var i = 0; i < r.length; i++) {
        var parts = split2(r[i], ':');
        var key = parts[0].substring(2).trim();
        if(!_items[key])
          _items[key] = parts[1].trim();
      }
    }
  }

  function read_title() {
    if (_items.TITLE) {
      data.title = _items.TITLE;
    }
  }

  function convert_org_time(org_time) {
    return moment(Date.parse(org_time.replace(/[^0-9:-]/g, ' ')));
  }

  function read_date() {
    if (_items.DATE) {
      data.date = convert_org_time(_items.DATE);
    }
  }

  function read_tags(){
    if(_items.TAGS){
      data.setTags(_items.TAGS.replace(/\s+/g,'').split(','));
    }
  }

  function read_categories(){
    if(_items.CATEGORIES){
      data.setCategories(_items.CATEGORIES.replace(/\s+/g,'').split(','));
    }
  }

  function read_layout(){
    if(_items.LAYOUT){
      data.layout = _items.LAYOUT;
    }
  }

  function read_comments(){
    if(_items.COMMENTS == "no"){
      data.comments = false;
    }
  }

  function read_all() {
    if (!/.*\.org/.test(data.source)) {
      // skip if is not a org file
      return data;
    }
    read_in();
    read_title();
    read_date();
    read_tags();
    read_categories();
    read_layout();
    read_comments();
    return data;
  }
}

module.exports = read_info;
