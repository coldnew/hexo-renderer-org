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

  function read_updated() {
    if (_items.UPDATED) {
      data.updated = convert_org_time(_items.UPDATED);
    }
  }

  // #+TAGS: can split with comma or space
  function read_tags(){
    if(_items.TAGS){
      data.setTags(_items.TAGS.split(',').map((item) => item.trim()));
    }
  }

  function read_categories(){
    if(_items.CATEGORIES){
      data.setCategories(_items.CATEGORIES.split(',').map((item) => item.trim()));
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

  function read_permalink() {
    if(_items.PERMALINK){
      data.slug = _items.PERMALINK;
    }
  }

  function read_alias() {
    // support array, re-read again
    if(_items.ALIAS){
      var r = data.content.match(/#\+ALIAS:.*\n/g);
      if (r) {
        data.alias = [];
        for (var i = 0; i < r.length; i++) {
          var parts = split2(r[i], ':');
          var key = parts[0].substring(2).trim();
          data.alias.push( parts[1].trim());
        }
      }
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
    read_updated();
    read_tags();
    read_categories();
    read_layout();
    read_comments();
    read_permalink();
    read_alias();
    return data;
  }
}

module.exports = read_info;
