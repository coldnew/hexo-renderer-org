[![Build Status](https://travis-ci.org/CodeFalling/hexo-renderer-org.svg)](https://travis-ci.org/CodeFalling/hexo-renderer-org)
[![NPM version](https://badge.fury.io/js/hexo-renderer-org.svg)](http://badge.fury.io/js/hexo-renderer-org)
# hexo-renderer-org

Hexo renderer plugin for emacs org-mode

[中文介绍文章](http://codefalling.com/2015/11/10/new-version-of-hexo-renderer-org/)

# Usage

Cd to your hexo blog.Run

```shell
npm install https://github.com/CodeFalling/hexo-renderer-org#emacs --save
```

You can also install it from npm,too.

Then restart your hexo server.

# Options

You can configure this plugin in `_config.yml`.

```yml
org:
  emacs: '/Applications/Emacs.app/Contents/MacOS/Emacs'
```


# How to create new post

Create `balbala.org` in source/_posts/,there is a template:

```org
title: Title here
date: 2015-10-25 19:25:01
tags:
- emacs
- hexo
---
* Orgmode
** Why orgmode
#+BEGIN_SRC js
  consol.log("hello");
#+END_SRC

我编不下去了。。

```

# Q&A

## How to add `Read more` button?

Place

```org
#+BEGIN_HTML
<!--more-->
#+END_HTML
```

in where you would like to add a `Read more`

# Old pure-js version

See https://github.com/CodeFalling/hexo-renderer-org/tree/old-js-version for old pure-js version.
