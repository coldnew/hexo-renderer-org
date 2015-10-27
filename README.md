# hexo-renderer-org
Hexo renderer plugin for emacs org-mode

# Usage

Cd to your hexo blog.Run

```shell
npm install hexo-renderer-org --save
```

Then restart your hexo server.

# Options

You can configure this plugin in `_config.yml`.

```yml
org:
  toc: false
```

- toc - Enable table of content

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
