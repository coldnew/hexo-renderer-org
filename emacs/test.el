;; create cache dir
(make-directory "cache" t)

;; file to store debugging information (format: json)
(setq *debug-file* "debug.log")

;; load init.el
(load-file "./init.el")

;; render file according to args
(hexo-render-org '(:file "simple-test.org" :cache-dir "cache" :output-file "simple-test.html" :htmlize "true" :theme "tango" :common-file "empty"))