;;; init.el --- hexo-render-org's emacs init file.

;; Copyright (c) 2017 Yen-Chin, Lee.
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; This file is desinged to used by `hexo-renderer-org'.

;;; Bootstrap

(defvar init-path
  (file-name-directory (or load-file-name (buffer-file-name)))
  "This init.el file path.")

(require 'package)                      ; built-in since emacs24

;; Extra package repos
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; For important compatibility libraries like cl-lib
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

;; This must come before configurations of installed packages.
;; Don't delete this line. If you don't want it, just comment it out by adding a
;; semicolon to the start of the line. You may delete these explanatory
;; comments.
(package-initialize)

;; Auto refresh packages info when no archive available.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install deps packages
(package-install 'org)                      ; Installed by packages.el
(package-install 'htmlize)                  ; Installed by packages.el

;;;; Code:
(require 'org)

;; load ox-hexo.el
(load (expand-file-name "ox-hexo.el" init-path))

;; the exporter function
(defun hexo-render-org (args)
  "ARGS is a plist which contain following properities:

ARGS:
 (
 :file         \"File path to render\"
 :cache-dir    \"Directory path to store the cache files\"
 :output-file  \"Output file which redner by org-hexo\"
 :htmlize      \"enable htmlize or not\"
 :theme        \"emacs-theme you want to use\"
 :user-config  \"personal's emacs config file to load by emacs\"
 )"
  (let ((file         (or (plist-get args :file)             ""))
        (cache-dir    (or (plist-get args :cache-dir)        ""))
        (output-file  (or (plist-get args :output-file)      ""))
        (htmlize      (or (plist-get args :htmlize)      "true"))
        (theme        (or (plist-get args :theme)            ""))
        (user-config  (or (plist-get args :user-config)      "")))

    ;; (when (string-equal htmlize "true")
    ;;   (require 'htmlize)
    ;;   (setq org-src-fontify-natively t))

    ;; load theme if specify
    (unless (string-equal theme "")
      (load-theme (intern theme) t))

    ;; load user-config
    ;; (unless (string-equal user-config "")
    ;;   (load user-config))

    ;; export file content by ox-hexo.el
    (with-temp-buffer
      (insert-file-contents file)
      (org-hexo-export-as-html)
      (write-region (point-min) (point-max) output-file)
      (kill-buffer))

    ;; done and done, exit emacs now
    (kill-emacs)))


(provide 'init)
;;; init.el ends here