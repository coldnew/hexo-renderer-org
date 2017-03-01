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
(package-install 'f)                        ; Installed by packages.el
(package-install 's)                        ; Installed by packages.el
(package-install 'org)                      ; Installed by packages.el
(package-install 'htmlize)                  ; Installed by packages.el

;;;; Code:
(require 'f)
(require 's)
(require 'htmlize)

;; org-mode config
(require 'org)

;; syntax highlight
(load-theme 'tango-dark t)

;; load ox-hexo.el
(load (f-join init-path "ox-hexo.el"))

;; TODO: load user's config

;; (princ (format "---> %s" argv))
;; (find-file (first argv))
;; (org-hexo-export-as-html)

;; (princ (format "-->%s" (buffer-string)) #'external-debugging-output)
;; (princ "Asdsadjasdjasdjaskljdasd")
 ;; (kill-emacs)

;; arguments
;; 0: cache path
;; 1: file
(setq org-src-fontify-natively t)

(defun hexo-render-org (file cache-dir output-file)
  "Render FILE from `org-mode' to html and save cache to CACHE-DIR."
  (let* ((user-emacs-directory (file-name-directory cache-dir)))
    ;; open the file
    (find-file file)
    ;; export the file
    (org-hexo-export-as-html)
    ;; trow result to tmp file
    (write-file output-file)
    ;; finish and exit
    ;; (kill-emacs)
    (save-buffers-kill-terminal)
    ))

(provide 'init)
;;; init.el ends here