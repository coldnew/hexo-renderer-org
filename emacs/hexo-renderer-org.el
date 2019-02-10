;;; hexo-renderer-org.el --- hexo-renderer-org's emacs init file.

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

;;; Code:


;;;; Initial emacs server named `hexo-renderer-org'

;; when error, trigger an error buffer to make debug more easy
(setq debug-on-error t)

;; Ignore all directory-local variables in `.dir-locals.el', whick make Emacs stucks there.
(setq enable-dir-local-variables nil)

;; Disable "VC" (emacs internal version control stuff)
(setq vc-handled-backends nil)

(unless (boundp 'hexo-renderer-org-debug)
  (defvar hexo-renderer-org-debug nil))

(unless (and (boundp 'hexo-renderer-org--loaded) (not hexo-renderer-org-debug))

  ;;;; Public variables

  (defvar hexo-renderer-org-cachedir "./hexo-org-cache"
    "Cache directory to save generated result and Emacs packages to increase startup speed.")

  (defvar hexo-renderer-org-user-config ""
    "User's personal init.el to install extra packages or customize their own `org-mode' exporter.")

  (defvar hexo-renderer-org-theme ""
    "User's theme to use, it's recommand to use Emacs's builtin theme here.")

  (defvar hexo-renderer-org-common-block "#+OPTIONS: html-postamble:nil num:nil toc:nil ^:nil"
    "Common `org-mode' settings in string, like #+OPTIONS: html-postamble:nil num:nil toc:nil ^:nil .")

  (defvar hexo-renderer-org-htmlize "false"
    "Enable use Emacs's htmlize package to renderer code block or not.")


  ;;;; Private variables

  ;; for backward compability
  (defvar hexo-renderer-org-daemonize t
    "For backward compiblity, set `nil' to disable fetch org-mode and other files.")

  (defvar hexo-renderer-org--load-path
    (file-name-directory (or load-file-name (buffer-file-name)))
    "This hexo-renderer-org.el file path.")

  (defvar hexo-renderer-org--debug-file "./hexo-org-renderer.log"
    "YOU SHOULD NOT SETUP THIS VARIABLE.")

  (defvar hexo-renderer-org--use-htmlize nil
    "YOU SHOULD NOT SETUP THIS VARIABLE.
  This variable is keeped incase org-hexo not loaded.")


  ;;;; Debugger

  (defun hexo-org-renderer-oops (msg)
    "OOPS: something error, let's show the MSG and kill EMACS :(."
    (require 'json)                       ; built-in
    (let ((oops '(:success :json-false)))
      (plist-put oops :message msg)
      ;; Convert to JSON format and write to `*deebug-file*'
      (with-temp-buffer
        (insert (json-encode oops))
        (write-region (point-min) (point-max) hexo-renderer-org--debug-file))
      ;; bye-bye emacs
      (kill-emacs)))

  (defun hexo-org-renderer-debug-toggle()
    "Toggle debug to relaod hexo-renderer-org.el files."
    (interactive)
    (if hexo-renderer-org-debug
        (setq hexo-renderer-org-debug nil)
      (setq hexo-renderer-org-debug t)))

  ;; ;; The emacs daemon SHOULD die when error occurs.
  ;; (run-with-idle-timer
  ;; 1 t (lambda ()
  ;;       ;; When *Backtrace* exist, which means error occured, set `*statue*' to false and write value to `debug-file' then exit.
  ;;       (when (get-buffer "*Backtrace*")
  ;;         (with-current-buffer "*Backtrace*"
  ;;           (hexo-org-renderer-oops (buffer-string))))
  ;;       ;; Sometimes, there's another error "End of file during parsing:", this error may not trow Error to emacs but just display on *Messages* buffer.
  ;;       (with-current-buffer "*Messages*"
  ;;         (goto-char (point-min))
  ;;         (while (re-search-forward "End of file during parsing" nil t)
  ;;           (hexo-org-renderer-oops (buffer-string))))
  ;;       ))


  ;;;; Initial emacs packages

  (when hexo-renderer-org-daemonize

    ;; user-emacs-directory is under the `hexo-renderer-org-cachedir'
    (setq user-emacs-directory (concat (file-name-as-directory hexo-renderer-org-cachedir) "emacs.d"))

    ;; Initial package.el
    (require 'package)                      ; built-in since emacs24

    ;; Extra package repos
    (add-to-list 'package-archives
                 '("melpa" . "https://melpa.org/packages/") t)
    (add-to-list 'package-archives
                 '("org" . "http://orgmode.org/elpa/") t)

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
    (package-install 'org-plus-contrib)     ; Installed by packages.el

    ;; Only install htmlize when user use it
    (when hexo-renderer-org--use-htmlize
      (progn
        (package-install 'htmlize)        ; Installed by packages.el
        (require 'htmlize)))
    )


  ;;;; Initial org-mode and ox-hexo.el

  (require 'org)
  (require 'ox-html)

  ;; Make sure we really use org-mode 9.x
  ;; (when (version< org-version "9.0.0")
  ;;   (hexo-org-renderer-oops
  ;;    (format
  ;;     "
  ;; \e[1m\e[31mERROR:\e[0m

  ;;   hexo-renderer-org ONLY work on org-mode 9.x.

  ;;   Please remove your %s and let hexo-renderer-org re-download org-mode package again.

  ;;   Package info:

  ;;     emacs     : %s
  ;;     org-mode  : %s

  ;; "
  ;;     hexo-renderer-org-cachedir
  ;;     emacs-version
  ;;     org-version)))

  ;; load ox-hexo.el when `org-hexo-export-as-html' is called
  (add-to-list 'load-path hexo-renderer-org--load-path)
  (autoload 'org-hexo-export-as-html "ox-hexo")


  ;; The exporter function

  (defun hexo-renderer-org-exporter ()
    "The exporter function.
  When execute this function, we must in the `org-mode' file.
  This function is intend to let user overwrite in their user-config."
    (org-hexo-export-as-html))

  (defun hexo-renderer-org-insert-options (s)
    "Insert common option and settings S to current `org-mode' document.
  The string S will be prepent at beginning of file.

  Here's how you can use this function:

    (hexo-renderer-org-insert-options \"#+OPTIONS: num:t\")

  "
    (save-excursion
      (goto-char (point-min))
      (newline-and-indent)
      (insert s)
      (newline-and-indent)))

  (defun hexo-renderer-org (args)
    "ARGS is a plist which contain following properities:

  ARGS:
  (
  :file         \"File path to render\"
  :output-file  \"Output file which redner by org-hexo\"
  )"
    (let ((file         (or (plist-get args :file)             ""))
          (output-file  (or (plist-get args :output-file)      "")))
      ;; Export file content by ox-hexo.el
      (with-temp-buffer
        ;; Insert input-file contents
        (insert-file-contents file)
        ;; Insert common options
        (hexo-renderer-org-insert-options hexo-renderer-org-common-block)
        ;; Export the org-mode file to HTML (default)
        (hexo-renderer-org-exporter)
        ;; Write contents to output-file
        (write-region (point-min) (point-max) output-file)
        ;; bye-bye tmp buffer
        (kill-buffer))
      (delete-window)))


  ;; User config and other stuffs

  ;; Load user-config
  (when (and (not (string-equal hexo-renderer-org-user-config ""))
             (file-exists-p hexo-renderer-org-user-config))
    (load-file hexo-renderer-org-user-config)
    (message "Load User Config."))

  ;; Load theme if specify
  (unless (string-equal hexo-renderer-org-theme "")
    (load-theme (intern hexo-renderer-org-theme) t))

  ;; Allow use #+BIND: in org-mode
  (setq org-export-allow-bind-keywords t)

  ;; Emacs is Ready!!!
  (message "Emacs Hexo Renderer Org is READY!!!!!")
  (setq hexo-renderer-org--loaded t)

  (provide 'hexo-renderer-org))


;;; hexo-renderer-org.el ends here
