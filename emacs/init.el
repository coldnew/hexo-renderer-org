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

;; when error, trigger an error buffer to make debug more easy
(setq debug-on-error t)

;; Ignore all directory-local variables in `.dir-locals.el', else it'll make Emacs stucks there.
(setq enable-dir-local-variables nil)

(defvar *debug-file* nil
  "A file to save debugging info. All data store in `json' format.")

(defvar *status* t
  "Renderer status, we assume this renderer will success, else this value will be set to `:json-false'.")

(defun log-info (msg)
  "Logging MSG in json format to *debug-file*.

Output-File: (will be convert to JSON-format)
 (
 :success       t          ;; Not t means failed :(
 :msg           \"wtf?\"   ;; some msg
 )"
  (let ((info '(:msg "")))
    ;; Build info list
    (plist-put info :success *status*)
    (plist-put info :msg msg)
    ;; Convert to JSON format and write to `*deebug-file*'
    (require 'json)                     ; build-in
    (with-temp-buffer
      (insert (json-encode info))
      (write-region (point-min) (point-max) *debug-file*))))

;; Start a timer to detect error
(run-with-idle-timer
 0 t (lambda ()
       ;; When *Backtrace* exist, which means error occured, set `*statue*' to false and write value to `debug-file' then exit.
       (with-current-buffer "*Backtrace*"
         (setq *status* :json-false)
         (log-info (buffer-string))
         (kill-emacs))))

;; Here let's know the init.el is succeful loaded
(log-info "Enter init.el")


;;;; Initial emacs

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
(log-info (format "Load ox-hexo.el at: %s " init-path))


;; Org-mode 9.0 hack

(defun org-mode-compability-fixup ()
  ;; org-mode 9.x has some compatable problem with 8.x, we need to fix it here :S
  (when (version<= "9.0.0" org-version)
    ;; ------------------------------------------------------------
    ;; http://orgmode.org/Changes.html#orgf3f9c91
    ;; Repair export blocks and INCLUDE keywords in current buffer.
    (let ((case-fold-search t)
          (back-end-re (regexp-opt
                        '("HTML" "ASCII" "LATEX" "ODT" "MARKDOWN" "MD" "ORG"
                          "MAN" "BEAMER" "TEXINFO" "GROFF" "KOMA-LETTER")
                        t)))
      (org-with-wide-buffer
       (goto-char (point-min))
       (let ((block-re (concat "^[ \t]*#\\+BEGIN_" back-end-re)))
         (save-excursion
           (while (re-search-forward block-re nil t)
             (let ((element (save-match-data (org-element-at-point))))
               (when (eq (org-element-type element) 'special-block)
                 (save-excursion
                   (goto-char (org-element-property :end element))
                   (save-match-data (search-backward "_"))
                   (forward-char)
                   (insert "EXPORT")
                   (delete-region (point) (line-end-position)))
                 (replace-match "EXPORT \\1" nil nil nil 1))))))
       (let ((include-re
              (format "^[ \t]*#\\+INCLUDE: .*?%s[ \t]*$" back-end-re)))
         (while (re-search-forward include-re nil t)
           (let ((element (save-match-data (org-element-at-point))))
             (when (and (eq (org-element-type element) 'keyword)
                        (string= (org-element-property :key element) "INCLUDE"))
               (replace-match "EXPORT \\1" nil nil nil 1)))))))
    ;; ------------------------------------------------------------
    ))


;; The exporter function

(defun hexo-render-org-exporter ()
  "The exporter function, when execute this function, we must in the org-mode file.
This function is intend to let user overwrite in their user-config."
  (org-hexo-export-as-html))

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
  (log-info "Enter hexo-render-org")
  (let ((file         (or (plist-get args :file)             ""))
        (cache-dir    (or (plist-get args :cache-dir)        ""))
        (output-file  (or (plist-get args :output-file)      ""))
        (htmlize      (or (plist-get args :htmlize)      "true"))
        (theme        (or (plist-get args :theme)            ""))
        (user-config  (or (plist-get args :user-config)      "")))

    ;; Use emacs's htmlize to syntax highlight source code
    (when (string-equal htmlize "true")
      (require 'htmlize)
      (setq org-src-fontify-natively t))

    ;; Load theme if specify
    (unless (string-equal theme "")
      (load-theme (intern theme) t))

    ;; Load user-config
    (when (and (not (string-equal user-config ""))
               (file-exists-p user-config))
      (load user-config))

    ;; Export file content by ox-hexo.el
    (with-temp-buffer
      ;; Insert input-file contents
      (insert-file-contents file)
      (log-info (format "Read input file: %s" file))
      ;; Fix for org-mode 8.x file under org-mode 9.x
      (org-mode-compability-fixup)
      ;; Export the org-mode file to HTML (default)
      (hexo-render-org-exporter)
      ;; Write contents to output-file
      (write-region (point-min) (point-max) output-file)
      (log-info (format "Write output file: %s" output-file))
      ;; bye-bye tmp buffer
      (kill-buffer))

    ;; done and done, exit emacs now
    (log-info "Exit hexo-render-org")
    (kill-emacs)))



(provide 'init)
;;; init.el ends here.