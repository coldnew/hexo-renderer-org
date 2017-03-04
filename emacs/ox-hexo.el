;;; ox-hexo.el --- Core functions for ox-hexo.el.

;; Copyright (c) 2015 - 2017 Yen-Chin, Lee. (coldnew) <coldnew.tw@gmail.com>
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords:
;; X-URL: http://github.com/coldnew/org-hexo
;; Version: 0.1
;; Package-Requires: ((org "8.0") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;;; Code:
(eval-when-compile (require 'cl-lib))

(require 'ox-html)
(require 'ox-publish)


;;;; metadata

(defvar org-hexo--options-alist
  '(;; buildin in org-mode
    (:date      "DATE"       nil     nil)
    (:tags      "TAGS"       nil     nil)
    (:category  "CATEGORY"   nil     nil)
    ;; Need by hexo
    (:layout    "LAYOUT"     nil     nil)
    (:updated   "UPDATED"    nil     nil)
    (:comments  "COMMENTS"   nil     nil)
    (:permalink "PERMALINK"  nil     nil)))


;;;; Paragraph

(defun org-hexo-html-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  ;; Fix multibyte language like chinese will be automatically add
  ;; some space since org-mode will transpose auto-fill-mode's space
  ;; to newline char.
  (let* ((fix-regexp "[[:multibyte:]]")
         (fix-contents
          (replace-regexp-in-string
           (concat "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" contents)))

    ;; Send modify data to func
    (org-html-paragraph paragraph contents info)))


;;; Template

(defun org-hexo-html-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info)))

(defun org-hexo-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; start html header
   (org-html-doctype info)
   "\n"
   "<head>\n"
   "</head>\n"
   "<body>\n"

   ;; Document contents.
   (format "<%s id=\"%s\">\n"
           (nth 1 (assq 'content org-html-divs))
           (nth 2 (assq 'content org-html-divs)))

   contents
   (format "</%s>\n"
           (nth 1 (assq 'content org-html-divs)))

   ;; Closing document.
   "</body>\n</html>"))


;;;; Link

(defun org-hexo-html-link (link contents info)
  "Transcode LINE-BREAK object into Markdown format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type link))
         (raw-link (org-element-property :path link))
         (raw-path (expand-file-name raw-link))
         (html-link (org-html-link link contents info)))

    ;; file
    (when (string= type "file")

      ;; Fix file link if prefix with org-mode file name.
      ;; ex: if we has `hi.org' and asset dir `hi'
      ;; in hi.org: [[file:hi/xxx.png]] will be read as [[file:xxx.png]], this will help hexo
      ;; not have problem when render image path.
      (setq html-link
            (replace-regexp-in-string raw-link
                                      (file-name-nondirectory raw-path) html-link)))

  ;; Fix generate link
  (replace-regexp-in-string
   "<a href=\"\\(.*?\\)\"\s+class=\"\\(.*?\\)\"\\(.*?\\)" "<a href=\"\\1\" \\3"
   (replace-regexp-in-string
    "<img src=\"\\(.*?\\)\"\s+alt=\"\\(.*?\\)\"\\(.*?\\)" "<img src=\"\\1\" \\3" html-link))
  ))


;;; End-user functions

;;;###autoload
(defun org-hexo-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer for org-hexo.

Export is done in a buffer named \"*Hexo HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'hexo-html "*Hexo HTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (html-mode))))

;;;###autoload
(defun org-hexo-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." (or (plist-get ext-plist :html-extension)
                                    org-html-extension
                                    "html")))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'hexo-html file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-hexo-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'hexo-html filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension "html"))
                      plist pub-dir))

(provide 'ox-hexo)
;;; ox-hexo.el ends here.
