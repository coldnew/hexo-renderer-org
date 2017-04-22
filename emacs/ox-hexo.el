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


;;;; user config

(defvar org-hexo-use-line-number nil
  "Set t to add line-number to all src-block.")

(defvar org-hexo-use-line-number-on-example-block nil
  "Set t to add line-number to all example-block.
If you want to make example-block has line-number, you also need to setup `org-hexo-use-line-number' to t.")


;;;; Backend

(org-export-define-derived-backend 'hexo-html 'html
  :translate-alist
  '( ;; Fix for multibyte language
    (paragraph . org-hexo-html-paragraph)
    ;; convert relative link to let pelican can recognize
    (link . org-hexo-html-link)
    ;; For line-number and highlight.js support
    (src-block . org-hexo-src-block)
    (example-block . org-hexo-example-block)
    ;; Remove unuse html in template
    (template . org-hexo-template)
    ))


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

(defun org-hexo-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
   (let ((div (assq 'content (plist-get info :html-divs))))
     (format "%s%s%s"
             (format "<%s id=\"%s\">\n" (nth 1 div) (nth 2 div))
             contents
             (format "</%s>\n" (nth 1 div))))
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)))


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
            (replace-regexp-in-string (regexp-quote raw-link)
                                      (file-name-nondirectory raw-path) html-link)))
    ;; Fix generate link
    (replace-regexp-in-string
     "<a href=\"\\(.*?\\)\"\s+class=\"\\(.*?\\)\"\\(.*?\\)" "<a href=\"\\1\" \\3"
     (replace-regexp-in-string
      "<img src=\"\\(.*?\\)\"\s+alt=\"\\(.*?\\)\"\\(.*?\\)" "<img src=\"\\1\" \\3" html-link))
    ))




;;; src block

(defun org-hexo-do-format-code
    (code &optional lang refs retain-labels num-start)
  "Format CODE string as source code.
Optional arguments LANG, REFS, RETAIN-LABELS and NUM-START are,
respectively, the language of the source code, as a string, an
alist between line numbers and references (as returned by
`org-export-unravel-code'), a boolean specifying if labels should
appear in the source code, and the number associated to the first
line of code."
  (let* ((code-lines (org-split-string code "\n"))
         (code-length (length code-lines))
         (num-fmt
          (and num-start
               (format "%%%ds "
                       (length (number-to-string (+ code-length num-start))))))
         (code (org-html-fontify-code code lang)))
    (org-export-format-code
     code
     (lambda (loc line-num ref)
       (setq loc
             (concat
              ;; Add line number, if needed.
              (when num-start
                (format "<span class=\"linenr\">%s</span>"
                        (format num-fmt line-num)))
              ;; Transcoded src line.
              loc
              ;; Add label, if needed.
              (when (and ref retain-labels) (format " (%s)" ref))))
       ;; Mark transcoded line as an anchor, if needed.
       (if (not ref) loc
           (format "<span id=\"coderef-%s\" class=\"coderef-off\">%s</span>"
                   ref loc)))
     num-start refs)))

(defun org-hexo-format-code (element info)
  "Format contents of ELEMENT as source code.
ELEMENT is either an example block or a src block.  INFO is
a plist used as a communication channel."
  (let* ((lang (org-element-property :language element))
         ;; Extract code and references.
         (code-info (org-export-unravel-code element))
         (code (car code-info))
         (refs (cdr code-info))
         ;; Does the src block contain labels?
         (retain-labels (org-element-property :retain-labels element))
         ;; Does it have line numbers?
         (num-start
          (or (org-export-get-loc element info)
              ;; If it doesn't, does we enable line number globally ?
              (and lang org-hexo-use-line-number 0)
              ;; example-block is disable line-number globally support by default
              ;; If user really want to enable this, they should setup `org-hexo-use-line-number-on-example-block' manually.
              (and (not lang)
                   (and org-hexo-use-line-number-on-example-block 0)))))
    (org-hexo-do-format-code code lang refs retain-labels num-start)))

(defun org-hexo-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HEXO.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
      (let ((lang (org-element-property :language src-block))
            (code (org-hexo-format-code src-block info))
            (label (let ((lbl (and (org-element-property :name src-block)
                                   (org-export-get-reference src-block info))))
                     (if lbl (format " id=\"%s\"" lbl) ""))))
        (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
            (format "<div class=\"org-src-container\">\n%s%s\n</div>"
                    ;; Build caption.
                    (let ((caption (org-export-get-caption src-block)))
                      (if (not caption) ""
                          (let ((listing-number
                                 (format
                                  "<span class=\"listing-number\">%s </span>"
                                  (format
                                   (org-html--translate "Listing %d:" info)
                                   (org-export-get-ordinal
                                    src-block info nil #'org-html--has-caption-p)))))
                            (format "<label class=\"org-src-name\">%s%s</label>"
                                    listing-number
                                    (org-trim (org-export-data caption info))))))
                    ;; Contents.
                    (format "<pre class=\"src src-%s\"%s>%s</pre>"
                            lang label code))))))


(defun org-hexo-example-block (example-block _contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((attributes (org-export-read-attribute :attr_html example-block)))
    (if (plist-get attributes :textarea)
  (org-html--textarea-block example-block)
      (format "<pre class=\"example\"%s>\n%s</pre>"
        (let* ((name (org-element-property :name example-block))
         (a (org-html--make-attribute-string
       (if (or (not name) (plist-member attributes :id))
           attributes
         (plist-put attributes :id name)))))
    (if (org-string-nw-p a) (concat " " a) ""))
        (org-hexo-format-code example-block info)))))


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
