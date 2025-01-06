;;; ox-mybackend.el --- Org Back-End for MyBackend engine -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Nikolaos Chatzikonstantinou.

;; Author: Nikolaos Chatzikonstantinou <nchatz314@gmail.com>
;; Maintainer: Nikolaos Chatzikonstantinou <nchatz314@gmail.com>
;; Keywords: org, ox, backend, example
;; Version: 0.1
;; URL: https://github.com/createyourpersonalaccount/ox-mybackend

;;; Commentary:

;; This package illustrates a simple backend for ox.  To try it out
;; use `package-install-from-buffer' and then (require 'ox-mybackend)
;; to activate it.  Finally, visit an org file and use
;; `org-export-dispatch' on it to see the new menu entry for export to
;; MyBackend.

;;; Code:

(require 'org-macs)                     ; top-level org definitions
(org-assert-version)                    ; sanity check

(require 'ox)                           ; to register this backend

;;; Define the customization group.
;; Customizable by the user with `customize-group'. The customizable
;; options are defined with `defcustom' below.
(defgroup org-export-mybackend nil
  "Options for exporting Org mode files to MyBackend."
  :tag "Org Export MyBackend"
  :group 'org-export
  :version "0.1"
  :package-version '(Org . "8.0"))

(defcustom org-mybackend-my-example-option nil
  "This is an example boolean option."
  :group 'org-export-mybackend
  :type 'boolean)

;;; Register the backend with Org Export.
(org-export-define-backend 'mybackend
  '(
    ;; This is top final function called when everything else has been
    ;; transcoded. This function must be specified by the backend author.
    (template . org-mybackend-template)
    ;; This handles the underlying contents of headlines.
    (section . org-mybackend-section)
    ;; This handles the headlines themselves.
    (headline . org-mybackend-headline)
    ;; The rest are either as-is or ignored.
    (babel-call . org-mybackend-identity)
    (bold . org-mybackend-identity)
    (center-block . org-mybackend-identity)
    (clock . org-mybackend-identity)
    (code . org-mybackend-identity)
    (diary-sexp . org-mybackend-identity)
    (drawer . org-mybackend-identity)
    (dynamic-block . org-mybackend-identity)
    (entity . org-mybackend-identity)
    (example-block . org-mybackend-identity)
    (export-block . ignore)
    (fixed-width . org-mybackend-identity)
    (footnote-definition . ignore)
    (footnote-reference . org-mybackend-identity)
    (horizontal-rule . org-mybackend-identity)
    (inline-babel-call . org-mybackend-identity)
    (inline-src-block . org-mybackend-identity)
    (inlinetask . org-mybackend-identity)
    (italic . org-mybackend-identity)
    (item . org-mybackend-identity)
    (keyword . ignore)
    (latex-environment . org-mybackend-identity)
    (latex-fragment . org-mybackend-identity)
    (line-break . org-mybackend-identity)
    (link . ignore)
    (node-property . org-mybackend-identity)
    (paragraph . org-mybackend-identity)
    (plain-list . org-mybackend-identity)
    (planning . org-mybackend-identity)
    (property-drawer . org-mybackend-identity)
    (quote-block . org-mybackend-identity)
    (radio-target . org-mybackend-identity)
    (special-block . org-mybackend-identity)
    (src-block . org-mybackend-identity)
    (statistics-cookie . org-mybackend-identity)
    (strike-through . org-mybackend-identity)
    (subscript . org-mybackend-identity)
    (superscript . org-mybackend-identity)
    (table . org-mybackend-identity)
    (table-cell . org-mybackend-identity)
    (table-row . org-mybackend-identity)
    (target . org-mybackend-identity)
    (timestamp . ignore)
    (underline . org-mybackend-identity)
    (verbatim . org-mybackend-identity)
    (verse-block . org-mybackend-identity))
  ;; This is the menu entry that is shown in `org-export-dispatch'.
  :menu-entry
  '(?X "Export to MyBackend"
       ((?b "in buffer" org-mybackend-export-as-mybackend))))

(defun org-mybackend-identity (blob contents info)
  "Transcode BLOB element or object back into Org syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
  (ignore info)
  (let ((case-fold-search t))
    (replace-regexp-in-string
     "^[ \t]*#\\+attr_[-_a-z0-9]+:\\(?: .*\\)?\n" ""
     (org-export-expand blob contents t))))

(defun org-mybackend-template (contents info)
  "Return Org document template with document keywords.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  (cl-flet (($ (s) (plist-get info s)))
    (concat
     ;; Time stamp on top (modtime).
     (and ($ :time-stamp-file)
	  (format-time-string "# Created %Y-%m-%d %a %H:%M\n"))
     ;; We preserve any #+OPTIONS: set.
     (org-element-normalize-string
      (mapconcat #'identity
	         (org-element-map ($ :parse-tree) 'keyword
		   (lambda (k)
		     (and (string-equal (org-element-property :key k) "OPTIONS")
			  (concat "#+options: "
				  (org-element-property :value k)))))
	         "\n"))
     ;; Various keywords such as #+TITLE, #+DATE, and so on, are
     ;; handled here.
     (and ($ :with-title)
	  (format "#+title: %s\n" (org-export-data ($ :title) info)))
     (and ($ :with-date)
	  (let ((date (org-export-data (org-export-get-date info) info)))
	    (and (org-string-nw-p date)
	         (format "#+date: %s\n" date))))
     (and ($ :with-author)
	  (let ((author (org-export-data ($ :author) info)))
	    (and (org-string-nw-p author)
	         (format "#+author: %s\n" author))))
     (and ($ :with-email)
	  (let ((email (org-export-data ($ :email) info)))
	    (and (org-string-nw-p email)
	         (format "#+email: %s\n" email))))
     (and ($ :with-creator)
	  (org-string-nw-p ($ :creator))
	  (format "#+creator: %s\n" ($ :creator)))
     ;; Finally, the contents.
     contents)))

(defun org-mybackend-headline (headline contents info)
  "Transcode HEADLINE element back into Org syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
  (ignore info)
  ;; Indent the headline appropriately.
  (org-element-put-property headline :level
			    (org-export-get-relative-level headline info))
  (org-element-headline-interpreter headline contents))

(defun org-mybackend-section (section contents info)
  "Transcode SECTION element back into Org syntax.
CONTENTS is the contents of the section.  INFO is a plist used as
a communication channel."
  (ignore section)
  ;; remove duplicate newlines at end of contents
  (org-element-normalize-string contents))

;;;###autoload
(defun org-mybackend-export-as-mybackend
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a MyBackend buffer."
  (interactive)
  (org-export-to-buffer 'mybackend "*Org MyBackend Export*"
    async subtreep visible-only body-only ext-plist
    (lambda ()
      ;; You'd activate mybackend-mode here for the buffer, if such a
      ;; mode was defined. For example:
      ;;   (mybackend-mode)
      ;; We activate org-mode since we export to org-mode.
      (org-mode))))

;;; This is how packages should end, generally speaking.
(provide 'ox-mybackend)

;;; ox-mybackend.el ends here
