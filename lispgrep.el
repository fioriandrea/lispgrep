;;; lispgrep.el --- Recursive file finding and text searching in pure elisp  -*- lexical-binding: t; -*-

;; Copyright (C) Andrea Fiori

;; Author: Andrea Fiori <andrea.fiori.1998@gmail.com>
;; Maintainer: Andrea Fiori <andrea.fiori.1998@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, extensions, files, matching, tools, unix
;; URL: https://github.com/fioriandrea/lispgrep

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Recursive file finding and text searching implemented in pure Emacs Lisp.
;; Useful in environments without grep and find installed (such as MS-Windows).

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'project)
(require 'xref)

(defgroup lispgrep nil
  "Grep-like functionality implemented in pure Emacs Lisp."
  :group 'tools
  :prefix "lispgrep-")

(defun lispgrep-read-directory-name (&optional prompt)
  "Prompt for directory name.  Optionally, provide PROMPT."
  (expand-file-name
   (read-directory-name (or prompt "Base directory: ")
		        default-directory nil t)))

(defun lispgrep-read-regexp (&optional prompt)
  "Prompt for regexp.  Optionally, provide PROMPT."
  (read-regexp (or prompt "Search for")
               'find-tag-default-as-regexp
               'grep-regexp-history))

;;;; Find

(defun lispgrep-find-files (directory regexp &optional dir-filter)
  "Find files matching REGEXP in DIRECTORY.
DIR-FILTER is an optional predicate accepting (PARENT-DIR CURRENT-DIR);
if it returns nil, do not descend into CURRENT-DIR."
  (setq directory (directory-file-name directory))
  (let ((results nil)
        (full-files nil)
        (files (condition-case err
                   (directory-files
                    directory nil
                    directory-files-no-dot-files-regexp)
                 (file-error (message
                              "lispgrep-find-files: couldn't get files for dir %S because of %S"
                              directory err)
                             nil))))
    (dolist (file files)
      (let ((full-file (concat directory "/" file)))
        (if (file-directory-p full-file)
            (unless (or (file-symlink-p full-file)
                        (and dir-filter (not (funcall dir-filter directory file))))
              (setq results (nconc results (lispgrep-find-files full-file regexp dir-filter))))
          (when (string-match regexp file)
	    (push full-file full-files)))))
    (nconc results (nreverse full-files))))

(defun lispgrep-find-files-excluding-vc (directory regexp)
  "Like `lispgrep-find-files', but don't descend into vc-managed dirs."
  (lispgrep-find-files directory regexp
                       (lambda (_parent file)
                         (not (member file vc-directory-exclusion-list)))))

;;;###autoload
(defun lispgrep-find-name-dired (directory regexp &optional dir-filter)
  "Find files matching REGEXP in DIRECTORY.
A Dired buffer with the results will be opened and returned.

With a prefix argument, DIR-FILTER is read interactively from the
minibuffer.  See `lispgrep-find-files' for the semantics of DIR-FILTER."
  (interactive (list
                (lispgrep-read-directory-name)
                (lispgrep-read-file-regexp)
                (if current-prefix-arg
                    (read-minibuffer "Dir filter: " "(lambda (parent file) t)")
                  nil)))
  (dired (cons directory
               (lispgrep-find-files directory regexp dir-filter))))

;;;###autoload
(defun lispgrep-find-name-dired-excluding-vc (directory regexp)
  "Like `lispgrep-find-name-dired', but don't descend into vc-managed dirs."
  (interactive (list
                (lispgrep-read-directory-name)
                (lispgrep-read-file-regexp)))
  (dired (cons directory
               (lispgrep-find-files-excluding-vc directory regexp))))

(defmacro lispgrep-with-project-files-fallback (&rest body)
  "Execute BODY with `project-files' temporarily redefined.

If the original `project-files' function errors (e.g., if Unix find is
unavailable), it falls back to `lispgrep-find-files-excluding-vc' to
find files within the project."
  (declare (indent 0))
  (let ((orig (make-symbol "orig")))
    `(cl-letf* ((,orig (symbol-function 'project-files))
                ((symbol-function 'project-files)
                 (lambda (project &optional dirs)
                   (condition-case err
                       (apply ,orig (list project dirs))
                     (error
                      (message "project-files error: %S" err)
                      (mapcan (lambda (d)
                                (lispgrep-find-files-excluding-vc d "."))
                              (or dirs (list (project-root project)))))))))
       ,@body)))

;;;###autoload
(defun lispgrep-project-find-file (&optional include-all)
  "Like `project-find-file', but with fallback in case it errors out."
  (interactive "P")
  (lispgrep-with-project-files-fallback
    (project-find-file include-all)))

;;;###autoload
(defun lispgrep-project-find-dir ()
  "Like `project-find-dir', but with fallback in case it errors out."
  (interactive)
  (lispgrep-with-project-files-fallback
    (project-find-dir)))

;;;; Grep

(defcustom lispgrep-insert-files-literally nil
  "Whether lispgrep commands should insert files literally or not."
  :type 'boolean
  :group 'lispgrep)

(defun lispgrep-match-files (files regexp)
  "Search FILES (and their subdirectories) for REGEXP.

Returns a list of (FILE . MATCHES).
Each MATCHES entry is a list of line-level data:

\((:line . LINENUM-INT) (:text . LINETEXT-STRING) (:matches . LINE-MATCHES)).

LINE-MATCHES is a list of match details for that line:
\((:match-line-start . START-COL-INT) (:match-len . LENGTH-INT))

Uses `lispgrep-insert-files-literally' to determine how files are read."
  (let ((insert-file-contents-function (if lispgrep-insert-files-literally
                                           #'insert-file-contents-literally
                                         #'insert-file-contents)))
    (cl-labels
        ((process-files-and-dirs (files)
           (cl-loop for file in files nconc
                    (condition-case err
                        (if (file-directory-p file)
                            (process-files-and-dirs (lispgrep-find-files file "."))
                          (let ((res (process-one-file file)))
                            (when res
                              (list (cons file res)))))
                      (file-error
                       (message "lispgrep-match-files: failed to grep %S because of %S"
                                file err)
                       nil))))
         (process-one-file (file)
           (funcall
            insert-file-contents-function file nil nil nil 'if-regular)
           (goto-char (point-min))
           (let ((lines (list (cons -1 "")))
                 (matches nil))
             (while (and (not (eobp))
                         (re-search-forward regexp nil t))
               (let* ((line (line-number-at-pos))
                      (line-beg (line-beginning-position))
                      (match-beg (match-beginning 0))
                      (match-line-start (- match-beg line-beg))
                      (match-len (- (match-end 0) match-beg)))
                 (when (/= (caar lines) line)
                   (push (cons line (buffer-substring-no-properties
                                     line-beg (line-end-position)))
                         lines))
                 (push (cons line (list (cons :match-line-start match-line-start)
                                        (cons :match-len match-len)))
                       matches)
                 (when (= match-len 0)
                   (unless (eobp)
                     (forward-char 1)))))
             (group-matches lines matches)))
         (group-matches (lines matches)
           (let ((grouped (make-hash-table)))
             (cl-loop for (linenr . match) in matches do
                      (push match
                            (gethash linenr grouped '())))
             (cl-loop with matches-by-line = ()
                      for (linenr . linetext) in lines
                      unless (= linenr -1)
                      do (push (list
                                (cons :line linenr)
                                (cons :text linetext)
                                (cons :matches (gethash linenr grouped)))
                               matches-by-line)
                      finally return matches-by-line))))
      (with-temp-buffer
        (process-files-and-dirs files)))))

(defun lispgrep-matches-to-xref (matches)
  "Convert MATCHES into a list of `xref-match' objects.

MATCHES uses the same structure produced by `lispgrep-match-files'."
  (cl-loop
   for (file . file-matches) in matches
   nconc (cl-loop
          for file-match in file-matches
          for line = (alist-get :line file-match)
          for text = (alist-get :text file-match)
          for textlen = (length text)
          for line-matches = (alist-get :matches file-match)
          nconc (nreverse
                 (cl-loop
                  with prev-start = textlen
                  with first-match = (car line-matches)
                  with first-start = (alist-get :match-line-start first-match)
                  for line-match in (nreverse line-matches)
                  for start = (alist-get :match-line-start line-match)
                  for len = (alist-get :match-len line-match)
                  for sumstart = (if (= start first-start)
                                     0 start)
                  for summary = (substring text sumstart prev-start)
                  do (add-face-text-property
                      (- start sumstart) (- (+ start len) sumstart)
                      'xref-match t summary)
                  (setq prev-start start)
                  collect (xref-make-match summary
                                           (xref-make-file-location
                                            file line start)
                                           len))))))

(defun lispgrep-xrefs-show (regexp files)
  "Show REGEXP matches from FILES in the xref UI."
  (let ((fetcher (lambda (regexp files)
                   (unless files
                     (user-error "Empty file list"))
                   (let* ((matches (lispgrep-match-files files regexp))
                          (xrefs (lispgrep-matches-to-xref matches)))
                     (unless xrefs
                       (user-error "No matches for: %s" regexp))
                     xrefs))))
    (xref-show-xrefs (apply-partially fetcher regexp files) nil)))

(defvar lispgrep-file-regexp-history nil)

(defun lispgrep-read-file-regexp ()
  "Read a file-name regular expression.
Uses `lispgrep-file-regexp-history' for history."
  (read-regexp "File name regexp"
               "." 'lispgrep-file-regexp-history))

;;;###autoload
(defun lispgrep-rgrep (regexp file-regexp dir)
  "Like `rgrep', but doesn't require external programs.
Also, uses FILE-REGEXP instead of glob to match file names."
  (interactive
   (list
    (lispgrep-read-regexp)
    (lispgrep-read-file-regexp)
    (lispgrep-read-directory-name)))
  (lispgrep-xrefs-show
   regexp (lispgrep-find-files-excluding-vc dir file-regexp)))

;;;###autoload
(defun lispgrep-project-find-regexp (regexp)
  "Like `project-find-regexp', but doesn't require external programs."
  (interactive (list (lispgrep-read-regexp)))
  (if current-prefix-arg
      (let ((directory (lispgrep-read-directory-name))
            (file-regexp (lispgrep-read-file-regexp)))
        (lispgrep-rgrep regexp file-regexp directory))
    (let* ((pr (project-current t))
           (default-directory (project-root pr))
           (files (lispgrep-with-project-files-fallback
                    (project-files pr))))
      (lispgrep-xrefs-show regexp files))))

;;;###autoload
(defun lispgrep-dired-do-find-regexp (regexp)
  "Like `dired-do-find-regexp', but doesn't require external programs."
  (interactive (list (read-regexp "Find regexp"
                                  nil 'dired-regexp-history))
               dired-mode)
  (lispgrep-xrefs-show regexp (dired-get-marked-files)))

;;;###autoload
(defun lispgrep-dired-do-occur (regexp &optional nlines)
  "Perform `multi-occur' on marked files matching REGEXP.
Optionally, NLINES parameter can be passed to `multi-occur'.
Each file gets opened in it's own buffer, so this might be slow
on many files."
  (interactive (occur-read-primary-args) dired-mode)
  (multi-occur
   (mapcar #'find-file-noselect (dired-get-marked-files))
   regexp nlines))

(provide 'lispgrep)

;;; lispgrep.el ends here
