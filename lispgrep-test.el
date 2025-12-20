;;; lispgrep-test.el --- Tests for lispgrep  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for lispgrep.

;;; Code:

(require 'ert)
(require 'seq)
(require 'lispgrep)

(defmacro lispgrep-test--with-temp-dir (var &rest body)
  "Bind VAR to a temporary directory and evaluate BODY there."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (make-temp-file "lispgrep-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))

(defun lispgrep-test--write-file (file contents &optional modes)
  "Write CONTENTS to FILE and optionally set MODES."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert contents))
  (when modes
    (set-file-modes file modes))
  file)

(defun lispgrep-test--make-tree-fixture (root)
  "Create the standard recursive file tree under ROOT."
  (list :alpha
        (lispgrep-test--write-file
         (expand-file-name "alpha.txt" root)
         "alpha\nbeta target\ngamma target target\n")
        :beta
        (lispgrep-test--write-file
         (expand-file-name "beta.el" root)
         "(message \"target from elisp\")\n")
        :gamma
        (lispgrep-test--write-file
         (expand-file-name "subdir/gamma.txt" root)
         "first line\ntarget in subdir\n")
        :nonmatch
        (lispgrep-test--write-file
         (expand-file-name "subdir/nonmatch.md" root)
         "nothing to see here\n")
        :sample-noeol
        (lispgrep-test--write-file
         (expand-file-name "sample-noeol.txt" root)
         "abc")))

(ert-deftest lispgrep-find-files-recurses-and-filters ()
  (lispgrep-test--with-temp-dir root
                                (lispgrep-test--make-tree-fixture root)
                                (let* ((files (sort (lispgrep-find-files root "\\.txt\\'") #'string<))
                                       (relative-files (mapcar (lambda (file)
                                                                 (file-relative-name file root))
                                                               files)))
                                  (should (equal relative-files
                                                 '("alpha.txt" "sample-noeol.txt" "subdir/gamma.txt"))))))

(ert-deftest lispgrep-find-files-excluding-vc-skips-dot-git ()
  (lispgrep-test--with-temp-dir root
                                (make-directory (expand-file-name "src" root))
                                (make-directory (expand-file-name ".git" root))
                                (with-temp-file (expand-file-name "src/main.el" root)
                                  (insert "(message \"fixture target\")\n"))
                                (with-temp-file (expand-file-name "src/notes.txt" root)
                                  (insert "project target\n"))
                                (with-temp-file (expand-file-name ".git/config" root)
                                  (insert "[core]\n\trepositoryformatversion = 0\n"))
                                (let* ((files (sort (lispgrep-find-files-excluding-vc root ".") #'string<))
                                       (relative-files (mapcar (lambda (file)
                                                                 (file-relative-name file root))
                                                               files)))
                                  (should (equal relative-files
                                                 '("src/main.el" "src/notes.txt"))))))

(ert-deftest lispgrep-match-files-collects-line-data ()
  (lispgrep-test--with-temp-dir root
                                (let* ((fixture (lispgrep-test--make-tree-fixture root))
                                       (file (plist-get fixture :alpha))
                                       (matches (lispgrep-match-files (list file) "target"))
                                       (file-result (cdr (assoc file matches)))
                                       (line-2 (nth 0 file-result))
                                       (line-3 (nth 1 file-result)))
                                  (should (= 2 (length file-result)))
                                  (should (equal 2 (alist-get :line line-2)))
                                  (should (equal "beta target" (alist-get :text line-2)))
                                  (should (equal '((:match-line-start . 5) (:match-len . 6))
                                                 (car (alist-get :matches line-2))))
                                  (should (equal 3 (alist-get :line line-3)))
                                  (should (equal 2 (length (alist-get :matches line-3)))))))

(ert-deftest lispgrep-match-files-handles-zero-length-match-at-eof ()
  (lispgrep-test--with-temp-dir root
                                (let* ((fixture (lispgrep-test--make-tree-fixture root))
                                       (file (plist-get fixture :sample-noeol))
                                       (matches (lispgrep-match-files (list file) "$"))
                                       (file-result (cdr (assoc file matches)))
                                       (line-result (car file-result))
                                       (match (car (alist-get :matches line-result))))
                                  (should (= 1 (length file-result)))
                                  (should (equal 1 (alist-get :line line-result)))
                                  (should (equal "abc" (alist-get :text line-result)))
                                  (should (equal '((:match-line-start . 3) (:match-len . 0)) match)))))

(ert-deftest lispgrep-match-files-skips-unreadable-files ()
  (skip-unless (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin berkeley-unix)))
  (lispgrep-test--with-temp-dir root
                                (let ((file (expand-file-name "secret.txt" root)))
                                  (with-temp-file file
                                    (insert "target\n"))
                                  (unwind-protect
                                      (progn
                                        (set-file-modes file #o000)
                                        (skip-unless (not (file-readable-p file)))
                                        (should (equal nil (lispgrep-match-files (list file) "target"))))
                                    (set-file-modes file #o600)))))

(ert-deftest lispgrep-matches-to-xref-creates-xrefs ()
  (lispgrep-test--with-temp-dir root
                                (let* ((fixture (lispgrep-test--make-tree-fixture root))
                                       (file (plist-get fixture :alpha))
                                       (matches (lispgrep-match-files (list file) "target"))
                                       (xrefs (lispgrep-matches-to-xref matches))
                                       (xref (car xrefs))
                                       (location (xref-item-location xref)))
                                  (should (= 3 (length xrefs)))
                                  (should (string-match-p "target" (xref-item-summary xref)))
                                  (should (equal file (xref-file-location-file location)))
                                  (should (= 2 (xref-file-location-line location))))))

(ert-deftest lispgrep-matches-to-xref-preserves-same-line-match-columns ()
  (lispgrep-test--with-temp-dir root
                                (let* ((fixture (lispgrep-test--make-tree-fixture root))
                                       (file (plist-get fixture :alpha))
                                       (matches (lispgrep-match-files (list file) "target"))
                                       (xrefs (lispgrep-matches-to-xref matches))
                                       (line-3-xrefs (seq-filter
                                                      (lambda (xref)
                                                        (= 3 (xref-location-line (xref-item-location xref))))
                                                      xrefs))
                                       (columns (mapcar (lambda (xref)
                                                          (xref-file-location-column
                                                           (xref-item-location xref)))
                                                        line-3-xrefs)))
                                  (should (= 2 (length line-3-xrefs)))
                                  (should (equal '(6 13) columns)))))

(provide 'lispgrep-test)
;;; lispgrep-test.el ends here
