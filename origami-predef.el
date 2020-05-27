;;; origami-predef.el --- Apply folding when finding (opening) files  -*- lexical-binding: t; -*-

;; Author: Álvaro González Sotillo <alvarogonzalezsotillo@gmail.com>
;; URL: https://github.com/alvarogonzalezsotillo/origami-predef
;; Package-Requires: ((emacs "24") (origami "1.0"))
;; Version: 1.0
;; Keywords: convenience folding

;; This file is not part of GNU Emacs.

;;; License:

;; GNU General Public License v3.0. See COPYING for details.


;;; Commentary:

;; Apply predefined folding to a buffer, based on customizable string occurrences.
;; The origami package is used to perform the actual folding.
;;
;; Quick start:
;; Mark lines to fold with *origami-this*, *origami-next*, then invoke origami-predef-apply.
;; Use origami-predef-global-mode to apply folding automatically when finding (opening) files.
;; Use a hook on the major mode to apply custom folding with origami-predef-apply-patterns
;;
;; More information at https://github.com/alvarogonzalezsotillo/origami-predef



;;; Code:
(require 'origami)

(defgroup origami-predef-group nil
  "Apply predefined folding when finding (opening) files."
  :group 'convenience)

(defcustom origami-predef-strings-fold-this '("\\*origami-this\\*")  ; *origami-this*
  "When found, `origami-close-node' will the invoked on the same line."
  :type '(repeat string))

;;; *origami-next*
(defcustom origami-predef-strings-fold-next '("\\*origami-next\\*")
  "When found, `origami-close-node' will the invoked on the next line."
  :type '(repeat string))


(defun origami-predef--match-and-apply (pattern-or-patterns function)
  "Search buffer and apply the FUNCTION on each line.
PATTERN-OR-PATTERNS is a string or a list of strings to search"
  (let ((patterns (if (listp pattern-or-patterns) pattern-or-patterns (list pattern-or-patterns) )))
    (save-excursion
      (dolist (pattern patterns)
        (goto-char (point-min))
        (while (re-search-forward pattern nil t 1)
          (funcall function))))))

(defun origami-predef--hide-element-next-line ()
  "Apply origami-hide-element to the next line of current point."
  (forward-line)
  (origami-predef--hide-element-this-line))

(defun origami-predef--hide-element-this-line ()
  "Apply origami-hide-element to the line of current point."
  (move-end-of-line nil)
  (origami-close-node (current-buffer) (point)))

(defun origami-predef-apply()
  "Apply folding based on origami-predef-strings-fold-* variables."
  (interactive)
  (origami-predef-apply-patterns origami-predef-strings-fold-this origami-predef-strings-fold-next))


(defun origami-predef-apply-patterns (this-line &optional next-line)
  "Apply folding to patterns in THIS-LINE and NEXT-LINE.
The folding is performed by `origami-predef--hide-element-this-line'
and `origami-predef--hide-element-next-line'"
  (origami-predef--match-and-apply this-line #'origami-predef--hide-element-this-line)
  (origami-predef--match-and-apply next-line #'origami-predef--hide-element-next-line))


;;;###autoload
(define-minor-mode origami-predef-global-mode
  "Apply initial folding when finding (opening) a file buffer"
  :group 'origami-predef-group
  :global t
  
  (remove-hook 'find-file-hook #'origami-predef--find-file-hook t)
  (when origami-predef-global-mode
    (add-hook 'find-file-hook #'origami-predef--find-file-hook t)))

(defun origami-predef--find-file-hook ()
  "Called each time a file is opened."
  (origami-predef-apply))


(provide 'origami-predef)
;;; origami-predef.el ends here
