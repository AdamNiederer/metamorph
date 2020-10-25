;;; metamorph.el --- Transform your buffers with lisp

;; Copyright 2018 Adam Niederer

;; Author: Adam Niederer <adam.niederer@gmail.com>
;; URL: http://github.com/AdamNiederer/metamorph
;; Version: 0.1
;; Keywords: metaprogramming wp
;; Package-Requires: ((emacs "24.4") (ov "1.0.6"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Use metamorph-map-region with a regular expression and a Lisp expression
;; to apply that Lisp expression to all matching strings within the region.
;;
;; Exported names start with "metamorph-"; private names start with
;; "metamorph--".

;;; Code:

(require 'ov)

(defgroup metamorph nil
  "Buffer transformation with lisp"
  :prefix "metamorph-"
  :link '(url-link :tag "Github" "https://github.com/AdamNiederer/metamorph")
  :link '(emacs-commentary-link :tag "Commentary" "metamorph"))

(defface metamorph-preview-face
  '((t :background "dark slate gray"))
  "Face used for metamorph preview overlays"
  :tag "Metamorph Preview Face"
  :group 'metamorph)

(defvar metamorph-ols nil
  "Overlays used to display inputs and results of a transformation.")

(defvar metamorph-buffer nil
  "The buffer from which the current metamorph transformation was invoked.")

(defun metamorph--stringify (obj)
  "If OBJ is a string, pass it through.  Otherwise, turn it into a string."
  (if (stringp obj) obj (prin1-to-string obj)))

(defmacro metamorph--save-everything (&rest exprs)
  "Perform EXPRS, preserving as much global state as possible."
  `(save-current-buffer
     (save-window-excursion
       (save-restriction
         (save-match-data
           (save-mark-and-excursion
             (save-window-excursion
               (with-demoted-errors "metamorph: error in user-provided transformation: %s"
                 ,@exprs))))))))

(defun metamorph--preview-re (input)
  "Preview the text that will be selected by INPUT."
  (ov-reset metamorph-ols)
  (with-current-buffer metamorph-buffer
    (let ((ols (condition-case err (ov-regexp input) ('error))))
      (setq metamorph-ols (sort ols (lambda (a b) (< (ov-beg a) (ov-beg b)))))))
  (ov-set metamorph-ols 'face 'metamorph-preview-face))

(defun metamorph--preview-tx (input &optional unsafe)
  "Preview the result of evaling INPUT on all selected text, evaling it unsafely if UNSAFE."
  (dolist (ol-and-index (seq-map-indexed (lambda (ol i) (list ol i)) metamorph-ols))
    (with-current-buffer metamorph-buffer
      (let* ((ol (car ol-and-index))
             (result (metamorph--eval
                      input
                      (buffer-substring (ov-beg ol) (ov-end ol))
                      (cadr ol-and-index)
                      (length metamorph-ols)
                      unsafe)))
        (ov-set ol 'before-string (propertize "[" 'face metamorph-preview-face))
        (ov-set ol 'after-string (propertize (concat " => " result "]")
                                                    'face metamorph-preview-face))))))

(defun metamorph--eval (to-eval content idx len &optional unsafe)
  "Eval TO-EVAL with CONTENT, IDX and LEN, and read CONTENT if UNSAFE is set."
  (let* ((% content) (%i (string-to-number %)) (%0 idx) (%n len))
    (cond
     (unsafe (let* ((%! (read %))) (metamorph--stringify (metamorph--save-everything (eval (read to-eval))))))
     (t (metamorph--stringify (metamorph--save-everything (eval (read to-eval))))))))

;;;###autoload
(defun metamorph-cleanup ()
  "Clean up all overlays created in the preview process."
  (interactive)
  (ov-reset metamorph-ols)
  (setq metamorph-ols nil)
  (setq metamorph-buffer nil))

(defun metamorph-apply-changes ()
  "Clean up all overlays created in the preview process."
  (message "%s" metamorph-ols)
  (dolist (ol metamorph-ols)
    (with-current-buffer metamorph-buffer
      (message "%s" ol)
      (save-excursion
        (delete-region (ov-beg ol) (ov-end ol))
        (goto-char (ov-beg ol))
        (insert (substring (overlay-get ol 'after-string) (length " => ") (1- (length (overlay-get ol 'after-string))))))))
  (metamorph-cleanup))

(defun metamorph--preview-re-hook ()
  "Add an after-change hook and some cleanup functions to the minibuffer."
  ;; TODO: Can we hook to C-g? We need to clean up these overlays on abort somehow.
  (add-hook 'after-change-functions (lambda (&rest _) (metamorph--preview-re (minibuffer-contents))) nil 'local))

(defun metamorph--preview-tx-hook ()
  "Add as an after-change hook and some cleanup functions to the minibuffer."
  ;; TODO: Can we hook to C-g? We need to clean up these overlays on abort somehow.
  (add-hook 'after-change-functions (lambda (&rest _) (metamorph--preview-tx (minibuffer-contents))) nil 'local))

(defun metamorph--preview-tx-unsafe-hook ()
  "Add as an after-change hook and some cleanup functions to the minibuffer."
  ;; TODO: Can we hook to C-g? We need to clean up these overlays on abort somehow.
  (add-hook 'after-change-functions (lambda (&rest _) (metamorph--preview-tx (minibuffer-contents) t)) nil 'local))

;;;###autoload
(defun metamorph-map-region ()
  "Replace all strings matching REGEX, with the result of TRANSFORM.

TRANSFORM can be any Lisp expression.  The result is stringified
via `prin1-to-string' before being placed in the buffer.  The
following values may be used in TRANSFORM:

- % is the raw matched string without any additional processing
- %i is the matched string converted to an integer
- %0 is an index which starts at zero, and increments for each match
- %n is the number of matched strings

This function does not read or evaluate any buffer contents
without explicit user direction, and is therefore safe to use on
untrusted buffers.  For more power, try `metamorph-map-region-unsafe'."
  (interactive)
  (setq metamorph-buffer (current-buffer))
  (minibuffer-with-setup-hook #'metamorph--preview-re-hook
    (read-string "Transform regex: "))
  (minibuffer-with-setup-hook #'metamorph--preview-tx-hook
    (read-string "Transformation: "))
  (metamorph-apply-changes))

;;;###autoload
(defun metamorph-map-region-unsafe ()
  "Replace all strings matching REGEX, with the result of TRANSFORM.

TRANSFORM can be any Lisp expression.  The result is stringified
via `prin1-to-string' before being placed in the buffer.  The
following values may be used in TRANSFORM:

- % is the raw matched string without any additional processing
- %i is the matched string converted to an integer
- %! is the value of the string as a Lisp expression
- %0 is an index which starts at zero, and increments for each match
- %n is the number of matched strings

Because % is read and evaluated as a Lisp expression, consider
using `metamorph-map-region' on untrusted buffers, or buffers
containing Emacs Lisp code."
  (interactive)
  (setq metamorph-buffer (current-buffer))
  (minibuffer-with-setup-hook #'metamorph--preview-re-hook
    (read-string "Transform regex: "))
  (minibuffer-with-setup-hook #'metamorph--preview-tx-unsafe-hook
    (read-string "Transformation: "))
  (metamorph-apply-changes))

(provide 'metamorph)

;;; metamorph.el ends here
