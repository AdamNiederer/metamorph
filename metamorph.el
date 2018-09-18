;;; metamorph.el --- replace-string, but crazier

;; Copyright 2018 Adam Niederer

;; Author: Adam Niederer <adam.niederer@gmail.com>
;; URL: http://github.com/AdamNiederer/metamorph
;; Version: 0.1
;; Keywords: metaprogramming
;; Package-Requires: ((emacs "24.4"))

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

(defun metamorph--stringify (obj)
  "If OBJ is a string, pass it through.  Otherwise, turn it into a string."
  (if (stringp obj) obj (prin1-to-string obj)))

;;;###autoload
(defun metamorph-map-region (regex transform)
  "Replace all strings matching REGEX, with the result of TRANSFORM.

TRANSFORM can be any Lisp expression.  The result is stringified
via `prin1-to-string' before being placed in the buffer.  The
following values may be used in TRANSFORM:

- % is the raw matched string without any additional processing
- %! is the value of the string as a Lisp expression
- %0 is an index which starts at zero, and increments for each match

Because % is read and evaluated as a Lisp expression, consider
using `metamorph-map-region-safe' on untrusted buffers, or
buffers containing Emacs Lisp code."
  (interactive "*sTransform regex: \nxTransformation: ")
  (let ((match-index 0)
        (search-extent (region-end))) ;; goto-char overwrites region data
    (goto-char (region-beginning))
    (while (re-search-forward regex search-extent t)
      (let* ((% (match-string 0))
             (%! (read %))
             (%0 match-index)
             (output (metamorph--stringify (eval transform))))
        (replace-match output t t)
        (setq search-extent (+ search-extent (- (length output) (length %))))
        (setq match-index (1+ match-index))))))

;;;###autoload
(defun metamorph-map-region-safe (regex transform)
  "Replace all strings matching REGEX, with the result of TRANSFORM.

TRANSFORM can be any Lisp expression.  The result is stringified
via `prin1-to-string' before being placed in the buffer.  The
following values may be used in TRANSFORM:

- % is the raw matched string without any additional processing
- %i is the matched string converted to an integer
- %0 is an index which starts at zero, and increments for each match

This function does not read or evaluate any buffer contents
without explicit user direction, and is therefore safe to use on
untrusted buffers."
  (interactive "*sTransform regex: \nxTransformation: ")
  (let ((match-index 0)
        (search-extent (region-end))) ;; goto-char overwrites region data
    (goto-char (region-beginning))
    (while (re-search-forward regex search-extent t)
      (let* ((% (match-string 0))
             (%i (string-to-number %))
             (%0 match-index)
             (output (metamorph--stringify (eval transform))))
        (replace-match (metamorph--stringify (eval transform)) t t)
        (setq search-extent (+ search-extent (- (length output) (length %))))
        (setq match-index (1+ match-index))))))

(provide 'metamorph)

;;; metamorph.el ends here
