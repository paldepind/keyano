;;; keym.el --- keym editing -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Simon Friis Vindum

;; Author: Simon Friis Vindum <simon@vindum.io>
;; Keywords: lisp
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Description of keym.el here.

;;; Code:

(require 'cl-lib)
(require 'multiple-cursors)

(cl-defstruct object
  find-next expand-left expand-right) ; find-previous matches)

(defvar line-object
  (make-object
   :find-next
   (lambda (_start end)
     (save-excursion
       (goto-char end)
       (forward-line)
       (list (point) (line-end-position))))
   :expand-left
   (lambda (start _end)
     (save-excursion
       (goto-char start)
       (line-beginning-position)))
   :expand-right
   (lambda (_start end)
     (save-excursion
       (goto-char end)
       (line-end-position)))))

(defvar word-object
  (make-object
   :find-next
   (lambda (_start end)
     (let ((from 0))
       (save-excursion
	 (goto-char end)
	 (forward-to-word 1)
	 (setq from (point))
	 (forward-word)
	 (list from (point)))))))

(defvar parentheses-object
  (make-object
   :find-next
   (lambda (_start end)
     (save-excursion
       (goto-char (+ end 1))
       (let ((result (search-forward "(")))
	 (if result
	     (progn
	       (backward-char)
	       (forward-sexp)
	       (list (- result 1) (point)))
	   nil))))))

(defun keym-line ()
  "Command representing a line object."
  (interactive)
  (cl-destructuring-bind
      (from to) (funcall (object-find-next line-object) (point) (point))
    (progn
      (goto-char from)
      (set-mark to))))

(defun keym-word ()
  "Command representing a word object."
  (interactive)
  (cl-destructuring-bind
      (from to) (funcall (object-find-next word-object) (point) (point))
    (progn
      (goto-char from)
      (set-mark to))))

(defun keym-parentheses ()
  "Command representing a word object."
  (interactive)
  (cl-destructuring-bind
      (from to) (funcall (object-find-next parentheses-object) (point) (point))
    (progn
      (goto-char from)
      (set-mark to))))

(defvar keym-mode-map (make-sparse-keymap)
  "Keymap used for normal mode.")

(define-key keym-mode-map "w" 'keym-word)
(define-key keym-mode-map "l" 'keym-line)
(define-key keym-mode-map "p" 'keym-parentheses)

(define-minor-mode keym-mode
  "Keym mode"
  :keymap keym-mode-map)

;;; keym.el ends here
