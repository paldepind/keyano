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

(defun cmd-to-run-once (command)
  "Add COMMAND to the list of commands to run once."
  (setq mc--default-cmds-to-run-once
	(cons command mc--default-cmds-to-run-once)))

(defun cmd-to-run-for-all (command)
  "Add COMMAND to the list of commands to run for all."
  (setq mc--default-cmds-to-run-for-all
	(cons command mc--default-cmds-to-run-for-all)))

(defun region-beginning-or-point ()
  "Get the beginning of the region or point if region is inactive."
  (if (mark) (region-beginning) (point)))

(defun region-end-or-point ()
  "Get the beginning of the region or point if region is inactive."
  (if (mark) (region-end) (point)))

(cl-defstruct object
  find-next find-previous expand-left expand-right) ; find-previous matches)

(defvar keym-state 'normal)

(defvar keym-last-object nil)

(defvar line-object
  (make-object
   :find-next
   (lambda ()
     (forward-line)
     (set-mark (point))
     (end-of-line))
   :expand-left
   (lambda ()
     (save-excursion
       (goto-char start)
       (line-beginning-position)))
   :expand-right
   (lambda ()
     (save-excursion
       (goto-char end)
       (line-end-position)))))

(defvar word-object
  (make-object
   :find-next
   (lambda ()
     (forward-to-word 1)
     (set-mark (point))
     (forward-word))))

(defvar parentheses-object
  (make-object
   :find-next
   (lambda ()
     (forward-char)
     (when (search-forward "(")
       (backward-char)
       (set-mark (point))
       (forward-sexp)))))

(defun keym--object-command (object)
  "Execute an object command with the given OBJECT."
  (goto-char (region-beginning-or-point))
  (funcall (object-find-next object))
  (setq keym-last-object object))

(defun keym-line ()
  "Command representing a line object."
  (interactive)
  (keym--object-command line-object))

(cmd-to-run-for-all 'keym-line)

(defun keym-word ()
  "Command representing a word object."
  (interactive)
  (keym--object-command word-object))

(cmd-to-run-for-all 'keym-word)

(defun keym-parentheses ()
  "Command representing a line object."
  (interactive)
  (keym--object-command parentheses-object))

(cmd-to-run-for-all 'keym-parentheses)

(defun keym-next ()
  "Move to the next occurrence of the current object."
  (interactive)
  (keym--object-command keym-last-object))

(cmd-to-run-for-all 'keym-next)

(defun keym-prev ()
  "Move to the next occurrence of the current object."
  (interactive)
  (keym--object-command keym-last-object))

(cmd-to-run-for-all 'keym-next)

(defun keym-add-next ()
  "Select the next occurrence of the current object."
  (interactive)
  (mc/create-fake-cursor-at-point)
  (keym-next)
  (mc/maybe-multiple-cursors-mode))

(cmd-to-run-once 'keym-add-next)

(defvar keym-command-mode-map (make-sparse-keymap)
  "Keymap used for normal mode.")

(define-key keym-command-mode-map "w" 'keym-word)
(define-key keym-command-mode-map "l" 'keym-line)
(define-key keym-command-mode-map "p" 'keym-parentheses)
(define-key keym-command-mode-map "i" 'keym-insert-left)
(define-key keym-command-mode-map "I" 'keym-insert-right)
(define-key keym-command-mode-map "c" 'keym-change)
(define-key keym-command-mode-map "n" 'keym-next)
(define-key keym-command-mode-map "N" 'keym-add-next)
(define-key keym-command-mode-map (kbd "C->") 'mc/mark-next-like-this)

(defun keym-command-to-insert ()
  "Switch from insert state to command state."
  (setq keym-state 'insert)
  (keym-command-mode -1)
  (keym-insert-mode 1))

(defun keym-insert-to-command ()
  "Switch from insert state to command state."
  (interactive)
  (setq keym-state 'command)
  (keym-insert-mode -1)
  (keym-command-mode 1))

(cmd-to-run-once 'keym-insert-to-command)

(defun keym-insert-left ()
  "Switch from insert state to command state.
Place the cursor at the left side of the region."
  (interactive)
  (let ((n (min (point) (mark))))
    (goto-char n)
    (set-mark n)
    (keym-command-to-insert)))

(cmd-to-run-for-all 'keym-insert-left)

(defun keym-insert-right ()
  "Switch from insert state to command state.
Place the cursor at the right side of the region."
  (interactive)
  (let ((n (max (point) (mark))))
    (goto-char n)
    (set-mark n)
    (keym-command-to-insert)))

(cmd-to-run-for-all 'keym-insert-right)

(defun keym-change ()
  "Kill the current selections and enters insert state."
  (interactive)
  (kill-region 0 0 t)
  (keym-command-to-insert))

(cmd-to-run-for-all 'keym-change)

(define-minor-mode keym-command-mode
  "Keym mode"
  :lighter " cmd"
  :keymap keym-command-mode-map
  (setq cursor-type 'box))

(define-minor-mode keym-insert-mode
  "Keym mode"
  :lighter " ins"
  :keymap
  '(((kbd "q") . keym-insert-to-command)
    (([escape]) . keym-insert-to-command))
  (setq cursor-type 'bar))

;;; keym.el ends here
