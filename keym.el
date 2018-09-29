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
  find find-start find-end) ; matches)

(defvar keym-state 'normal)

(defvar keym-last-object nil)

(defun create-regexp-object (full start end)
  "Create an object based on regular expressions.
FULL is a regexp that matches the entire object, START must match
the start of the object, and END matches the end.  Returns an
object struct."
  (make-object
   :find
   (lambda (n &optional limit)
     (when (search-forward-regexp full limit t n)
       (set-mark (match-beginning 0))
       (goto-char (match-end 0))))
   :find-start
   (lambda (n &optional limit)
     (when (search-forward-regexp start limit t n)
       (goto-char (match-beginning 0))))
   :find-end
   (lambda (n &optional limit)
     (when (search-forward-regexp end limit t n)
       (goto-char (match-end 0))))))

(defvar char-object
  (create-regexp-object "." "." "."))

(defvar word-object
  (create-regexp-object "\\<\\w*\\>" "\\<" "\\w\\>"))

(defvar number-object
  (create-regexp-object "\\<[0-9]+\\(\\.[0-9]+\\)?\\>" "\\<[0-9]" "[0-9]\\>"))

(defvar line-object
  (create-regexp-object "^.*$" "^." ".$"))

(defvar parentheses-object
  (make-object
   :find
   (lambda (n &optional limit)
     (when (search-forward "(" limit t n)
       (goto-char (match-beginning 0))
       (set-mark (point))
       (forward-sexp)))))

(defun keym--object-command (object)
  "Execute an object command with the given OBJECT."
  ;; (goto-char (region-beginning-or-point))
  ;; (funcall (object-find object) 1)
  (setq keym-last-object object))

(defun keym-char ()
  "Command representing a char object."
  (interactive)
  (keym--object-command char-object))

(cmd-to-run-for-all 'keym-char)

(defun keym-word ()
  "Command representing a word object."
  (interactive)
  (keym--object-command word-object))

(defun keym-number ()
  "Command representing a number object."
  (interactive)
  (keym--object-command number-object))

(cmd-to-run-for-all 'keym-word)

(defun keym-line ()
  "Command representing a line object."
  (interactive)
  (keym--object-command line-object))

(cmd-to-run-for-all 'keym-line)

(defun keym-parentheses ()
  "Command representing a line object."
  (interactive)
  (keym--object-command parentheses-object))

(cmd-to-run-for-all 'keym-parentheses)

(defun keym-next-in ()
  "Move to the next occurrence of the current object in the selection."
  (interactive)
  (when keym-last-object
    (let ((orig-from (region-beginning-or-point))
	  (orig-to (region-end-or-point)))
      (goto-char (region-beginning-or-point))
      (funcall (object-find keym-last-object) 1)
      (when (and (= orig-from (region-beginning-or-point))
		 (= orig-to (region-end-or-point)))
	(goto-char (+ (region-beginning-or-point) 1))
	(funcall (object-find keym-last-object) 1)))))

(cmd-to-run-for-all 'keym-next-in)

(defun keym-next-after ()
  "Move to the next occurrence of the current object after the selection."
  (interactive)
  (when keym-last-object
    (goto-char (region-end-or-point))
    (funcall (object-find keym-last-object) 1)))

(cmd-to-run-for-all 'keym-next-after)

(defun keym-previous ()
  "Move to the previous occurrence of the current object."
  (interactive)
  (when keym-last-object
    (goto-char (region-beginning-or-point))
    (funcall (object-find keym-last-object) -1)))

(cmd-to-run-for-all 'keym-previous)

(defun keym-expand ()
  "Expand the current selection."
  (interactive)
  (when keym-last-object
    (when (< (mark) (point))
      (exchange-point-and-mark))
    (funcall (object-find-start keym-last-object) -1)
    (exchange-point-and-mark)
    (funcall (object-find-end keym-last-object) 1)))

(defun keym-include-next ()
  "Grow the selection to include the next occurrence of the curent object."
  (interactive)
  (let ((from (region-beginning-or-point)))
    (keym-next-after)
    (set-mark from)))

(cmd-to-run-for-all 'keym-include-next)

(defun keym-add-next ()
  "Select the next occurrence of the current object."
  (interactive)
  (mc/create-fake-cursor-at-point)
  (keym-next-after)
  (mc/maybe-multiple-cursors-mode))

(cmd-to-run-once 'keym-add-next)

(defun keym-add-previous ()
  "Select the next occurrence of the current object."
  (interactive)
  (mc/create-fake-cursor-at-point)
  (keym-previous)
  (mc/maybe-multiple-cursors-mode))

(cmd-to-run-once 'keym-add-previous)

(defvar keym-command-mode-map (make-sparse-keymap)
  "Keymap used for normal mode.")

;; Left-hand side

(define-key keym-command-mode-map "p" 'keym-parentheses)
(define-key keym-command-mode-map "w" 'keym-number)

(define-key keym-command-mode-map "a" 'keym-char)
(define-key keym-command-mode-map "r" 'keym-word)
(define-key keym-command-mode-map "s" 'keym-line)
(define-key keym-command-mode-map "t" 'keym-change)

(define-key keym-command-mode-map "z" 'undo)
(define-key keym-command-mode-map "x" 'kill-region)
(define-key keym-command-mode-map "c" 'kill-ring-save)
(define-key keym-command-mode-map "v" 'yank)

;; Right-hand side

(define-key keym-command-mode-map "l" 'keym-insert-left)
(define-key keym-command-mode-map "u" 'keym-insert-right)
(define-key keym-command-mode-map ";" 'comment-or-uncomment-region)

(define-key keym-command-mode-map "n" 'keym-next-in)
(define-key keym-command-mode-map "N" 'keym-include-next)
(define-key keym-command-mode-map (kbd "C-n") 'keym-next-after)
(define-key keym-command-mode-map (kbd "M-n") 'keym-add-next)
(define-key keym-command-mode-map "e" 'keym-previous)
(define-key keym-command-mode-map (kbd "C-e") 'keym-previous)
(define-key keym-command-mode-map "E" 'keym-previous)
(define-key keym-command-mode-map "m" 'keym-expand)
(define-key keym-command-mode-map "k" 'keym-all-in)

(define-key keym-command-mode-map (kbd "C->") 'mc/mark-next-like-this)

(defun keym-all-in ()
  "Select all of the current object in the current selection."
  (interactive)
  (let ((to (region-end-or-point))
	(find (object-find keym-last-object)))
    (goto-char (region-beginning-or-point))
    (while (funcall find 1 to)
      (mc/create-fake-cursor-at-point))
      (mc/maybe-multiple-cursors-mode)))

(cmd-to-run-for-all 'keym-all-in)

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

(cmd-to-run-for-all 'kill-region)

(defun keym-change ()
  "Kill the current selections and enters insert state."
  (interactive)
  (kill-region 0 0 t)
  (keym-command-to-insert))

(cmd-to-run-for-all 'keym-change)

(define-minor-mode keym-command-mode
  "Keym command mode"
  :lighter " cmd"
  :keymap keym-command-mode-map
  :global t
  (delete-selection-mode 1)
  (setq cursor-type 'box))

(define-minor-mode keym-insert-mode
  "Keym insert mode"
  :lighter " ins"
  :keymap
  '(((kbd "q") . keym-insert-to-command)
    (([escape]) . keym-insert-to-command))
  (setq cursor-type 'bar))

(provide 'keym)

;;; keym.el ends here
