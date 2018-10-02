;;; keyano.el --- keyano editing -*- lexical-binding: t; -*-

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

;; Description of keyano.el here.

;;; Code:

(require 'cl-lib)
(require 'seq)
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
  (if mark-active (region-beginning) (point)))

(defun region-end-or-point ()
  "Get the beginning of the region or point if region is inactive."
  (if mark-active (region-end) (point)))

(defun keyano--selection ()
  "Get the beginning and end of the current selection."
  (if mark-active (list (region-beginning) (region-end)) (list (point) (point))))

(defun keyano--set-selection (from to)
  "Set the current selection FROM TO."
  (set-mark from)
  (goto-char to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selectors

(cl-defstruct object
  find find-start find-end expand kill-range matches)

(defvar keyano--current-object nil)

(defun keyano--object ()
  "Get the current object."
  (if keyano--current-object keyano--current-object word-object))

(defun keyano--create-constant-object (str)
  "Create an object based on STR."
  (make-object
   :find
   (lambda (n &optional limit)
     (when (search-forward str limit t n)
       (set-mark (match-beginning 0))
       (goto-char (match-end 0))))
   :find-start
   (lambda (n &optional limit)
     (when (search-forward str limit t n)
       (goto-char (match-beginning 0))))
   :find-end
   (lambda (n &optional limit)
     (when (search-forward str limit t n)
       (goto-char (match-end 0))))
   :expand
   (lambda ()
     (backward-char (length str))
     (when (search-forward-regexp str (+ (point) (* 2 (length str))) t)
       (set-mark (match-beginning 0))
       (goto-char (match-end 0))))))

(defun create-regexp-object (full subexp start end)
  "Create an object based on regular expressions.
FULL is a regexp that matches the entire object or more.  SUBEXP
denotes which subexpression to use.  START must match the start
of the object, and END matches the end.  Returns an object
struct."
  (make-object
   :find
   (lambda (n &optional limit)
     (when (search-forward-regexp full limit t n)
       (set-mark (match-beginning subexp))
       (goto-char (match-end subexp))))
   :find-start
   (lambda (n &optional limit)
     (when (search-forward-regexp start limit t n)
       (goto-char (match-beginning 0))))
   :find-end
   (lambda (n &optional limit)
     (when (search-forward-regexp end limit t n)
       (goto-char (match-end 0))))
   :expand
   (lambda ()
     (when (search-forward-regexp end nil t)
       (set-mark (point))
       (search-backward-regexp start nil t)))))

(defvar char-object
  (create-regexp-object "." 0 "." "."))

(defvar word-object
  (create-regexp-object "\\<\\w*\\>" 0 "\\<" "\\w\\>"))

(defvar number-object
  (create-regexp-object "\\<[0-9]+\\(\\.[0-9]+\\)?\\>" 0 "\\<[0-9]" "[0-9]\\>"))

(defvar line-object
  (let ((object (create-regexp-object "^[\t ]*\\(.*\\)$" 1 "^." ".$")))
    (setf (object-kill-range object)
	  (lambda ()
	    (seq-let (from to) (keyano--selection)
	      (goto-char from)
	      (beginning-of-line)
	      (set-mark (point))
	      (goto-char to)
	      (forward-char))))
    object))

(defvar parentheses-object
  (make-object
   :find
   (lambda (n &optional limit)
     (when (search-forward "(" limit t n)
       (goto-char (match-beginning 0))
       (set-mark (point))
       (forward-sexp)
       t))))

(defvar current-selection-object
  (make-object
   :find
   (lambda (n &optional limit)
     (let ((s (buffer-substring-no-properties (mark) (point))))
       (forward-char n) ;; Move forward so we find new occurrence
       (when (search-forward s limit t n)
     	 (set-mark (match-beginning 0))
     	 (goto-char (match-end 0))
	 t)))
   :matches (lambda () t)))

(defun keyano--object-command (object)
  "Execute an object command with the given OBJECT."
  ;; (goto-char (region-beginning-or-point))
  ;; (funcall (object-find object) 1)
  (setq keyano--current-object object))

(defun keyano-char ()
  "Command representing a char object."
  (interactive)
  (keyano--object-command char-object))

(cmd-to-run-for-all 'keyano-char)

(defun keyano-word ()
  "Command representing a word object."
  (interactive)
  (keyano--object-command word-object))

(defun keyano-number ()
  "Command representing a number object."
  (interactive)
  (keyano--object-command number-object))

(cmd-to-run-for-all 'keyano-word)

(defun keyano-line ()
  "Command representing a line object."
  (interactive)
  (keyano--object-command line-object))

(cmd-to-run-for-all 'keyano-line)

(defun keyano-parentheses ()
  "Command representing a line object."
  (interactive)
  (keyano--object-command parentheses-object))

(cmd-to-run-for-all 'keyano-parentheses)

(defun keyano-current-selection ()
  "Command representing an objet equal to the current selection."
  (interactive)
  (keyano--object-command current-selection-object))

(cmd-to-run-for-all 'keyano-current-selection)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selectors

(defun keyano-next (&optional arg)
  "Move to the ARGth occurrence of the current object.
Starts from the beginning of the selection."
  (interactive "p")
  (when (not arg) (setq arg 1))
  (seq-let (orig-from orig-to) (keyano--selection)
    ;; Make sure that point is before point so the search starts inside the
    ;; selection. But, do not remove the point since `current-selection-object'
    ;; uses it.
    (when (< (mark) (point)) (exchange-point-and-mark))
    (when (and (funcall (object-find (keyano--object)) arg)
               (= orig-from (region-beginning-or-point))
	       (= orig-to (region-end-or-point)))
      (goto-char (+ (region-beginning-or-point) (if (< arg 0) -1 1)))
      (funcall (object-find (keyano--object)) arg))))

(cmd-to-run-for-all 'keyano-next)

(defun keyano-next-after (&optional n)
  "Move to the Nth occurrence of the current object after the selection."
  (interactive "p")
  (goto-char (region-end-or-point))
  (funcall (object-find (keyano--object)) (if n n 1)))

(cmd-to-run-for-all 'keyano-next-after)

(defun keyano-transpose-next (&optional n)
  "Transpose the current selection with the next object.
Transposes with Nth occurrence of the current object after the selection."
  (interactive "p")
  (seq-let (a b) (keyano--selection)
    (keyano-next-after n)
    (seq-let (c d) (keyano--selection)
      (transpose-regions a b c d)
      (setq deactivate-mark nil)
      (set-mark (- d (- b a))))))

(cmd-to-run-for-all 'keyano-transpose-next)

(defun keyano-previous (&optional arg)
  "Move backwards to the ARGth occurrence of the current object.
Starts from the beginning of the selection."
  (interactive "p")
  (keyano-next (if arg (- arg) -1)))

(cmd-to-run-for-all 'keyano-previous)

(defun keyano-expand ()
  "Expand the current selection."
  (interactive)
  (funcall (object-expand (keyano--object))))

(cmd-to-run-for-all 'keyano-expand)

(defun keyano-expand-backward ()
  "Expand the selection to the previous start of the current object."
  (interactive)
  (let ((to (region-end-or-point)))
    (goto-char (region-beginning-or-point))
    (set-mark to)
    (funcall (object-find-start (keyano--object)) -1)))

(cmd-to-run-for-all 'keyano-expand-backward)

(defun keyano-expand-forward ()
  "Expand the selection to the next end of the current object."
  (interactive)
  (let ((from (region-beginning-or-point)))
    (goto-char (region-end-or-point))
    (set-mark from)
    (funcall (object-find-end (keyano--object)) 1)))

(cmd-to-run-for-all 'keyano-expand-forward)

;; This command is probably not very useful as it is very close in behavior to
;; `keyano-expand-forward'.
(defun keyano-include-next ()
  "Grow the selection to include the next occurrence of the curent object."
  (interactive)
  (let ((from (region-beginning-or-point)))
    (keyano-next-after)
    (set-mark from)))

(cmd-to-run-for-all 'keyano-include-next)

(defun keyano-add-next ()
  "Select the next occurrence of the current object."
  (interactive)
  (mc/create-fake-cursor-at-point)
  (keyano-next-after)
  (mc/maybe-multiple-cursors-mode))

(cmd-to-run-once 'keyano-add-next)

(defun keyano-add-previous ()
  "Select the next occurrence of the current object."
  (interactive)
  (mc/create-fake-cursor-at-point)
  (keyano-previous)
  (mc/maybe-multiple-cursors-mode))

(cmd-to-run-once 'keyano-add-previous)

(defun keyano-above (from to)
  "Move the current selection below itself NTH lines down.
Operates on FROM to TO."
  (interactive "r")
  (let ((n (+ from (/ (- to from) 2))))
    (set-mark nil)
    (goto-char n)
    (previous-line)
    (keyano-expand)))

(cmd-to-run-for-all 'keyano-above)

(defun keyano-below (from to)
  "Move the current selection below itself NTH lines down.
Operates on FROM to TO."
  (interactive "r")
  (let ((n (+ from (/ (- to from) 2))))
    (set-mark nil)
    (goto-char n)
    (next-line)
    (keyano-expand)))

(cmd-to-run-for-all 'keyano-below)

(defun keyano-find (str)
  "Find STR."
  (interactive "sSearch: ")
  (keyano--object-command (keyano--create-constant-object str)))

(defvar keyano-command-mode-map (make-sparse-keymap)
  "Keymap used for command mode.")

;; Top row

(define-key keyano-command-mode-map (kbd "1") 'digit-argument)
(define-key keyano-command-mode-map (kbd "2") 'digit-argument)
(define-key keyano-command-mode-map (kbd "3") 'digit-argument)
(define-key keyano-command-mode-map (kbd "4") 'digit-argument)
(define-key keyano-command-mode-map (kbd "5") 'digit-argument)
(define-key keyano-command-mode-map (kbd "6") 'digit-argument)
(define-key keyano-command-mode-map (kbd "7") 'digit-argument)
(define-key keyano-command-mode-map (kbd "8") 'digit-argument)
(define-key keyano-command-mode-map (kbd "9") 'digit-argument)
(define-key keyano-command-mode-map (kbd "-") 'negative-argument)

(define-key keyano-command-mode-map (kbd "*") 'keyano-current-selection)

;; Left-hand side

(define-key keyano-command-mode-map "p" 'keyano-parentheses)
(define-key keyano-command-mode-map "w" 'keyano-number)
(define-key keyano-command-mode-map "g" 'keyano-transpose-next)

(define-key keyano-command-mode-map "a" 'keyano-char)
(define-key keyano-command-mode-map "r" 'keyano-word)
(define-key keyano-command-mode-map "s" 'keyano-line)
(define-key keyano-command-mode-map "t" 'keyano-change)

(define-key keyano-command-mode-map "z" 'undo)
(define-key keyano-command-mode-map "x" 'keyano-kill)
(define-key keyano-command-mode-map "c" 'kill-ring-save)
(define-key keyano-command-mode-map "v" 'yank)

;; Right-hand side

(define-key keyano-command-mode-map "l" 'keyano-insert-left)
(define-key keyano-command-mode-map "u" 'keyano-above)
(define-key keyano-command-mode-map "y" 'keyano-insert-right)
(define-key keyano-command-mode-map ";" 'comment-or-uncomment-region)

(define-key keyano-command-mode-map "n" 'keyano-previous)
(define-key keyano-command-mode-map (kbd "C-n") 'keyano-previous)
(define-key keyano-command-mode-map "N" 'keyano-expand-backward)
(define-key keyano-command-mode-map "e" 'keyano-below)
(define-key keyano-command-mode-map "i" 'keyano-next)
(define-key keyano-command-mode-map "I" 'keyano-expand-forward)
(define-key keyano-command-mode-map (kbd "C-i") 'keyano-next-after)
(define-key keyano-command-mode-map (kbd "M-i") 'keyano-add-next)

(define-key keyano-command-mode-map "k" 'keyano-all-in)
(define-key keyano-command-mode-map "," 'keyano-expand)
(define-key keyano-command-mode-map "/" 'keyano-find)

(define-key keyano-command-mode-map (kbd "C->") 'mc/mark-next-like-this)

(defun keyano-all-in ()
  "Select all of the current object in the current selection."
  (interactive)
  (let ((to (region-end-or-point))
	(find (object-find (keyano--object))))
    (goto-char (region-beginning-or-point))
    (while (funcall find 1 to)
      (mc/create-fake-cursor-at-point))
      (mc/maybe-multiple-cursors-mode)))

(cmd-to-run-for-all 'keyano-all-in)

(defun keyano-command-to-insert ()
  "Switch from insert state to command state."
  (keyano-command-mode -1)
  (keyano-insert-mode 1))

(defun keyano-insert-to-command ()
  "Switch from insert state to command state."
  (interactive)
  (keyano-insert-mode -1)
  (keyano-command-mode 1))

(cmd-to-run-once 'keyano-insert-to-command)

(defun keyano-insert-left ()
  "Switch from insert state to command state.
Place the cursor at the left side of the region."
  (interactive)
  (goto-char (region-beginning-or-point))
  (keyano-command-to-insert))

(cmd-to-run-for-all 'keyano-insert-left)

(defun keyano-insert-right ()
  "Switch from insert state to command state.
Place the cursor at the right side of the region."
  (interactive)
  (goto-char (region-end-or-point))
  (keyano-command-to-insert))

(cmd-to-run-for-all 'keyano-insert-right)

(defun keyano-kill ()
  "Kill the current region."
  (interactive)
  (let ((r (object-kill-range (keyano--object))))
    (when r (funcall r))
    (kill-region 0 0 t)))

(cmd-to-run-for-all 'keyano-kill)

(defun keyano-change ()
  "Kill the current selections and enters insert state."
  (interactive)
  (kill-region 0 0 t)
  (keyano-command-to-insert))

(cmd-to-run-for-all 'keyano-change)

(define-minor-mode keyano-command-mode
  "Keyano command mode"
  :lighter " cmd"
  :keymap keyano-command-mode-map
  (delete-selection-mode 1)
  (setq cursor-type 'box))

(define-minor-mode keyano-insert-mode
  "Keyano insert mode"
  :lighter " ins"
  :keymap
  `((,(kbd "C-n") . keyano-insert-to-command)
    ([escape] . keyano-insert-to-command))
  (setq cursor-type 'bar)
  (deactivate-mark))

(define-globalized-minor-mode keyano-mode keyano-command-mode
  (lambda ()
    (if (minibufferp)
	(keyano-insert-mode t)
      (keyano-command-mode t))))

(provide 'keyano)

;;; keyano.el ends here
