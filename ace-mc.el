;;; ace-mc.el --- Ace Jump - Multiple Cursors

;; Copyright (C) 2015 Josh Moller-Mara

;; Author: Josh Moller-Mara <jmm@cns.nyu.edu>
;; Version: 1.0
;; Package-Requires ((ace-jump-mode "1.0") (multiple-cursors "1.0"))
;; Keywords: motion, location, cursor
;; URL: https://github.com/mm--/ace-mc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ace-mc.el is a package that allows you to add a Multiple Cursors
;; mode cursor using ace-jump.

;; This package adds the command `ace-mc/add-multiple-cursors', which
;; acts like `ace-jump-mode'. It will continue to keep prompting for
;; places to add cursors until you hit Enter.
;;
;; You can use the command `ace-mc/add-single-cursor' for a non-looping
;; version of `ace-mc/add-multiple-cursors'.

;; If you have ace-jump bound on C-0, for example, I recommend the
;; following key bindings:
;;
;; (global-set-key (kbd "C-)") 'ace-mc/add-multiple-cursors)
;; (global-set-key (kbd "C-M-)") 'ace-mc/add-single-cursor)

;;; Code:
(require 'ace-jump-mode)
(require 'multiple-cursors-core)
(require 'dash)

(defvar ace-mc/marking nil
  "Internal flag for detecting if currently marking.")

(defvar ace-mc/keyboard-reset nil
  "See if we've quit out yet.")

(defvar ace-mc/query-char nil
  "Char.")

(defvar ace-mc/loop-marking nil
  "Keep adding until we quit.")

(defvar ace-mc/saved-point nil
  "The position of our cursor before jumping around with ace-jump.")

;; TODO: Fix description
(defvar ace-mc/ace-mode-function nil
  "The function from `ace-jump-mode-submode-list` to use.")

(defun ace-mc/maybe-jump-start ()
  "Push the mark when marking with `ace-jump-char-mode'."
  (when ace-mc/marking
    (setq ace-mc/saved-point (point)
	  ace-mc/keyboard-reset nil)))

(defun ace-mc/maybe-jump-end ()
  "Zap after marking with `ace-jump-char-mode.'."
  (if (not ace-mc/marking)
      (ace-mc/reset)
    (let ((ace-mc/fake-cursor-at-point (-filter 'mc/fake-cursor-p (overlays-at (point)))))
      (if ace-mc/fake-cursor-at-point
	  (mc/remove-fake-cursor (car ace-mc/fake-cursor-at-point))
	(unless (equal ace-mc/saved-point (point))
	  (mc/create-fake-cursor-at-point))))
    (mc/maybe-multiple-cursors-mode)
    (when ace-mc/saved-point
      (goto-char ace-mc/saved-point))
    (if (and ace-mc/loop-marking (not ace-mc/keyboard-reset))
	(ace-mc/add-char ace-mc/query-char)
      (ace-mc/reset))))

(add-hook 'ace-jump-mode-before-jump-hook #'ace-mc/maybe-jump-start)

(add-hook 'ace-jump-mode-end-hook #'ace-mc/maybe-jump-end)

(defun ace-mc/reset ()
  "Reset the internal zapping variable flags."
  (setq ace-mc/marking nil))

(defun ace-mc/keyboard-reset ()
  "Reset when `ace-jump-mode' is cancelled.
Also called when chosen character isn't found while zapping."
  (interactive)
  (ace-mc/reset)
  (ace-jump-done))

;;;###autoload
(defun ace-mc/add-multiple-cursors (&optional prefix single-mode)
  "Use AceJump to add or remove multiple cursors.

ace-mc/add-multiple-cursors will prompt your for locations to
add multiple cursors.  If a cursor already exists at that
location, it will be removed.  This process continues looping
until you exit, for example by pressing return or escape. This
happens unless SINGLE-MODE is set to 't'.

Without a \\[universal-argument] prefix argument, use the default
AceJump jumping mode as described in
`ace-jump-mode-submode-list'.  When called interactively with one
or more \\[universal-argument] prefix arguments PREFIX, use the
corresponding mode from `ace-jump-mode-submode-list'.  For
example, by default
   \\[ace-mc/add-multiple-cursors] ==> ace-jump-word-mode
   \\[universal-argument] \\[ace-mc/add-multiple-cursors] ==> ace-jump-char-mode
   \\[universal-argument] \\[universal-argument] \\[ace-mc/add-multiple-cursors] ==> ace-jump-line-mode

When the region is active, prompt for AceJump matches based on matching strings."
  (interactive "pi")
  (let* ((index (/ (logb prefix) 2))
	(submode-list-length (length ace-jump-mode-submode-list)))
    (setq ace-mc/loop-marking (not single-mode))
    (if (< index 0)
        (setq index 0))
    (if (>= index submode-list-length)
        (setq index submode-list-length))
    (setq ace-mc/ace-mode-function (if (use-region-p)
				     'ace-mc/regexp-mode
				   (nth index ace-jump-mode-submode-list)))
    ;; Sometimes we want to go to different characters. Gets reset with movement.
    ;; TODO: Fix coding convention violation. Accessing a private function. :/
    (mc--reset-read-prompts)
    (if (use-region-p)
	(progn
	  (when (> (point) (mark))
	    (exchange-point-and-mark)
	    (mc/execute-command-for-all-fake-cursors 'exchange-point-and-mark))
	    (deactivate-mark)
	  (ace-mc/add-char (buffer-substring-no-properties (mark) (point))))
      (ace-mc/add-char (unless (eq ace-mc/ace-mode-function 'ace-jump-line-mode)
		       (read-char "Query Char:"))))))

;;;###autoload
(defun ace-mc/add-single-cursor (&optional prefix)
    "This is a wrapper for `ace-mc/add-multiple-cursors', only adding a single cursor.

PREFIX is passed to `ace-mc/add-multiple-cursors', see the documentation there."
    (interactive "p")
    (ace-mc/add-multiple-cursors prefix t))

(defun ace-mc/regexp-mode (regex)
  "Ace Jump Multiple Cursor with a REGEX."
  (ace-jump-do (regexp-quote regex)))

(defun ace-mc/add-char (query-char)
  "Call `ace-jump-char-mode' with a character QUERY-CHAR and add a cursor at the point."
  (let ((ace-jump-mode-scope 'window))
    (setq ace-mc/marking t
	  ace-mc/query-char query-char)
    (if query-char
	(funcall ace-mc/ace-mode-function query-char)
      (funcall ace-mc/ace-mode-function))
    (when overriding-local-map
      (define-key overriding-local-map [t] 'ace-mc/keyboard-reset))))

;; Prevent keyboard-reset from being added to mc-list
;; mc/cmds-to-run-once
;; Use ace-jump-do when the region is active.

(mapc (lambda (el) (add-to-list 'mc/cmds-to-run-once el))
      '(ace-mc/add-char
	ace-mc/keyboard-reset
	ace-mc/add-multiple-cursors
	ace-mc/add-single-cursor
	ace-jump-move))

(provide 'ace-mc)
;;; ace-mc.el ends here
