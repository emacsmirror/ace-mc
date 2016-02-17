;;; ace-mc.el --- Ace Jump - Multiple Cursors

;; Copyright (C) 2015 Josh Moller-Mara

;; Author: Josh Moller-Mara <jmm@cns.nyu.edu>
;; Version: 1.0
;; Package-Requires ((ace-jump-mode "1.0") (multiple-cursors "1.0"))
;; Keywords: motion, location, cursor
;; URL: https://github.com/mm--/ace-jump-multiple-cursors

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

;; This package adds the command `ajmc/add-multiple-cursors', which
;; acts like `ace-jump-mode'. It will continue to keep prompting for
;; places to add cursors until you hit Enter.
;;
;; You can use the command `ajmc/add-single-cursor' for a non-looping
;; version of `ajmc/add-multiple-cursors'.

;; If you have ace-jump bound on C-0, for example, I recommend the
;; following key bindings:
;;
;; (global-set-key (kbd "C-)") 'ajmc/add-multiple-cursors)
;; (global-set-key (kbd "C-M-)") 'ajmc/add-single-cursor)

;;; Code:
(require 'ace-jump-mode)
(require 'multiple-cursors-core)
(require 'dash)

(defvar ajmc/marking nil
  "Internal flag for detecting if currently marking.")

(defvar ajmc/keyboard-reset nil
  "See if we've quit out yet.")

(defvar ajmc/query-char nil
  "Char.")

(defvar ajmc/loop-marking nil
  "Keep adding until we quit.")

(defvar ajmc/saved-point nil
  "The position of our cursor before jumping around with ace-jump.")

;; TODO: Fix description
(defvar ajmc/ace-mode-function nil
  "The function from `ace-jump-mode-submode-list` to use.")

(defun ajmc/maybe-jump-start ()
  "Push the mark when marking with `ace-jump-char-mode'."
  (when ajmc/marking
    (setq ajmc/saved-point (point)
	  ajmc/keyboard-reset nil)))

(defun ajmc/maybe-jump-end ()
  "Zap after marking with `ace-jump-char-mode.'."
  (if (not ajmc/marking)
      (ajmc/reset)
    (let ((ajmc/fake-cursor-at-point (-filter 'mc/fake-cursor-p (overlays-at (point)))))
      (if ajmc/fake-cursor-at-point
	  (mc/remove-fake-cursor (car ajmc/fake-cursor-at-point))
	(unless (equal ajmc/saved-point (point))
	  (mc/create-fake-cursor-at-point))))
    (mc/maybe-multiple-cursors-mode)
    (when ajmc/saved-point
      (goto-char ajmc/saved-point))
    (if (and ajmc/loop-marking (not ajmc/keyboard-reset))
	(ajmc/add-char ajmc/query-char)
      (ajmc/reset))))

(add-hook 'ace-jump-mode-before-jump-hook #'ajmc/maybe-jump-start)

(add-hook 'ace-jump-mode-end-hook #'ajmc/maybe-jump-end)

(defun ajmc/reset ()
  "Reset the internal zapping variable flags."
  (setq ajmc/marking nil))

(defun ajmc/keyboard-reset ()
  "Reset when `ace-jump-mode' is cancelled.
Also called when chosen character isn't found while zapping."
  (interactive)
  (ajmc/reset)
  (ace-jump-done))

;;;###autoload
(defun ajmc/add-multiple-cursors (&optional prefix single-mode)
  "Use AceJump to add or remove multiple cursors.

ajmc/add-multiple-cursors will prompt your for locations to
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
   \\[ajmc/add-multiple-cursors] ==> ace-jump-word-mode
   \\[universal-argument] \\[ajmc/add-multiple-cursors] ==> ace-jump-char-mode
   \\[universal-argument] \\[universal-argument] \\[ajmc/add-multiple-cursors] ==> ace-jump-line-mode

When the region is active, prompt for AceJump matches based on matching strings."
  (interactive "pi")
  (let* ((index (/ (logb prefix) 2))
	(submode-list-length (length ace-jump-mode-submode-list)))
    (setq ajmc/loop-marking (not single-mode))
    (if (< index 0)
        (setq index 0))
    (if (>= index submode-list-length)
        (setq index submode-list-length))
    (setq ajmc/ace-mode-function (if (use-region-p)
				     'ajmc/regexp-mode
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
	  (ajmc/add-char (buffer-substring-no-properties (mark) (point))))
      (ajmc/add-char (unless (eq ajmc/ace-mode-function 'ace-jump-line-mode)
		       (read-char "Query Char:"))))))

;;;###autoload
(defun ajmc/add-single-cursor (&optional prefix)
    "This is a wrapper for `ajmc/add-multiple-cursors', only adding a single cursor.

PREFIX is passed to `ajmc/add-multiple-cursors', see the documentation there."
    (interactive "p")
    (ajmc/add-multiple-cursors prefix t))

(defun ajmc/regexp-mode (regex)
  "Ace Jump Multiple Cursor with a REGEX."
  (ace-jump-do (regexp-quote regex)))

(defun ajmc/add-char (query-char)
  "Call `ace-jump-char-mode' with a character QUERY-CHAR and add a cursor at the point."
  (let ((ace-jump-mode-scope 'window))
    (setq ajmc/marking t
	  ajmc/query-char query-char)
    (if query-char
	(funcall ajmc/ace-mode-function query-char)
      (funcall ajmc/ace-mode-function))
    (when overriding-local-map
      (define-key overriding-local-map [t] 'ajmc/keyboard-reset))))

;; Prevent keyboard-reset from being added to mc-list
;; mc/cmds-to-run-once
;; Use ace-jump-do when the region is active.

(mapc (lambda (el) (add-to-list 'mc/cmds-to-run-once el))
      '(ajmc/add-char
	ajmc/keyboard-reset
	ajmc/add-multiple-cursors
	ajmc/add-single-cursor
	ace-jump-move))

(provide 'ace-mc)
;;; ace-mc.el ends here
