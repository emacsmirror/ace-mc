;;; ace-mc.el --- Ace Jump - Multiple Cursors

;; Copyright (C) 2015 Josh Moller-Mara

;; Author: Josh Moller-Mara <jmm@cns.nyu.edu>
;; Version: 1.0
;; Package-Requires ((ace-jump-mode "1.0") (multiple-cursors "1.0"))
;; Keywords: motion, location, cursor
;; URL: https://github.com/mm--/ace-jump-multiple-cursors

;;; Commentary:

;; ace-mc.el is a package that allows you to add a Multiple Cursors
;; mode cursor using ace-jump.

;; This package adds the command `ace-jump-add-multiple-cursor', which
;; acts like `ace-jump-mode'. It will continue to keep prompting for
;; places to add cursors until you hit Enter.

;;; Code:
(require 'ace-jump-mode)
(require 'multiple-cursors-core)
(require 'dash)

(defcustom ajmc/always-loop nil
  "Non-nil if `ace-jump-add-multiple-cursor' automatically loops without a prefix."
  :type '(boolean)
  :group 'ace-jump-mc)

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
  (message "CANCELLED!")
  (ace-jump-done))

;; Rename this?
;; ace-jump-mc/mark?
(defun ace-jump-add-multiple-cursor (&optional prefix)
  "Use AceJump to add or remove multiple cursors.

ace-jump-add-multiple-cursor will prompt your for locations to
add multiple cursors.  If a cursor already exists at that
location, it will be removed.  This process continues looping
until you exit, for example by pressing return or escape.  You can
customize the variable `ajmc/always-loop' to toggle this behavior.

Without a \\[universal-argument] prefix argument, use the default
AceJump jumping mode as described in
`ace-jump-mode-submode-list'.  When called interactively with one
or more \\[universal-argument] prefix arguments PREFIX, use the
corresponding mode from `ace-jump-mode-submode-list'.  For
example, by default
   \\[ace-jump-add-multiple-cursor] ==> ace-jump-word-mode
   \\[universal-argument] \\[ace-jump-add-multiple-cursor] ==> ace-jump-char-mode
   \\[universal-argument] \\[universal-argument] \\[ace-jump-add-multiple-cursor] ==> ace-jump-line-mode

When the region is active, prompt for AceJump matches based on matching strings."
  (interactive "p")
  (let* ((preindex (/ (logb prefix) 2))
	 (index (- preindex (if ajmc/always-loop 0 1)))
	(submode-list-length (length ace-jump-mode-submode-list)))
    (setq ajmc/loop-marking (or ajmc/always-loop (use-region-p) (> prefix 1)))
    (if (< index 0)
        (setq index 0))
    (if (>= index submode-list-length)
        (setq index submode-list-length))
    (setq ajmc/ace-mode-function (if (use-region-p)
				     'ajmc/regexp-mode
				   (nth index ace-jump-mode-submode-list)))
    (mc--reset-read-prompts)		;Sometimes we want to go to different characters. Gets reset with movement.
    (if (use-region-p)
	(progn
	  (when (> (point) (mark))
	    (exchange-point-and-mark)
	    (mc/execute-command-for-all-fake-cursors 'exchange-point-and-mark))
	    (deactivate-mark)
	  (ajmc/add-char (buffer-substring-no-properties (mark) (point))))
      (ajmc/add-char (unless (eq ajmc/ace-mode-function 'ace-jump-line-mode)
		       (read-char "Query Char:"))))))

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
      '(ace-jump-add-multiple-cursor
	ajmc/add-char
	ajmc/keyboard-reset
	ace-jump-move))

;; TODO: Add advice to show cursors? But the bad thing here is that
;; with looping, removing a cursor will shift the letter combinations for the others.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TEST

;; (defun ajmc/get-cursor-positions ()
;;   (loop for va in (ace-jump-list-visual-area)
;; 		  append (let* ((current-window (aj-visual-area-window va))
;; 				(start-point (window-start current-window))
;; 				(end-point   (window-end   current-window t)))
;; 			   (with-selected-window current-window
;; 			     (save-excursion
;; 			       (loop for m in (mc/all-fake-cursors)
;; 				     if (and (>= (overlay-start m) start-point)
;; 					     (<= (overlay-start m) end-point))
;; 				     collect (make-aj-position :offset (overlay-start m)
;; 							       :visual-area va))
;; 			       )))))

;; (defun ajmc/add-cursors-to-candidates (orig-fun &rest args)
;;   (delete-dups (append (apply orig-fun args)
;; 		       (when ajmc/marking ajmc/list-cursors))))


;; (defun ajmc/add-char (query-char)
;;   "Call `ace-jump-char-mode' with a character QUERY-CHAR and add a cursor at the point."
;;   (let ((ace-jump-mode-scope 'window))
;;     (unless ajmc/marking		;We're already marking
;;       (setq ajmc/list-cursors (ajmc/get-cursor-positions)))
;;     (setq ajmc/marking t
;; 	  ajmc/query-char query-char)
;;     (if query-char
;; 	(funcall ajmc/ace-mode-function query-char)
;;       (funcall ajmc/ace-mode-function))
;;     (when overriding-local-map
;;       (define-key overriding-local-map [t] 'ajmc/keyboard-reset))))

;; (defun ajmc/maybe-jump-start ()
;;   "Push the mark when marking with `ace-jump-char-mode'."
;;   (when ajmc/marking
;;     (setq ajmc/saved-point (point)
;; 	  ajmc/keyboard-reset nil)))


;; (advice-add 'ace-jump-search-candidate :around #'ajmc/add-cursors-to-candidates)
;; (advice-del 'ace-jump-search-candidate :around #'ajmc/add-cursors-to-candidates)

;; (interactive (list (read-char "Query Char:")))

;;; ace-mc.el ends here
