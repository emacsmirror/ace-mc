(require 'ace-jump-mode)
(require 'multiple-cursors-core)

(defvar ajmc/marking nil
  "Internal flag for detecting if currently marking.")

(defvar ajmc/keyboard-reset nil
  "See if we've quit out yet.")

(defvar ajmc/query-char nil
  "Char.")

(defvar ajmc/loop-marking nil
  "Keep adding until we quit.")

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
    (mc/create-fake-cursor-at-point)
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
  "Call `ace-jump-char-mode' and add a cursor at the point."
  (interactive "p")
  (let* ((preindex (/ (logb prefix) 2))
	 (index (- preindex 1))
	(submode-list-length (length ace-jump-mode-submode-list)))
    (setq ajmc/loop-marking (or (use-region-p) (> prefix 1)))
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
  "Ace Jump with a regexp"
  (ace-jump-do (regexp-quote regex)))
  
;; TODO: Shouldn't be interactive
(defun ajmc/add-char (query-char)
  "Call `ace-jump-char-mode' and add a cursor at the point."
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

;; (interactive (list (read-char "Query Char:")))
