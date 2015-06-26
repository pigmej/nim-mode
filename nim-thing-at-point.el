;;; nim-thing-at-point.el --- 'thing-at-point' for nim-mode
;;; Code:

(require 'cl-lib)
(require 'nim-mode)

(defun nim-thing-at-point ()
  "Return call signature of current function for context at point."
  (interactive)
  (when (and (derived-mode-p 'nim-mode) (not (string-equal (buffer-name) "*nim-thing-at-point*")))
    (nim-call-epc 'def
                  (lambda (sigs)
                    (when sigs
                      (nim-thing-at-point--format-minibuffer sigs))))))


(defun nim-thing-at-point--format-minibuffer (sigs)
  "Format callsignatures in minibuffer."
  (let ((info (mapcar (lambda (x) (format "%s: %s" (nim-epc-symkind x)(nim-thing-at-point-buffer x))) sigs)))
    (message (mapconcat 'identity info " | "))))


(defun nim-thing-at-point-buffer (sig)
  "Interts element to buffer to apply nim-mode color rules.
   then returns buffer-string"
  (let* ((element (nim-epc-forth sig))
         (buf (get-buffer-create "*nim-thing-at-point*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert element)
      (delay-mode-hooks (nim-mode))
      (font-lock-default-function 'nim-mode)
      (font-lock-default-fontify-buffer)
      (buffer-string))))


(defcustom nim-get-thing-at-point-delay 1
  "How long Nim-mode should wait before showing call signature")

(defvar nim-thing-at-point-timer nil
  "A variable where nim-mode keeps timer for signatures")

(defun nim-enable-thing-at-point ()
  "Starts timer for thing-at-point features "
  (interactive)
  (when nim-thing-at-point-timer
    (cancel-timer nim-thing-at-point-timer))
  (setq nim-thing-at-point-timer
        (run-with-idle-timer nim-get-thing-at-point-delay t 'nim-thing-at-point)))
