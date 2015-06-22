;;; nim-sigs.el --- Call signatures and 'thing-at-point' thins for nim-mode
;;; Code:

(require 'cl-lib)
(require 'nim-mode)


;; call signature
(defun nim-call-signature ()
  "Return call signature of current function for context at point."
  (interactive)
  (when (derived-mode-p 'nim-mode)
    (nim-call-epc 'def
                  (lambda (sigs)
                    (when sigs
                      (nim-call-signature--format-minibuffer sigs))))))


(defun nim-call-signature--propertize-params (params)
  "Formats single params"
  (mapcar
   (lambda (x) (let* ((spl2 (split-string x ": "))
                      (name (propertize (car spl2) 'face 'default))
                      (type (propertize (cadr spl2) 'face 'font-lock-type-face)))
                 (format "%s: %s" name type)))
   (split-string params ", ")))

(defun nim-call-signature--format-minibuffer-single (nim-type sig)
  "Formats single sig details to use in minibuffer"
  ;; TODO: apply nim-mode rules
  (cond ((or (string-equal nim-type "skMethod")
             (string-equal nim-type "skProc"))
         (let* ((sig-string (nim-epc-forth sig))
                (params (substring sig-string (+ 1 (search "(" sig-string)) (search ")" sig-string))))
           (format "(%s)"
                   (mapconcat 'identity (nim-call-signature--propertize-params params) ", "))))
        (t "")))


(defun nim-call-signature--format-minibuffer-name (nim-type sig)
  "Formats single sig name to use in minibuffer"
  (format "%s: %s"
   (substring nim-type 2)
   (cond ((or (string-equal nim-type "skMethod")
              (string-equal nim-type "skProc"))
          (propertize (cadr (nim-epc-qualifiedPath sig)) 'face 'font-lock-function-name-face))
         ((string-equal nim-type "skType")
          (propertize (nim-epc-forth sig) 'face 'font-lock-type-face))
         (t (nim-epc-forth sig)))))


(defun nim-call-signature--format-minibuffer (sigs)
  "Format callsignatures in minibuffer."
  (let* ((sig (first sigs))
         (nim-type (nim-epc-symkind sig))
         (name (nim-call-signature--format-minibuffer-name nim-type sig))
         (props (mapcar (lambda (x) (nim-call-signature--format-minibuffer-single nim-type x)) sigs)))
    (message "%s %s" name (mapconcat 'identity props " | "))))

(defcustom nim-get-call-signatures-delay 1
  "How long Nim-mode should wait before showing call signature")

(defvar nim-call-signatures-timer nil
  "A variable where nim-mode keeps timer for signatures")

(defun nim-enable-call-signatures ()
  "Enable call signatures"
  (when nim-call-signatures-timer
    (cancel-timer nim-call-signatures-timer))
  (setq nim-call-signatures-timer
        (run-with-idle-timer nim-get-call-signatures-delay t 'nim-call-signature)))
