;;;; viper-lisp.el --- Commands for editing LISP in Viper
;;;
;;; Copyright (c) 2013 OOHASHI Daichi,
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;;
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; 3. Neither the name of the authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this
;;;    software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(eval-when-compile
  (require 'cl))

(require 'viper-cmd)

(defvar viper-lisp-brac-functions '())
(defvar viper-lisp-ket-functions '())

(defsubst viper-lisp-wrap-movement (arg name move &optional if-delete)
  (declare (indent 2))
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (funcall move val)
    (when (and (or (not com) (viper-char-equal com ?d))
               if-delete)
      (funcall if-delete))
    (when com
      (viper-execute-com name val com))))

(defmacro viper-lisp-eor (&rest exprs)
  (and exprs
      `(condition-case nil
           ,(car exprs)
         (error (viper-lisp-eor ,@(cdr exprs))))))

(defun viper-lisp-forward-sexp (arg)
  "Forward sexp."
  (interactive "P")
  (viper-lisp-wrap-movement arg 'viper-lisp-forward-sexp
    #'(lambda (val)
        (let ((p (point)))
          (viper-lisp-eor
           (dotimes (_ val) (forward-sexp 1))
           (or (/= p (point))
               (up-list 1))
           (goto-char (point-max)))))
    #'(lambda ()
        (viper-skip-separators t))))

(defun viper-lisp-backward-sexp (arg)
  "Backward sexp."
  (interactive "P")
  (viper-lisp-wrap-movement arg 'viper-lisp-backward-sexp
    #'(lambda (val)
        (viper-lisp-eor
         (backward-sexp val)
         (up-list -1)
         (goto-char (point-min))))))

(defun viper-lisp-forward-list (arg)
  "Forward list."
  (interactive "P")
  (viper-lisp-wrap-movement arg 'viper-lisp-forward-list
    #'(lambda (val)
        (viper-lisp-eor
         (forward-list val)
         (let ((p (point)))
           (ignore-errors
             (while t
               (forward-sexp 1)))
           (when (= (point) p)
             (up-list 1)))
         (goto-char (point-max))))
    #'(lambda ()
        (viper-skip-separators t))))

(defun viper-lisp-backward-list (arg)
  "Backward list."
  (interactive "P")
  (viper-lisp-wrap-movement arg 'viper-lisp-backward-list
    #'(lambda (val)
        (viper-lisp-eor
         (backward-list val)
         (up-list -1)
         (goto-char (point-min))))))

(defun viper-lisp-prev-defun (arg)
  (interactive "P")
  (viper-lisp-wrap-movement arg 'viper-lisp-prev-defun
    #'(lambda (val)
        (viper-lisp-eor
         (beginning-of-defun val)
         (goto-char (point-min))))))

(defun viper-lisp-next-defun (arg)
  (interactive "P")
  (viper-lisp-wrap-movement arg 'viper-lisp-next-defun
    #'(lambda (val)
        (viper-lisp-eor
         (end-of-defun val)
         (goto-char (point-max))))
    #'(lambda ()
        (viper-skip-separators t))))

(defun define-viper-lisp-brac-function (ch f)
  (declare (indent 1))
  (push (cons ch f) viper-lisp-brac-functions))

(defun define-viper-lisp-ket-function (ch f)
  (declare (indent 1))
  (push (cons ch f) viper-lisp-ket-functions))

(defsubst viper-lisp-bracket (arg alist fallback-fun)
  (let* ((reg (read-char))
         (f (assoc reg alist)))
    (if (and f (cdr f))
        (funcall (cdr f) arg)
      (push reg unread-command-events)
      (funcall fallback-fun arg))))

(defun viper-lisp-brac (arg)
  "Function called by \[, the brac."
  (interactive "P")
  (viper-lisp-bracket arg viper-lisp-brac-functions #'viper-brac-function))

(defun viper-lisp-ket (arg)
  "Function called by \], the ket."
  (interactive "P")
  (viper-lisp-bracket arg viper-lisp-ket-functions #'viper-ket-function))

(define-minor-mode viper-lisp-mode
  nil
  :init-value nil
  (when viper-lisp-mode
    (add-hook 'change-major-mode-hook
              #'disable-viper-lisp-mode
              nil
              t))
  (mapc #'(lambda (lis)
            (destructuring-bind (key f g) lis
              (define-key viper-vi-local-user-map key
                (if viper-lisp-mode f g))))
        `(("(" ,#'viper-lisp-backward-sexp ,#'viper-backward-sentence)
          (")" ,#'viper-lisp-forward-sexp ,#'viper-forward-sentence)
          ("{" ,#'viper-lisp-backward-list ,#'viper-backward-paragraph)
          ("}" ,#'viper-lisp-forward-list ,#'viper-forward-paragraph)
          ("[" ,#'viper-lisp-brac ,#'viper-brac-function)
          ("]" ,#'viper-lisp-ket ,#'viper-ket-function))))

(defun enable-viper-lisp-mode ()
  (interactive)
  (viper-lisp-mode +1))

(defun disable-viper-lisp-mode ()
  (interactive)
  (viper-lisp-mode -1))

(define-viper-lisp-brac-function ?\[ #'viper-lisp-prev-defun)
(define-viper-lisp-ket-function ?\] #'viper-lisp-next-defun)

(provide 'viper-lisp)
