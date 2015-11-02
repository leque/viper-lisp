;;; viper-lisp.el --- Commands for editing LISP in Viper  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015 OOHASHI Daichi

;; Author: OOHASHI Daichi <dico.leque.comicron@gmail.com>
;; Keywords: emulations
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4"))

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

;;; Code:
(require 'cl-lib)
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

;;;###autoload
(define-minor-mode viper-lisp-mode
  nil
  :init-value nil
  (when viper-lisp-mode
    (add-hook 'change-major-mode-hook
              #'disable-viper-lisp-mode
              nil
              t))
  (mapc #'(lambda (lis)
            (cl-destructuring-bind (key f g) lis
              (define-key viper-vi-local-user-map key
                (if viper-lisp-mode f g))))
        `(("(" ,#'viper-lisp-backward-sexp ,#'viper-backward-sentence)
          (")" ,#'viper-lisp-forward-sexp ,#'viper-forward-sentence)
          ("{" ,#'viper-lisp-backward-list ,#'viper-backward-paragraph)
          ("}" ,#'viper-lisp-forward-list ,#'viper-forward-paragraph)
          ("[" ,#'viper-lisp-brac ,#'viper-brac-function)
          ("]" ,#'viper-lisp-ket ,#'viper-ket-function))))

;;;###autoload
(defun enable-viper-lisp-mode ()
  "Turn on `viper-lisp-mode'"
  (interactive)
  (viper-lisp-mode +1))

;;;###autoload
(defun disable-viper-lisp-mode ()
  "Turn off `viper-lisp-mode'"
  (interactive)
  (viper-lisp-mode -1))

(define-viper-lisp-brac-function ?\[ #'viper-lisp-prev-defun)
(define-viper-lisp-ket-function ?\] #'viper-lisp-next-defun)

(provide 'viper-lisp)
;;; viper-lisp.el ends here
