;;; gorepl.el --- Go REPL Interactive Development in top of Gore -*- lexical-binding: t -*-

;; Copyright © 2015-2016 Manuel Alonso

;; Author: Manuel Alonso <manuteali@gmail.com>
;; Maintainer: Manuel Alonso <manuteali@gmail.com>
;; URL: http://www.github.com/manute/gorepl-mode
;; Version: 0.1.0
;; Package-Requires: ()
;; Keywords: languages, go, golang, gorepl

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;;
;; This library provides a Go repl interactive development environment for Emacs, built on
;; top of Gore (https://github.com/motemen/gore).
;;
;;; Code:

(defgroup gorepl nil
  "GO repl interactive"
  :prefix "gorepl-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/manute/gorepl-mode")
  :link '(emacs-commentary-link :tag "Commentary" "gorepl"))

(defcustom gorepl-command
  "gore"
  "The command used to execute gore."
  :type 'string
  :group 'gorepl)

(defconst gorepl-version "0.1.0-snapshot")
(defconst gorepl-buffer "*Go REPL*")
(defconst gorepl-buffer-name "Go REPL")


(defun gorepl--send-string (s)
  "Send string to gore repl process and return the output."
  (with-current-buffer (get-buffer gorepl-buffer)
    ;;insert into gorepl as comment for visual representation
    (insert (concat "/*" s "*/"))
    (comint-send-string (get-buffer-process gorepl-buffer) s)
    (comint-send-input)))

;; MANY THANKS to masteringenmacs for this:
;; https://www.masteringemacs.org/article/comint-writing-command-interpreter
(defun gorepl--run-gore (args)
  "Run an inferior instance of `gore' inside Emacs."
  (let* ((buffer (comint-check-proc gorepl-buffer-name)))
    ;; pop to the "*GO REPL Buffer*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer
     (if (or buffer (not (derived-mode-p 'gorepl-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer gorepl-buffer))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer gorepl-buffer-name buffer
             gorepl-command nil args)
      (gorepl-mode))))

;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;

(defun gorepl-version ()
  "Display GOREPL's version."
  (interactive)
  (message "GOREPL %s" gorepl-version))

(defun gorepl-run ()
  (interactive)
  (gorepl--run-gore '()))

(defun gorepl-run-load-current-file ()
  (interactive)
  (gorepl--run-gore (list "-context" (buffer-file-name))))

(defun gorepl-eval-selected-region ()
  "Eval the selected region in the gorepl."
  (interactive)
  (gorepl--send-string (filter-buffer-substring (region-beginning)
                                                (region-end))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE MINOR MODE
;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-minor-mode gorepl-mode
  "A minor mode for run a go repl"
  :lighter " gorepl"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-g") #'gorepl-run)
            (define-key map (kbd "C-c C-l") #'gorepl-run-load-current-file)
            (define-key map (kbd "C-x C-e") #'gorepl-eval-selected-region)
            map))


(provide 'gorepl)
