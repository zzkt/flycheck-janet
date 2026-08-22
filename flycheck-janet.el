;;; flycheck-janet.el --- Flycheck support for Janet -*- lexical-binding: t; -*-

;; Copyright (C) 2026 FoAM oü

;; Author: sogaiu
;;         nik gaffney <nik@fo.am>
;; Maintainer: nik gaffney <nik@fo.am>
;; Created: 24 August 2020
;; Version: 2026.08.21
;; Homepage: https://codeberg.org/zzkt/flycheck-janet
;; Package-Requires: ((emacs "26.1") (flycheck "0.18"))


;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This package integrates janet with Emacs via flycheck.  To use it,
;; add to your .emacs or equivalent (e.g. init.el):
;;
;;   (require 'flycheck-janet)
;;
;; likely something like:
;;
;;   (global-flycheck-mode)
;;
;; will be necessary too.

;; Make sure the janet binary is on your path.


;; Miscellaneous

;; * See `'flycheck-redefine-standard-error-levels' for calls to
;;  `flycheck-define-error-level' which define the three levels:
;;  error, warning, and info.  Only warning and error are used atm in
;;  :error-patterns in this file.

;; * The checkers in this file operate on stdin content so one may end
;;   up seeing `stdin' as the file name in many cases.

;; * Note that the linter reports issues with files that are not the
;;   "current file" as well.  These don't appear to show up via "Show
;;   all errors".  it may be that flycheck filters these out via
;;   `flycheck-relevant-error-p'.


;;; Code:
(require 'flycheck)

(defun flycheck-janet--debug-error-filter (errors)
  "Debugging function for observing flycheck error objects.

Takes a single argument `ERRORS' of flycheck error objects.
Use with `:error-filter' portion of `flycheck-define-checker'.

At the moment, for each flycheck error object, it dumps all of the
flycheck error objects and reports filename, message, and/or level for
each if these are defined."
  (message "%S" errors)
  (dolist (err errors)
    (when (flycheck-error-filename err)
      (message "filename: %s" (flycheck-error-filename err)))
    (when (flycheck-error-message err)
      (message "message: %s" (flycheck-error-message err)))
    (when (flycheck-error-level err)
      (message "level: %s" (flycheck-error-level err))))
  errors)

(defun flycheck-janet-error-filter (errors)
  "Error filter for janet checkers.

Takes a single argument `ERRORS' of flycheck error objects.
Use with `:error-filter' portion of `flycheck-define-checker'.

It massages file names and normalizes error positions.

Janet reports many diagnostics at the column of the enclosing form's
opening parenthesis, which are converted to explicit locations."
  (dolist (err errors)
    (when-let* ((fname (flycheck-error-filename err)))
      (if (and buffer-file-name (string-equal "stdin" fname))
          (setf (flycheck-error-filename err) buffer-file-name)
        (setf (flycheck-error-filename err) (expand-file-name fname))))
    ;; Normalize positions
    (let ((line (flycheck-error-line err))
          (column (flycheck-error-column err)))
      (when line
        ;; End-of-source parse errors are reported with column 0
        ;; flycheck columns are 1-based.
        (when (and column (< column 1))
          (setf (flycheck-error-column err) 1)
          (setq column 1))

        ;; End-of-source diagnostics can land on an empty trailing
        ;; line of the buffer. Move such errors onto preceding line.
        (when (> line 1)
          (save-excursion
            (goto-char (point-max))
            (when (and (bolp) (= (point-max) (line-beginning-position)))
              (let ((empty-last-line (line-number-at-pos (point-max))))
                (when (and (> empty-last-line 1) (>= line empty-last-line))
                  ;; (setq line (1- empty-last-line))
                  (setq line empty-last-line)
                  (setf (flycheck-error-line err) line))))))

        ;; Provide explicit zero-width end positions.
        (unless (flycheck-error-end-line err)
          (setf (flycheck-error-end-line err) line))
        (unless (flycheck-error-end-column err)
          (setf (flycheck-error-end-column err) (max 1 (or column 1)))))))
  errors)

(defvar flycheck-janet-error-patterns
  '((warning line-start
             ;; XXX: gets fooled by "error: stdin"
             ;;(file-name)
             ;; better
             (file-name (one-or-more (not ":")))
             ":"
             line
             ":"
             column ": "
             (message)
             line-end)
    (error line-start
           "error: "
           ;; use via stdin causes the following to be stdin:
           (one-or-more (not ":")) ":"
           line
           ":"
           column ": "
           (message)
           line-end))
  "Error patterns for janet checkers.")


(flycheck-def-executable-var janet-janet "janet")
(flycheck-define-command-checker 'janet-janet
  "A checker for Janet using janet -n -k."
  :command '("janet" "-n" "-k")
  :standard-input t
  :error-filter #'flycheck-janet-error-filter
  :error-patterns flycheck-janet-error-patterns
  :modes '(janet-mode janet-ts-mode)
  :predicate (lambda ()
               (memq major-mode '(janet-mode
                                  janet-ts-mode))))

(flycheck-def-executable-var janet-relaxed "janet")
(flycheck-define-command-checker 'janet-relaxed
  "A checker for Janet using janet -n -k -w relaxed."
  :command '("janet" "-n" "-k" "-w" "relaxed")
  :standard-input t
  :error-filter #'flycheck-janet-error-filter
  :error-patterns flycheck-janet-error-patterns
  :modes '(janet-mode janet-ts-mode)
  :predicate (lambda ()
               (memq major-mode '(janet-mode
                                  janet-ts-mode))))

(flycheck-def-executable-var janet-normal "janet")
(flycheck-define-command-checker 'janet-normal
  "A checker for Janet using janet -n -k -w normal"
  :command '("janet" "-n" "-k" "-w" "normal")
  :standard-input t
  :error-filter #'flycheck-janet-error-filter
  :error-patterns flycheck-janet-error-patterns
  :modes '(janet-mode janet-ts-mode)
  :predicate (lambda ()
               (memq major-mode '(janet-mode
                                  janet-ts-mode))))

(flycheck-def-executable-var janet-strict "janet")
(flycheck-define-command-checker 'janet-strict
  "A checker for Janet using janet -n -k -w strict."
  :command '("janet" "-n" "-k" "-w" "strict")
  :standard-input t
  :error-filter #'flycheck-janet-error-filter
  :error-patterns flycheck-janet-error-patterns
  :modes '(janet-mode janet-ts-mode)
  :predicate (lambda ()
               (memq major-mode '(janet-mode
                                  janet-ts-mode))))

;; use a similar line in .emacs equivalent if one of the other
;; checkers, e.g. janet-relaxed, janet-normal, or janet-strict is
;; desired instead
(add-to-list 'flycheck-checkers 'janet-janet)

(provide 'flycheck-janet)
;;; flycheck-janet.el ends here
