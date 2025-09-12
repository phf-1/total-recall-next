;;; total-recall-utils.el --- Utils functions for Total Recall -*- lexical-binding: t; coding: utf-8 -*-

;; :ID: 253f2cf4-3b7d-42a4-b841-1f6dbc52bdfe

;; Copyright (C) 2025 Pierre-Henry FRÖHRING

;; Author: Pierre-Henry FRÖHRING <contact@phfrohring.com>
;; Maintainer: Pierre-Henry FRÖHRING <contact@phfrohring.com>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This code defines Utils, a library of pure functions for Total Recall.

;;; Code:

;; Context

(require 'pp)
(require 'cl-lib)
(require 'cl-macs)
(require 'parse-time)
(require 'rx)

;; Interface

(defun total-recall--reduce-while (acc funcs)
  "Reduce ACC over FUNCS until a halt condition is met.
FUNCS is a list of functions, each taking ACC and returning a cons cell
with :cont or :halt and the next accumulator value."
  (let (step)
    (while funcs
      (setq step (car funcs))
      (setq funcs (cdr funcs))
      (pcase (funcall step acc)
        (`(:cont . ,next-acc) (setq acc next-acc))
        (`(:halt . ,next-acc)
         (setq funcs '())
         (setq acc next-acc))))
    acc))

(defun total-recall--satisfies-invariant-p (actor invariant)
  "Check if ACTOR satisfies INVARIANT.
ACTOR is a function, and INVARIANT is a keyword. Returns t if INVARIANT
is in the list returned by sending :invariants to ACTOR, nil otherwise."
  (unless (keywordp invariant)
    (error "INVARIANT is not a keyword. invariant = %s" invariant))
  (and (functionp actor)
       (condition-case nil
           (memq invariant (total-recall--send actor :invariants))
         (error
          nil))))

(defun total-recall--nat-p (any)
  "Return t if ANY is a non-negative integer, nil otherwise."
  (and (integerp any) (<= 0 any)))

(defun total-recall--epoch-p (any)
  "Return t if ANY is a valid epoch (non-negative integer), nil otherwise."
  (total-recall--nat-p any))

(defun total-recall--probability-p (any)
  "Return t if ANY is a number between 0 and 1 inclusive, nil otherwise."
  (and (numberp any) (<= 0 any) (<= any 1)))

(defun total-recall--list-of-p (any pred)
  "Return t if ANY is a list and all elements satisfy PRED, nil otherwise."
  (and (listp any) (cl-every pred any)))

(defun total-recall--send (actor message)
  "Send MESSAGE to ACTOR and return the result.
ACTOR must be a function, or an error is signaled."
  (unless (functionp actor)
    (error "ACTOR is not a function. actor = %s" actor))
  (funcall actor message))

(defun total-recall--pp (any)
  "Pretty-print ANY to a buffer named *PP* in `emacs-lisp-mode'."
  (with-current-buffer (get-buffer-create "*PP*")
    (erase-buffer)
    (insert (pp-to-string any))
    (emacs-lisp-mode)
    (display-buffer (current-buffer))))

(defun total-recall--ok-mk (value)
  "Return a cons cell with :ok and VALUE."
  (cons :ok value))

(defun total-recall--error-mk (value)
  "Return a cons cell with :error and VALUE."
  (cons :error value))

(defmacro total-recall--context (syms &rest body)
  "Bind SYMS to Total Recall functions and evaluate BODY.
Each symbol in SYMS is bound to a function named `total-recall--<sym>'."
  (declare (indent 1) (debug ((&rest symbolp) body)))
  `(cl-flet
       ,(mapcar
         (lambda (sym) `(,sym #',(intern (concat "total-recall--" (symbol-name sym)))))
         syms)
     ,@body))

(cl-defun total-recall--wait-until-process-run
    (process &optional (timeout 1) (delta 0.1))
  "Wait until PROCESS is live or TIMEOUT seconds elapse.
Check every DELTA seconds. Signal an error if PROCESS is not live after TIMEOUT."
  (while (and (<= 0 timeout)
              (not (eq (process-status process) 'run)))
    (sleep-for delta)
    (setq timeout (- timeout delta)))
  (when (< timeout 0)
    (error "PROCESS is not live. process = %s" process)))

(defun total-recall--epoch->iso8601 (epoch)
  "Convert EPOCH (a non-negative integer) to an ISO 8601 string."
  (format-time-string "%FT%TZ" epoch t))

(defun total-recall--iso8601->epoch (iso8601)
  "Convert ISO8601 string to an epoch (integer seconds since Unix epoch)."
  (time-convert (parse-iso8601-time-string iso8601) 'integer))

(defun total-recall--string-truncate (str)
  "Truncate STR to 25 characters, replacing newlines with spaces.
Return the truncated string with an ellipsis if necessary."
  (truncate-string-to-width
   (replace-regexp-in-string "\n" " " (string-trim str))
   25 0 nil "…"))

(defconst total-recall--uuid-re
  (rx
    (= 8 hex)
    "-"
    (= 4 hex)
    "-"
    (= 4 hex)
    "-"
    (= 4 hex)
    "-"
    (= 12 hex)))

(defconst total-recall--uuid-re-strict
  (rx bol (regex total-recall--uuid-re) eol))

(defun total-recall--uuid-p (any)
  "Check if ANY is a valid UUID string.
Returns t if ANY matches the UUID format, nil otherwise."
  (and (stringp any)
    (string-match-p total-recall--uuid-re-strict any)))

(provide 'total-recall-utils)

;;; total-recall-utils.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
