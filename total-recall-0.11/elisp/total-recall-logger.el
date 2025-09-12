;;; total-recall-logger.el --- Logger  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: a0e557e8-87f8-4970-a423-adea9435f934

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

;; This code defines Logger, a type of objects that provides a logging interface to
;; its clients.

;;; Code:

;; Context

(require 'total-recall-utils)

(total-recall--context
  (send satisfies-invariant-p)

  (defconst total-recall--logger-levels
    '((:error . 0) (:info . 1) (:debug . 2)))

  (defconst total-recall--logger-symbols
    (mapcar #'car total-recall--logger-levels))

  (defun total-recall--logger-name->level (name)
    (alist-get name total-recall--logger-levels))

  (defun total-recall--logger-env->level (env)
    (pcase env
      ((pred (string= "prod")) 0)
      ((pred (string= "dev")) 2)
      ((pred (string= "test")) 0)
      (_ (error "Unexpected env. env = %s" env))))

  (defun total-recall--logger-log? (msg-level logger-level)
    (<= msg-level logger-level))

  (defun total-recall--logger-init (env)
    (let
      (
        (logger-level (total-recall--logger-env->level env))
        self)
      (setf self
        (lambda (msg)
          (let (msg-level)
            (when (memq (car-safe msg) total-recall--logger-symbols)
              (setq msg-level
                (total-recall--logger-name->level (car msg))))
            (pcase msg
              (`(:debug . ,msg)
                (when
                  (total-recall--logger-log? msg-level logger-level)
                  (princ (format "DEBUG | %s\n" msg)))
                self)

              (`(:info . ,msg)
                (when
                  (total-recall--logger-log? msg-level logger-level)
                  (princ (format "INFO | %s\n" msg)))
                self)

              (`(:error . ,msg)
                (when
                  (total-recall--logger-log? msg-level logger-level)
                  (princ (format "ERROR | %s\n" msg)))
                self)

              (:invariants '(:Logger))

              (_ (error "Unexpected message. msg = %s" msg))))))
      self))

  ;; Interface

  (defun total-recall--logger-mk (env)
    (total-recall--logger-init env))

  (defun total-recall--logger-p (any)
    (satisfies-invariant-p any :Logger))

  (defun total-recall--logger-error (logger msg)
    (send logger (cons :error msg)))

  (defun total-recall--logger-debug (logger msg)
    (send logger (cons :debug msg)))

  (defun total-recall--logger-info (logger msg)
    (send logger (cons :info msg))))

(provide 'total-recall-logger)

;;; total-recall-logger.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
