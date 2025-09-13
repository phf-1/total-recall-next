;;; total-recall-report.el --- Report  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: d6adcba5-5f9a-42ae-b8f9-dd627d6c10f4
;; :REF: 05a4ce8c-583a-43d2-9dde-af32164d1a97

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

;; This code defines Report, a type of objects used to accumulate lines of text
;; during execution and return the result afterwards.

;;; Code:

;; Context

(require 'total-recall-logger)
(require 'total-recall-utils)

(total-recall--context
    (logger-p
     logger-debug
     satisfies-invariant-p
     send)

  (defun total-recall--report-init (logger)
    (unless (logger-p logger)
      (error "LOGGER is not a Logger %s" logger))

    (let ((lines '()) self)
      (setq self
            (lambda (msg)
              (logger-debug logger (format "report#rcv %s" msg))
              (pcase msg
                (:invariants '(:Report))

                (`(:add ,line)
                 (unless (stringp line)
                   (error "LINE is not a String %s" line))
                 (setq lines (cons line lines))
                 self)

                (:string (string-join (reverse lines) "\n")))))
      self))

  ;; Interface

  (defun total-recall--report-mk (logger)
    (total-recall--report-init logger))

  (defun total-recall--report-p (any)
    (satisfies-invariant-p any :Report))

  (defun total-recall--report-add (report line)
    (send report `(:add ,line)))

  (defun total-recall--report-string (report)
    (send report :string)))

(provide 'total-recall-report)

;;; total-recall-report.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
