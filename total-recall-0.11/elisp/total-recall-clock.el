;;; total-recall-clock.el --- Clock  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: 712feead-c9ee-4dad-830a-0181ab4082d5

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

;; This code defines Clock, a type of objects provides time to its clients.

;;; Code:

;; Context

(require 'total-recall-logger)
(require 'total-recall-utils)

(total-recall--context
  (logger-p logger-debug satisfies-invariant-p send)
  (defun total-recall--clock-init (logger)
    (unless (logger-p logger)
      (error "LOGGER is not a Logger %s" logger))

    (let (self)
      (setq self
        (lambda (msg)
          (logger-debug logger (format "clock#rcv %s" msg))
          (pcase msg
            (:now (time-convert (current-time) 'integer))
            (:invariants '(:Clock))
            (_ (error "Unexpected message. msg = %s" msg)))))
      self))

  ;; Interface

  (defun total-recall--clock-p (any)
    (satisfies-invariant-p any :Clock))

  (defun total-recall--clock-mk (logger)
    (total-recall--clock-init logger))

  (defun total-recall--clock-now (clock)
    (send clock :now)))

(provide 'total-recall-clock)

;;; total-recall-clock.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
