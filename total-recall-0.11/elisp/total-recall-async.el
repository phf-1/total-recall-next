;;; total-recall-async.el --- Async  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: 888b08d8-4adf-4202-a62d-6b3909ed925d

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

;; This code defines Async, an interface for ready async values from objects that
;; implement it.

;;; Code:

;; Context

(require 'total-recall-utils)

(total-recall--context
  (send)

  ;; Interface

  (defconst total-recall--async-invariant :Async)

  (defun total-recall--async-ready (actor)
    (send actor :async-ready))

  (defun total-recall--async-get (actor)
    (send actor :async-get)))


(provide 'total-recall-async)

;;; total-recall-async.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
