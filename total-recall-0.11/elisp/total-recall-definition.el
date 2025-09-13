;;; total-recall-definition.el --- Definition  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: 297c1e4b-3fcc-4e35-887e-442ca564f8c6
;; :REF: 64dc5603-95db-44fd-a37a-46ad390be8e7

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

;; This code defines Definition, a type of objects that represents the idea of a
;; definition to its clients.

;;; Code:

;; Context

(require 'total-recall-logger)
(require 'total-recall-utils)

(total-recall--context
  (list-of-p
    logger-debug
    satisfies-invariant-p
    send
    string-truncate
    uuid-p)

  (defun total-recall--definition-init (logger file id path content)
    (unless (stringp file)
      (error "FILE is not a string. file = %s" file))

    (unless (uuid-p id)
      (error "ID is not an UUID. id = %s" id))

    (unless (list-of-p path #'stringp)
      (error "PATH is not a string. path = %s" path))

    (unless (stringp content)
      (error "CONTENT is not a string. content = %s" content))

    (lambda (msg)
      (logger-debug logger (format "definition#rcv %s" msg))
      (pcase msg
        (:invariants '(:Definition))
        (:file file)
        (:id id)
        (:path path)
        (:content content)
        (:string
         (let (truncated-content)
           (setq truncated-content (string-truncate content))
           (string-join
            `("Definition(" ,id ,path ,truncated-content ")")
            " ")))
        (_ (error "Unexpected message. msg = %s" msg)))))

  ;; Interface

  (defun total-recall--definition-mk (logger file id path content)
    (total-recall--definition-init logger file id path content))

  (defun total-recall--definition-p (any)
    (total-recall--satisfies-invariant-p any :Definition))

  (defun total-recall--definition-file (definition)
    (send definition :file))

  (defun total-recall--definition-id (definition)
    (send definition :id))

  (defun total-recall--definition-path (definition)
    (send definition :path))

  (defun total-recall--definition-content (definition)
    (send definition :content))

  (defun total-recall--definition-string (definition)
    (send definition :string)))

(provide 'total-recall-definition)

;;; total-recall-definition.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
