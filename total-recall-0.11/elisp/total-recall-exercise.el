;;; total-recall-exercise.el --- Exercise  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: 5ee57d9b-958e-4b6e-a62f-1c0b06a0f4d9

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

;; This code defines Exercise, a type of objects that represents the idea of an
;; exercise to its clients.

;;; Code:

;; Context

(require 'total-recall-logger)
(require 'total-recall-utils)
(require 'cl-lib)

(total-recall--context
  (list-of-p
    logger-debug
    satisfies-invariant-p
    send
    string-truncate
    uuid-p)

  (defun total-recall--exercise-init
    (logger file path id question answer)
    (unless (stringp file)
      (error "FILE is not a string. file = %s" file))

    (unless (uuid-p id)
      (error "ID is not an UUID. id = %s" id))

    (unless (list-of-p path #'stringp)
      (error "PATH is not a string. path = %s" path))

    (unless (stringp question)
      (error "QUESTION is not a string. question = %s" question))

    (unless (stringp answer)
      (error "ANSWER is not a string. answer = %s" answer))

    (lambda (msg)
      (logger-debug logger (format "exercise#rcv %s" msg))
      (pcase msg
        (:invariants '(:Exercise))
        (:file file)
        (:id id)
        (:path path)
        (:question question)
        (:answer answer)
        (:string
          (string-join
            `
            ("Exercise("
              ,id
              ,(format "%s" path)
              ,(string-truncate question)
              ,(string-truncate answer)
              ")")
            " "))
        (_ (error "Unexpected message. msg = %s" msg)))))

  ;; Interface

  (defun total-recall--exercise-mk
    (logger file path id question answer)
    (total-recall--exercise-init logger file path id question answer))

  (defun total-recall--exercise-p (any)
    (satisfies-invariant-p any :Exercise))

  (defun total-recall--exercise-file (exercise)
    (send exercise :file))

  (defun total-recall--exercise-id (exercise)
    (send exercise :id))

  (defun total-recall--exercise-path (exercise)
    (send exercise :path))

  (defun total-recall--exercise-question (exercise)
    (send exercise :question))

  (defun total-recall--exercise-answer (exercise)
    (send exercise :answer))

  (defun total-recall--exercise-string (exercise)
    (send exercise :string)))

(provide 'total-recall-exercise)

;;; total-recall-exercise.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
