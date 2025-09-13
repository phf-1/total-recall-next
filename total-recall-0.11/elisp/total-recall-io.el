;;; total-recall-io.el --- Io  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: b4ab8208-e1b2-4468-8a4f-db373d7d556f
;; :REF: a5fdf3d6-a742-418f-9af6-8e83cf2bcef6

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

;; This code defines Io, a type of objects that represents IO to its clients.

;;; Code:

;; Context

(require 'total-recall-logger)
(require 'total-recall-utils)

(total-recall--context
  (logger-p send)

  (defun total-recall--io-init (logger name)
    (unless (logger-p logger)
      (error "LOGGER is not a Logger. logger = %s" logger))

    (unless (stringp name)
      (error "NAME is not a string. name = %s" name))

    (let*
      (
        (buffer (get-buffer-create name))
        (buffer-name (buffer-name buffer))
        self)

      (setq self
        (lambda (msg)
          (pcase msg
            (`(:minibuffer ,string)
              (message "%s" (string-trim string))
              self)

            (`(:buffer ,string)
              (with-current-buffer buffer
                (erase-buffer)
                (insert (string-join (list string "\n"))))
              self)

            (:buffer-name buffer-name)

            (_ (error "Unexpected message. msg = %s" msg)))))
      self))

  ;; Interface

  (defun total-recall--io-mk (logger name)
    (total-recall--io-init logger name))

  (defun total-recall--io-minibuffer (io msg)
    (send io `(:minibuffer ,msg)))

  (defun total-recall--io-buffer (io msg)
    (send io `(:buffer ,msg)))

  (defun total-recall--io-buffer-name (io)
    (send io :buffer-name)))

;; Implementation

(provide 'total-recall-io)

;;; total-recall-io.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
