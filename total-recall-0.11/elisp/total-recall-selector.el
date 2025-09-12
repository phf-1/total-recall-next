;;; total-recall-selector.el --- Selector  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: ecfaf421-0c40-4a47-b5c9-26155acf9986

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

;; This code defines Selector, a type of objects that provide an interface to select
;; exercises to be reviewed.

;;; Code:

;; Context

(require 'cl-extra)
(require 'json)
(require 'total-recall-async)
(require 'total-recall-db)
(require 'total-recall-logger)
(require 'total-recall-utils)

(total-recall--context
    (db-p
     db-path
     epoch->iso8601
     epoch-p
     error-mk
     list-of-p
     logger-debug
     logger-p
     ok-mk
     satisfies-invariant-p
     send
     uuid-p
     wait-until-process-run)

  (defun total-recall--selector-init (logger venv lib exec-path db)
    (unless (logger-p logger)
      (error "LOGGER is not a Logger. logger = %s" logger))

    (unless (stringp venv)
      (error "VENV is not a directory. venv = %s" venv))

    (unless (file-directory-p lib)
      (error "LIB is not a directory. lib = %s" lib))

    (unless (db-p db)
      (error "DB is not a Db. db = %s" db))

    (let (self process)
      (setq self
            (lambda (state)
              (lambda (msg)
                (logger-debug logger (format "selector#rcv %s" msg))
                (pcase-let
                    (
                     (`(,reply . ,next-state)
                      (total-recall--selector-tx state msg)))
                  (setq state next-state)
                  reply))))

      (let ((process-environment (append
                                  process-environment
                                  `(,(format "TOTAL_RECALL_VENV_ACTIVATE=%s" (file-name-concat venv "bin" "activate"))
                                    ,(format "TOTAL_RECALL_PYTHON_LIB=%s" lib)))))
        (setq process
              (make-process
               :name "selector-process"
               :buffer nil
               :command (list exec-path)
               :coding 'utf-8-unix
               :noquery t
               :connection-type 'pipe
               :filter
               (lambda (_process str)
                 (send self `(:set-result ,str "from:self")))
               :stderr nil
               :file-handler nil)))

      (wait-until-process-run process)

      (setq self (funcall self (list self logger db process nil nil)))

      self))

  (defun total-recall--selector-tx (state msg)
    (pcase-let
        (
         (`(,self ,logger ,db ,process ,result ,ready) state)
         (reply nil))

      (setq reply
            (pcase msg
              (:invariants `(:Selector ,total-recall--async-invariant))

              (:async-ready ready)

              (:async-get result)

              (`(:select ,uuids ,epoch)
               (unless (list-of-p uuids #'uuid-p)
                 (error
                  "UUIDS is not a list of UUIDs. uuids = %s"
                  uuids))

               (unless (epoch-p epoch)
                 (error "EPOCH is not an Epoch. epoch = %s" epoch))

               (let (content message)
                 (setq content
                       ;; TODO: 1.0
                       (vector "select" (db-path db) 1.0 (epoch->iso8601 epoch) (apply #'vector uuids)))
                 (setq message (json-serialize content))
                 (unless (string-suffix-p "\n" message)
                   (setq message (concat message "\n")))

                 (process-send-string process message)

                 (setq ready nil)

                 nil))

              (`(:set-result ,str "from:self")
               (pcase (json-parse-string str)
                 (`[":error" ,rest] (setq result (error-mk rest)))
                 (`[":ok" ,rest] (setq result (ok-mk rest))))
               (setq ready t)
               nil)

              (:kill
               (when (process-live-p process)
                 (interrupt-process process))
               nil)

              (_ (error "Unexpected message. msg = %s" msg))))

      (cons reply (list self logger db process result ready))))

  ;; Interface

  (defun total-recall--selector-mk (logger venv lib exec-path db)
    (total-recall--selector-init logger venv lib exec-path db))

  (defun total-recall--selector-p (any)
    (satisfies-invariant-p any :Selector))

  (defun total-recall--selector-select (selector uuids epoch)
    (send selector `(:select ,uuids ,epoch)))

  (defun total-recall--selector-kill (selector)
    (send selector :kill)))

(provide 'total-recall-selector)

;;; total-recall-selector.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
