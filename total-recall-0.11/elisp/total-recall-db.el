;;; total-recall-db.el --- Db  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: 3020d2a5-face-4348-b05d-e8755d7c6195
;; :REF: 74d00768-f37a-49c9-a943-4a39f1a26c0e

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

;; This code defines Db, a type of objects that provides reading/writing values to
;; persistent storage to its clients.

;;; Code:

;; Context

(require 'parse-time)
(require 'total-recall-logger)
(require 'total-recall-utils)
(require 'total-recall-rating)

(total-recall--context
  (epoch->iso8601
    iso8601->epoch
    logger-debug
    rating-data
    rating-mk
    rating-p
    satisfies-invariant-p
    send
    uuid-p)

  (defun total-recall--db--save (sqlite rating)
    (unless (rating-p rating)
      (error "RATING is not a Rating. rating = %s" rating))

    (pcase (rating-data rating)
      (`(,epoch ,id ,value)
        (let
          (
            row
            time)
          (setq time (epoch->iso8601 epoch))

          (setq row (list (symbol-name value) id time))

          (sqlite-execute
            sqlite
            "INSERT INTO exercise_log (type, id, time) VALUES (?, ?, ?)"
            row)))))

  (defun total-recall--db--ratings (sqlite id)
    (unless (uuid-p id)
      (error "ID is not a UUID string: id = %s" id))

    (let
      (
        rows
        ratings
        cmd)
      (setq cmd
        "SELECT type, id, time FROM exercise_log WHERE id = ? ORDER BY time ASC")

      (setq rows (sqlite-select sqlite cmd (list id)))

      (setq ratings
        (mapcar
          (lambda (row)
            (pcase row
              (`(,type ,id ,iso8601)
                (rating-mk (iso8601->epoch iso8601) id (intern type)))
              (_ (error "Unexpected row: row = %s" row))))
          rows))

      ratings))

  (defun total-recall--db-init (logger &optional path)
    (let
      (
        sqlite
        self)
      (unless (sqlite-available-p)
        (error
          "Emacs must be compiled with built-in support for SQLite databases"))

      (setq sqlite (sqlite-open path))

      (unless
        (sqlite-select
          sqlite
          "SELECT name FROM sqlite_master WHERE type='table' AND name='exercise_log'")
        (sqlite-execute
          sqlite
          "CREATE TABLE exercise_log (
                       type TEXT NOT NULL,
                       id TEXT NOT NULL,
                       time TEXT NOT NULL)"))

      (setq self
        (lambda (msg)
          (logger-debug logger (format "db#rcv %s" msg))
          (pcase msg
            (:invariants '(:Db))

            (`(:save ,rating)
              (total-recall--db--save sqlite rating)
              self)

            (`(:ratings ,id) (total-recall--db--ratings sqlite id))

            (:path path)

            (:stop
              (sqlite-close sqlite)
              self)

            (_ (error "Unexpected message. msg = %s" msg)))))
      self))

  ;; Interface

  (defun total-recall--db-mk (logger &optional path)
    (total-recall--db-init logger path))

  (defun total-recall--db-p (any)
    (satisfies-invariant-p any :Db))

  (defun total-recall--db-save (db rating)
    (send db `(:save ,rating)))

  (defun total-recall--db-ratings (db id)
    (send db `(:ratings ,id)))

  (defun total-recall--db-stop (db)
    (send db :stop))

  (defun total-recall--db-path (db)
    (send db :path)))

(provide 'total-recall-db)

;;; total-recall-db.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
