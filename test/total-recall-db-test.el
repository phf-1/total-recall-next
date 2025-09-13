;;; total-recall-db-test.el -*- lexical-binding: t; -*-

;; :ID: d31af193-988e-49b3-93db-08c41266bf84
;; :REF: 3020d2a5-face-4348-b05d-e8755d7c6195

;; Copyright (C) 2025 Pierre-Henry FRÖHRING
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; Test suite for total-recall-db.el using ERT.
;;
;;; Code:

(require 'ert)
(require 'parse-time)
(require 'total-recall-logger)
(require 'total-recall-rating)
(require 'total-recall-uuid)
(require 'total-recall-db)


(ert-deftest total-recall--db ()
  "Test saving a valid rating to the database."
  (let* ((logger (total-recall--logger-mk "test"))
         (db (total-recall--db-mk logger))
         (id "123e4567-e89b-12d3-a456-426614174000")
         (time (current-time))
         (rating (total-recall--success time id))
         rows)

    (setq rows
          (total-recall--db-ratings
           (total-recall--db-save db rating) id))

    (should (equal (length rows) 1))

    (total-recall--db-stop db)))


(provide 'total-recall-db-test)

;;; total-recall-db-test.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
