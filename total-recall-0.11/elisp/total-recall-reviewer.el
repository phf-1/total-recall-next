;;; total-recall-reviewer.el --- Reviewer  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: 9d1ca161-acfa-48b8-bf55-b0f6e97d8d48
;; :REF: 088be4f0-e515-4501-b6f7-28201d1d0713

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

;; This code defines Reviewer, a type of objects that orchestrate the db, the ui and
;; exercises to be reviewed to provide the UX.

;;; Code:

;; Context

(require 'cl-extra)
(require 'total-recall-clock)
(require 'total-recall-db)
(require 'total-recall-exercise)
(require 'total-recall-logger)
(require 'total-recall-utils)
(require 'total-recall-rating)
(require 'total-recall-report)
(require 'total-recall-ui)

(total-recall--context
  (clock-now
    clock-p
    db-p
    db-save
    exercise-file
    exercise-id
    exercise-p
    exercise-path
    rating-failure-mk
    rating-skip-mk
    rating-success-mk
    report-add
    report-mk
    send
    ui-p
    ui-show)

  (defun total-recall--reviewer-init (logger clock db ui)
    (unless (clock-p clock)
      (error "CLOCK is not a Clock. clock = %s" clock))

    (unless (db-p db)
      (error "DB is not a Db. db = %s" db))

    (unless (ui-p ui)
      (error "UI is not a Ui. ui = %s" ui))

    (let (self)
      (setq self
        (lambda (msg)
          (pcase msg
            (`(:start ,exercises)
              (unless
                (and (listp exercises)
                  (cl-every #'exercise-p exercises))
                (error
                  "EXERCISES is not a List(Exercise). exercises = %s"
                  exercises))
              (let ((report (report-mk logger)))
                (report-add
                  report
                  "TotalRecall review session started.")
                (while (not (null exercises))
                  (let ((exercise (car exercises)))
                    (setq exercises (cdr exercises))
                    (report-add
                      report
                      (format "show:%s"
                        (string-join
                          (append
                            (string-split (exercise-file exercise)
                              "/")
                            (exercise-path exercise))
                          " / ")))
                    (pcase (ui-show ui exercise)
                      (:stop (setq exercises '()))

                      (:skip
                        (let*
                          (
                            (now (clock-now clock))
                            (rating
                              (rating-skip-mk
                                now
                                (exercise-id exercise))))
                          (db-save db rating)))

                      (:success
                        (let*
                          (
                            (now (clock-now clock))
                            (rating
                              (rating-success-mk
                                now
                                (exercise-id exercise))))
                          (db-save db rating)))

                      (:failure
                        (let*
                          (
                            (now (clock-now clock))
                            (rating
                              (rating-failure-mk
                                now
                                (exercise-id exercise))))
                          (db-save db rating)))

                      (msg (error "Unexpected msg. msg = %s" msg)))))
                (report-add
                  report
                  "TotalRecall review session finished.")
                report))
            (_ (error "Unexpected message. message = %s" msg)))))
      self))

  ;; Interface

  (defun total-recall--reviewer-mk (logger clock db ui)
    (total-recall--reviewer-init logger clock db ui))

  (defun total-recall--reviewer-start (reviewer exercises)
    (send reviewer `(:start ,exercises))))

(provide 'total-recall-reviewer)

;;; total-recall-reviewer.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
