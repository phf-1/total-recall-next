;;; total-recall-scheduler.el --- Scheduler  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: dad9af0f-18ea-4ad2-9105-bd00fdc21c72
;; :REF: f7e13fe3-4ff0-4ebc-8dcc-78c6e8df3516

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

;; This code defines Scheduler, a type of objects that provides an interface used to
;; schedule exercises.

;;; Code:

;; Context

(require 'cl-extra)
(require 'rx)
(require 'seq)
(require 'total-recall-async)
(require 'total-recall-exercise)
(require 'total-recall-graph)
(require 'total-recall-logger)
(require 'total-recall-utils)
(require 'total-recall-selector)

(total-recall--context
    (async-get
     async-ready
     epoch-p
     exercise-answer
     exercise-id
     exercise-p
     exercise-question
     graph-p
     graph-sort
     list-of-p
     logger-debug
     logger-p
     probability-p
     selector-p
     selector-select
     send)

  (defconst total-recall--scheduler-ref-re
    (rx
     "[[ref:"
     (group (regex total-recall--uuid-re))
     (| "]]" (seq "][" (group (*? any)) "]]"))))

  (defun total-recall--scheduler-all-refs (str)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (let ((uuids '())
            (scheduler-ref-re total-recall--scheduler-ref-re))
        (save-match-data
          (while (re-search-forward scheduler-ref-re nil t)
            (push (match-string 1) uuids)))
        (seq-uniq uuids #'string=))))

  (defun total-recall--scheduler-exercise->edges (exercise)
    (let* ((question (exercise-question exercise))
           (answer (exercise-answer exercise))
           (content (concat question answer))
           (uuids (total-recall--scheduler-all-refs content)))
      (mapcar
       (lambda (uuid) (cons (exercise-id exercise) uuid))
       uuids)))

  (defun total-recall--scheduler-init (logger graph selector)
    (unless (logger-p logger)
      (error "LOGGER is not a Logger. logger = %s" logger))

    (unless (graph-p graph)
      (error "GRAPH is not a Graph. graph = %s" graph))

    (unless (selector-p selector)
      (error "SELECTOR is not a Selector. selector = %s" selector))

    (let (self)
      (setq self
            (lambda (msg)
              (logger-debug logger (format "scheduler#rcv %s" msg))
              (pcase msg
                (:invariants '(:Scheduler))
                (`(:schedule ,exercises ,threshold ,epoch)
                 (unless (list-of-p exercises #'exercise-p)
                   (error "EXERCISES is not a List(Exercise)"))

                 (unless (probability-p threshold)
                   (error
                    "THRESHOLD is not a number between 0 and 1. threshold = %s"
                    threshold))

                 (unless (epoch-p epoch)
                   (error "EPOCH is not an Epoch. epoch = %s" epoch))

                 (let (uuids
                       selected-uuids
                       edges
                       sorted-uuids
                       (timeout 5.0)
                       (delta 0.5))
                   (setq uuids (mapcar #'exercise-id exercises))
                   (setq edges (mapcan #'total-recall--scheduler-exercise->edges
                                       exercises))
                   (selector-select selector uuids epoch)
                   (graph-sort graph uuids edges)
                   (while
                       (and
                        (not
                         (and (async-ready selector)
                              (async-ready graph)))
                        (<= 0 timeout))
                     (sleep-for delta)
                     (setq timeout (- timeout delta)))
                   (when (< timeout 0)
                     (error
                      "TIMEOUT error. selector or graph was not ready in time"))
                   (pcase (async-get selector)
                     (`(:error . ,msg) (error msg))
                     (`(:ok . ,selected) (setq selected-uuids selected)))
                   (pcase (async-get graph)
                     (`(:error . ,msg) (error msg))
                     (`(:ok . ,sorted) (setq sorted-uuids sorted)))
                   (setq uuids
                         (seq-intersection sorted-uuids selected-uuids))
                   (seq-filter
                    (lambda (x) (not (null x)))
                    (mapcar
                     (lambda (uuid)
                       (seq-find
                        (lambda (ex) (string= (exercise-id ex) uuid))
                        exercises))
                     uuids))))
                (_ (error "Unexpected message. msg = %s" msg)))))
      self))

  ;; Interface

  (defun total-recall--scheduler-mk (logger graph selector)
    (total-recall--scheduler-init logger graph selector))

  (defun total-recall--scheduler-schedule
      (scheduler exercises threshold epoch)
    (send scheduler `(:schedule ,exercises ,threshold ,epoch))))

(provide 'total-recall-scheduler)

;;; total-recall-scheduler.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
