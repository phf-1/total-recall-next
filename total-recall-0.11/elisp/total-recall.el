;;; total-recall.el --- Spaced repetitions and Org Mode  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: 4c292e22-4b9a-4cb2-8867-17fa1eabf024

;; Copyright (C) 2025 Pierre-Henry FRÖHRING

;; Author: Pierre-Henry FRÖHRING <contact@phfrohring.com>
;; Maintainer: Pierre-Henry FRÖHRING <contact@phfrohring.com>
;; Homepage: https://github.com/phf-1/total-recall
;; Keywords: hypermedia outlines
;; Version: 0.11
;; Package-Requires: ((emacs "29.4"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; `total-recall' is an interactive spaced repetition system for Emacs. It manages
;; "elements" (i.e. definitions and exercises) stored in Org Files under
;; `total-recall-root-dir'. Elements are sorted based on dependencies (e.g., if
;; element A requires element B, element B is presented first). Element selection
;; uses historical data to optimize review frequency: easier elements are shown less
;; often.

;;; Code:

;; Context

(require 'total-recall-clock)
(require 'total-recall-conf)
(require 'total-recall-db)
(require 'total-recall-io)
(require 'total-recall-logger)
(require 'total-recall-parser)
(require 'total-recall-utils)
(require 'total-recall-report)
(require 'total-recall-reviewer)
(require 'total-recall-scheduler)
(require 'total-recall-searcher)
(require 'total-recall-ui)

(total-recall--context
    (
     clock-mk
     clock-now
     conf-buffer-name
     conf-db-path
     conf-def-type
     conf-def-type
     conf-env
     conf-ex-type
     conf-ex-type
     conf-failure-key
     conf-graph-exec-path
     conf-height
     conf-install-python-env
     conf-io-buffer-name
     conf-mk
     conf-python-lib
     conf-python-venv
     conf-quit-key
     conf-reveal-key
     conf-ripgrep
     conf-root
     conf-selector-exec-path
     conf-skip-key
     conf-success-key
     conf-threshold
     conf-width
     db-mk
     graph-kill
     graph-mk
     io-buffer
     io-buffer-name
     io-minibuffer
     io-mk
     logger-mk
     parser-mk
     parser-parse
     report-string
     reviewer-mk
     reviewer-start
     scheduler-mk
     scheduler-schedule
     searcher-files
     searcher-mk
     selector-kill
     selector-mk
     ui-mk
     )

  ;; Interface

  ;;;###autoload
  (defun total-recall ()
    (interactive)

    (let* ((conf (conf-mk))
           (_python-env (conf-install-python-env conf))
           (env (conf-env conf))
           (logger (logger-mk env))
           (ex-type (conf-ex-type conf))
           (def-type (conf-def-type conf))
           (db-path (conf-db-path conf))
           (threshold (conf-threshold conf))
           (searcher
            (searcher-mk
             logger
             (conf-root conf)
             (conf-def-type conf)
             (conf-ex-type conf)
             (conf-ripgrep conf)))
           (files (searcher-files searcher))
           (parser (parser-mk logger ex-type def-type))
           (exercises (parser-parse parser files))
           (clock (clock-mk logger))
           (db (db-mk logger db-path))
           (python-lib (conf-python-lib conf))
           (venv-path (conf-python-venv conf))
           (selector (selector-mk logger venv-path python-lib (conf-selector-exec-path conf) db))
           (graph (graph-mk logger venv-path python-lib (conf-graph-exec-path conf)))
           (scheduler (scheduler-mk logger graph selector))
           (schedule
            (scheduler-schedule
             scheduler
             exercises
             threshold
             (clock-now clock)))
           (ui
            (ui-mk
             :failure-key (conf-failure-key conf)
             :reveal-key (conf-reveal-key conf)
             :quit-key (conf-quit-key conf)
             :skip-key (conf-skip-key conf)
             :success-key (conf-success-key conf)
             :height (conf-height conf)
             :width (conf-width conf)
             :name (conf-buffer-name conf)
             :logger logger))
           (io (io-mk logger (conf-io-buffer-name conf)))
           (reviewer (reviewer-mk logger clock db ui))
           (finished-report (reviewer-start reviewer schedule))
           (finished-report-str (report-string finished-report))
           (minibuffer-msg
            (format
             "Total-recall execution finished. Report written to %s"
             (io-buffer-name io))))
      (io-buffer io finished-report-str)
      (io-minibuffer io minibuffer-msg)
      (selector-kill selector)
      (graph-kill graph))))

(provide 'total-recall)

;;; total-recall.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
