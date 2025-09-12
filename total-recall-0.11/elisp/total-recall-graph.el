;;; total-recall-graph.el --- Graph  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: 1e9f67e3-1c46-4ed0-890f-a7dca835b062

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

;; This code defines Graph, a type of objects that represents the idea of a graph to
;; its clients.

;;; Code:

;; Context

(require 'cl-extra)
(require 'json)
(require 'total-recall-async)
(require 'total-recall-logger)
(require 'total-recall-utils)

(total-recall--context
    (error-mk
     list-of-p
     logger-debug
     logger-p
     ok-mk
     satisfies-invariant-p
     send
     uuid-p
     wait-until-process-run)

  (defun total-recall--graph-init (logger venv lib exec-path)
    (unless (logger-p logger)
      (error "LOGGER is not a Logger. logger = %s" logger))

    (unless (stringp venv)
      (error "VENV is not a directory. venv = %s" venv))

    (unless (file-directory-p lib)
      (error "LIB is not a directory. lib = %s" lib))

    (let (self process)
      (setq self
            (lambda (state)
              (lambda (msg)
                (logger-debug logger (format "graph#rcv %s" msg))
                (pcase-let
                    (
                     (`(,reply . ,next-state)
                      (total-recall--graph-tx state msg)))
                  (setq state next-state)
                  reply))))

      (let ((process-environment (append
                                  process-environment
                                  `(,(format "TOTAL_RECALL_VENV_ACTIVATE=%s" (file-name-concat venv "bin" "activate"))
                                    ,(format "TOTAL_RECALL_PYTHON_LIB=%s" lib)))))
        (setq process
              (make-process
               :name "graph-process"
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

      (setq self (funcall self (list self logger process nil nil)))

      self))

  (defun total-recall--graph-tx (state msg)
    (pcase-let
        (
         (`(,self ,logger ,process ,result ,ready) state)
         (reply nil))

      (setq reply
            (pcase msg
              (:invariants `(:Graph ,total-recall--async-invariant))

              (:async-ready ready)

              (:async-get result)

              (`(:sort ,uuids ,edges)
               (unless (list-of-p uuids #'uuid-p)
                 (error
                  "UUIDS is not a list of UUIDs. uuids = %s"
                  uuids))

               (unless (list-of-p edges #'total-recall--graph-edge-p)
                 (error "EDGES is not a list of Edge. edges = %s" edges))

               (setq edges
                     (seq-filter
                      (pcase-lambda (`(,start . ,end)) (and (member start uuids) (member end uuids)))
                      edges))

               (let (message uuids-v edges-v)
                 (setq uuids-v (apply #'vector uuids))
                 (setq edges-v
                       (apply #'vector
                              (mapcar (pcase-lambda (`(,start . ,end)) (vector start end))
                                      edges)))
                 (setq message
                       (json-serialize (vector "sort" uuids-v edges-v)))
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

      (cons reply (list self logger process result ready))))

  (defun total-recall--graph-edge-p (edge)
    (and (consp edge) (uuid-p (car edge)) (uuid-p (cdr edge))))

  ;; Interface

  (defun total-recall--graph-mk (logger venv lib exec-path)
    (total-recall--graph-init logger venv lib exec-path))

  (defun total-recall--graph-p (any)
    (satisfies-invariant-p any :Graph))

  (defun total-recall--graph-sort (graph uuids edges)
    (send graph `(:sort ,uuids ,edges)))

  (defun total-recall--graph-kill (graph)
    (send graph :kill)))

(provide 'total-recall-graph)

;;; total-recall-graph.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
