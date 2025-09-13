;;; total-recall-parser.el --- Parser  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: 5d5fec94-de66-4530-8267-a7f26ae03f2b
;; :REF: 5c51b191-f640-434a-a194-a432ee2e967f

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

;; This code defines Parser, a type of objects that provides an interface to map Org
;; Elements to other objects to its clients.

;;; Code:

;; Context

(require 'org)
(require 'org-element)
(require 'org-element-ast)
(require 'total-recall-exercise)
(require 'total-recall-logger)
(require 'total-recall-utils)

(total-recall--context
  (uuid-p error-mk ok-mk exercise-mk logger-debug send)
  ;; Element :≡ [[help:org-element-parse-buffer]]
  ;; Headline :≡ Element e such that (eq (car e) 'headline)
  ;; Headline → Id ≡ String | Nil
  (defun total-recall--parser-headline->id (headline)
    (org-element-property :ID headline))

  ;; Headline → Type ≡ String | Nil
  (defun total-recall--parser-headline->type (headline)
    (org-element-property :TYPE headline))

  ;; Headline → List(Headline)
  (defun total-recall--parser-headline->children (headline)
    (org-element-contents headline))

  ;; Headline → String
  (defun total-recall--parser-headline->content (headline)
    (buffer-substring-no-properties
      (org-element-property :begin headline)
      (org-element-property :end headline)))

  ;; TreeString t → TreeString t' such that t' = t except that root level is 1
  (defun total-recall--parser-content->lowered (content)
    (with-temp-buffer
      (insert content)
      (org-mode)
      (goto-char (point-min))
      (while (> (org-current-level) 1)
        (org-promote-subtree))
      (buffer-substring-no-properties (point-min) (point-max))))

  ;; Headline → List(String)
  (defun total-recall--parser-headline->path (headline)
    (mapcar
     (lambda (parent) (org-element-property :raw-value parent))
     (cdr (reverse (org-element-lineage headline nil t)))))

  ;; Path :≡ List(String)
  ;; Id :≡ UUID
  ;; Question :≡ String
  ;; Answer :≡ String
  ;; ExData :≡ Path × Id × Question × Answer
  ;; String String Headline → 'ok × ExData | 'error × String
  (defun total-recall--parser-headline->ex-data
    (ex-type def-type headline)
    (let (path id)
      (setq path (total-recall--parser-headline->path headline))
      (setq id (total-recall--parser-headline->id headline))
      (pcase (uuid-p id)
        ('nil (error-mk "headline has no id"))
        (_
          (pcase (total-recall--parser-headline->type headline)
            ((pred (string= ex-type))
              (let (children question answer)
                (setq children (total-recall--parser-headline->children headline))
                (setq question (car-safe children))
                (pcase question
                  ('nil
                    (error-mk
                      (format
                        "exercise is missing a question. id = %s"
                        id)))
                  (_
                    (setq question
                      (total-recall--parser-content->lowered
                        (total-recall--parser-headline->content
                          question)))
                    (setq answer (car-safe (cdr children)))
                    (pcase answer
                      ('nil
                        (error-mk
                          (format
                            "exercise is missing an answer. id = %s"
                            id)))
                      (_
                        (setq answer
                          (total-recall--parser-content->lowered
                            (total-recall--parser-headline->content
                              answer)))
                        (ok-mk `(,path ,id ,question ,answer))))))))
            ((pred (string= def-type))
              (let (question answer)
                ;; TODO: must be configurable
                (setq question "* What is the definition?\n\n")
                (setq answer
                  (total-recall--parser-content->lowered
                    (total-recall--parser-headline->content
                      headline)))
                (ok-mk `(,path ,id ,question ,answer))))
            (_ (error-mk "headline has unexpected type")))))))

  ;; String String Headline → Exercise | Nil
  (defun total-recall--parser-headline->exercise
    (logger ex-type def-type file headline)
    (pcase
      (total-recall--parser-headline->ex-data ex-type def-type headline)
      (`(:ok . (,path ,id ,question ,answer))
        (exercise-mk logger file path id question answer))
      (_ nil)))

  ;; String String File → List(Exercise)
  (defun total-recall--parser-file->exercises
    (logger ex-type def-type file)
    (org-babel-with-temp-filebuffer file
      (org-element-map
        (org-element-parse-buffer 'headline) 'headline
        (lambda (headline)
          (total-recall--parser-headline->exercise
            logger
            ex-type
            def-type
            file
            headline)))))

  ;; Logger UUID UUID → Parser
  (defun total-recall--parser-init (logger ex-type def-type)
    (let (self)
      (setq self
        (lambda (msg)
          (logger-debug logger (format "parser#rcv %s" msg))
          (pcase msg
            (:invariants '(:Parser))
            (`(:parse-files ,files)
              (let (func)
                (setq func
                  (lambda (file)
                    (total-recall--parser-parse self file)))
                (mapcan func files)))
            (`(:parse-file ,file)
              (total-recall--parser-file->exercises
                logger
                ex-type
                def-type
                file)))))
      self))

  ;; Interface

  (defun total-recall--parser-mk (logger ex-type def-type)
    (total-recall--parser-init logger ex-type def-type))

  (defun total-recall--parser-parse (parser file-or-files)
    (cond
      ((listp file-or-files)
        (send parser `(:parse-files ,file-or-files)))
      ((file-exists-p file-or-files)
        (send parser `(:parse-file ,file-or-files)))
      (t
        (error "Unexpected input. input = %s" file-or-files)))))

(provide 'total-recall-parser)

;;; total-recall-parser.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
