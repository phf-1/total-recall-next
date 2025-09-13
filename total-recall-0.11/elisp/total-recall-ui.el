;;; total-recall-ui.el --- Ui  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: 071782c8-7575-4678-8090-9e8abaad044c
;; :REF: 2e317042-46f4-4407-9bd4-68ec22c1955e

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

;; This code defines Ui, a type of objects that provides a Ui that transduces user
;; interactions into messages to its clients.

;;; Code:

;; Context

(require 'cl-macs)
(require 'total-recall-conf)
(require 'total-recall-exercise)
(require 'total-recall-logger)
(require 'total-recall-utils)
(require 'total-recall-report)

(total-recall--context
  (exercise-answer
    exercise-file
    exercise-id
    exercise-p
    exercise-path
    exercise-question
    logger-debug
    logger-p
    nat-p
    satisfies-invariant-p
    send)

  (defun total-recall--ui-exercise-meta (exercise reviewed-nbr)
    (let
      (
        template
        path)
      (setq template (string-trim-left "
┌────
│ path: %s
│ link: %s
│ reviewed: %s exercises
└────
"))

      (setq path
        (string-trim
          (string-join
            (append
              (string-split (exercise-file exercise) "/")
              (exercise-path exercise))
            " / ")))

      (format template
        path
        (format "[[ref:%s]]" (exercise-id exercise))
        reviewed-nbr)))

  (cl-defun
    total-recall--ui-init
    (&key
      logger
      name
      width
      height
      success-key
      failure-key
      quit-key
      skip-key
      reveal-key)
    (unless (logger-p logger)
      (error "LOGGER is not a Logger. logger = %s" logger))

    (unless (stringp name)
      (error "NAME is not a String. name = %s" name))

    (unless (nat-p width)
      (error "WIDTH is not a Nat. width = %s" width))

    (unless (nat-p height)
      (error "HEIGHT is not a Nat. height = %s" height))

    (unless (characterp success-key)
      (error
        "SUCCESS-KEY is not a Character. success-key = %s"
        success-key))

    (unless (characterp failure-key)
      (error
        "FAILURE-KEY is not a Character. failure-key = %s"
        failure-key))

    (unless (characterp skip-key)
      (error "SKIP-KEY is not a Character. skip-key = %s" skip-key))

    (unless (characterp reveal-key)
      (error
        "REVEAL-KEY is not a Character. reveal-key = %s"
        reveal-key))

    (unless (characterp quit-key)
      (error "QUIT-KEY is not a Character. quit-key = %s" quit-key))

    (let
      (
        buffer
        frame
        reviewed-nbr
        self)
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
        (setq buffer-read-only t))
      (setq frame (make-frame `((width . ,width) (height . ,height))))
      (setq reviewed-nbr 0)
      (setq self
        (lambda (msg)
          (logger-debug logger (format "ui#rcv %s" msg))
          (pcase msg
            (:invariants '(:Ui))

            ((and `(:show ,exercise) (guard (exercise-p exercise)))
              (send self :show-frame)
              (send self :clear-frame)
              (send self `(:show-meta ,exercise ,reviewed-nbr))
              (send
                self
                `(:show-content ,(exercise-question exercise)))
              (pcase
                (send
                  self
                  `
                  (:ask-user
                    ((,quit-key . "Quit")
                      (,skip-key . "Skip")
                      (,reveal-key . "Reveal"))))
                ((pred (eq quit-key)) (send self :stop))
                ((pred (eq skip-key)) :skip)
                ((pred (eq reveal-key))
                  (send
                    self
                    `(:show-content ,(exercise-answer exercise)))
                  (setq reviewed-nbr (1+ reviewed-nbr))
                  (pcase
                    (send
                      self
                      `
                      (:ask-user
                        ((,success-key . "Success")
                          (,failure-key . "Failure")
                          (,skip-key . "Skip")
                          (,quit-key . "Quit"))))
                    ((pred (eq success-key)) :success)
                    ((pred (eq failure-key)) :failure)
                    ((pred (eq skip-key)) :skip)
                    ((pred (eq quit-key)) (send self :stop))))))

            (:show-frame
              (select-frame-set-input-focus frame)
              (switch-to-buffer buffer)
              self)

            (:clear-frame
              (with-current-buffer buffer
                (setq buffer-read-only nil)
                (erase-buffer)
                (unless (derived-mode-p 'org-mode)
                  (org-mode))
                (insert "* Total Recall *\n\n")
                (goto-char (point-min))
                (setq buffer-read-only t))
              self)

            (`(:show-meta ,exercise ,reviewed-nbr)
              (let (meta)
                (setq meta
                  (total-recall--ui-exercise-meta
                    exercise
                    reviewed-nbr))
                (send self `(:show-content ,meta)))
              self)

            (`(:show-content ,content)
              (send self :show-frame)
              (with-current-buffer buffer
                (setq buffer-read-only nil)
                (save-excursion
                  (goto-char (point-max))
                  (insert
                    (string-join
                      (list (string-trim content) "\n\n"))))
                (setq buffer-read-only t))
              self)

            (:stop
              (when (buffer-live-p buffer)
                (kill-buffer buffer))
              (when (frame-live-p frame)
                (delete-frame frame))
              :stop)

            (`(:ask-user ,options)
              (send self :show-frame)
              (let
                (
                  strs
                  str)
                (setq strs
                  (mapcar
                    (pcase-lambda (`(,char . ,name))
                      (format "%s (%s)" name (string char)))
                    options))
                (setq str (string-join strs ", "))
                (read-char-choice str (mapcar #'car options))))

            (_ (error "Unexpected message. message = %s" msg)))))
      self))

  ;; Interface

  (cl-defun
    total-recall--ui-mk
    (&key
      logger
      name
      width
      height
      success-key
      failure-key
      quit-key
      skip-key
      reveal-key)

    (total-recall--ui-init
      :logger logger
      :name name
      :width width
      :height height
      :success-key success-key
      :failure-key failure-key
      :quit-key quit-key
      :skip-key skip-key
      :reveal-key reveal-key))

  (defun total-recall--ui-p (any)
    (satisfies-invariant-p any :Ui))

  (defun total-recall--ui-show (ui exercise)
    (send ui `(:show ,exercise))))

(provide 'total-recall-ui)

;;; total-recall-ui.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
