;;; total-recall-searcher.el --- Searcher  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: 674f7b2d-6b75-4df0-a431-5c574586a871

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

;; This code defines Searcher, a type of objects that provide an interface used to
;; search files that contains exercises and definitions.

;;; Code:

;; Context

(require 'total-recall-logger)
(require 'total-recall-conf)

(total-recall--context
    (send logger-debug logger-p uuid-p)
  (defun total-recall--searcher-init
      (logger root def-type ex-type ripgrep)
    (unless (logger-p logger)
      (error "LOGGER is not a Logger. logger = %s" logger))

    (unless (file-directory-p root)
      (error "ROOT is not a directory. root = %s" root))

    (unless (uuid-p def-type)
      (error "DEF-TYPE is not a UUID: def-type = %s" def-type))

    (unless (uuid-p ex-type)
      (error "EX-TYPE is not a UUID: ex-type = %s" ex-type))

    (unless (stringp (executable-find ripgrep))
      (error "RIPGREP not found in PATH: ripgrep = %s" ripgrep))

    (let (cmd)
      (setq cmd
            (format
             "%s -g '*.org' -i --no-heading -n --color=never -m 1 '%s' %s"
             ripgrep
             (format "%s|%s" def-type ex-type)
             root))

      (lambda (msg)
        (logger-debug logger (format "searcher#rcv %s" msg))
        (pcase msg
          (:files
           (let (matches)
             (with-temp-buffer
               (call-process-shell-command cmd
                                           nil
                                           `(,(current-buffer) nil)
                                           nil)
               (goto-char (point-min))
               (while (not (eobp))
                 (let*
                     (
                      (line
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
                      (match (split-string line ":")))
                   (push (car match) matches))
                 (forward-line 1)))
             (delete-dups matches)))
          (_ (error "Unexpected message. msg = %s" msg))))))

  ;; Interface

  (defun total-recall--searcher-mk
      (logger root def-type ex-type ripgrep)
    (total-recall--searcher-init
     logger
     root
     def-type
     ex-type
     ripgrep))

  (defun total-recall--searcher-files (searcher)
    (send searcher :files)))

(provide 'total-recall-searcher)

;;; total-recall-searcher.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
