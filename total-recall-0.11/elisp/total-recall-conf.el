;;; total-recall-conf.el --- Conf  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: d9e6a76c-0233-409c-993a-3cd6ad4333af
;; :REF: 656ec911-0225-415d-abf0-4e760bc782f1

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

;; This code defines Conf, a type of objects that reads execution environement — e.g.
;; user options — and provide them to its clients.

;;; Code:

;; Context

(require 'total-recall-utils)

(total-recall--context
    (send)

  (defgroup total-recall nil
    "Customization options for Total Recall.
This package provides `total-recall' for spaced repetition in Emacs."
    :group 'convenience
    :prefix "total-recall-")

  (defcustom total-recall-dir
    (file-name-concat (expand-file-name user-emacs-directory)
                      "total-recall")
    ""
    :type 'string
    :group 'total-recall)

  (defcustom total-recall-root-dir
    (expand-file-name (or (getenv "TOTAL_RECALL_ROOT") "~"))
    "Specifies the root directory for Total Recall file searches.
This is a string representing the directory path where Org Mode files
are searched."
    :type 'string
    :group 'total-recall)

  (defcustom total-recall-database
    (file-name-concat total-recall-dir "total-recall-test.sqlite3")
    "Specifies the path to the Total Recall SQLite database.
This is a string representing the file path for storing review data."
    :type 'string
    :group 'total-recall)

  (defcustom total-recall-ripgrep-cmd "rg"
    "Specifies the name or path of the Ripgrep executable.
This is a string used to locate the Ripgrep command for file searching."
    :type 'string
    :group 'total-recall)

  (defcustom total-recall-io-buffer-name "*TotalRecall(io)*"
    "Specifies the name of the Total Recall output buffer.
This is a string used for the buffer where reports are written."
    :type 'string
    :group 'total-recall)

  (defcustom total-recall-buffer-name "*TotalRecall(review)*"
    "Specifies the name of the Total Recall review buffer.
This is a string used for the buffer where reviews are presented."
    :type 'string
    :group 'total-recall)

  (defcustom total-recall-def-type
    "f590edb9-5fa3-4a07-8f3d-f513950d5663"
    "Specifies the UUID for identifying definition headings in Org files.
This is a string used to mark headings as definitions in Total Recall."
    :type 'string
    :group 'total-recall)

  (defcustom total-recall-ex-type
    "b0d53cd4-ad89-4333-9ef1-4d9e0995a4d8"
    "Specifies the UUID for identifying exercise headings in Org files.
This is a string used to mark headings as exercises in Total Recall."
    :type 'string
    :group 'total-recall)

  (defcustom total-recall-env (or (getenv "TOTAL_RECALL_ENV") "prod")
    ""
    :type 'string
    :group 'total-recall)

  (defcustom total-recall-window-width 160
    "Specifies the width of the Total Recall UI frame in characters.
This is an integer defining the frame width for the UI."
    :type 'integer
    :group 'total-recall)

  (defcustom total-recall-window-height 90
    "Specifies the height of the Total Recall UI frame in characters.
This is an integer defining the frame height for the UI."
    :type 'integer
    :group 'total-recall)

  (defcustom total-recall-key-skip ?k
    "Specifies the key to skip an exercise in the Total Recall UI.
This is a character used to skip the current exercise."
    :type 'character
    :group 'total-recall)

  (defcustom total-recall-key-quit ?q
    "Specifies the key to quit the Total Recall session.
This is a character used to exit the UI session."
    :type 'character
    :group 'total-recall)

  (defcustom total-recall-key-success ?s
    "Specifies the key to mark an exercise as successful in the Total Recall UI.
This is a character used to record a successful review."
    :type 'character
    :group 'total-recall)

  (defcustom total-recall-key-failure ?f
    "Specifies the key to mark an exercise as failed in the Total Recall UI.
This is a character used to record a failed review."
    :type 'character
    :group 'total-recall)

  (defcustom total-recall-threshold 0.8
    ""
    :type 'float
    :group 'total-recall)

  (defcustom total-recall-key-reveal ?r
    "Specifies the key to reveal the answer in the Total Recall UI.
This is a character used to show the exercise answer."
    :type 'character
    :group 'total-recall)

  (defconst total-recall--root-path
    (file-name-parent-directory
     (file-name-parent-directory
      load-file-name)))

  (defconst total-recall--lib-elisp-path
    (file-name-concat total-recall--root-path "elisp"))

  (defconst total-recall--lib-python-path
    (file-name-concat total-recall--root-path "python"))

  (defconst total-recall--bin-path
    (file-name-concat total-recall--root-path "bin"))

  (defcustom total-recall-python "python3"
    ""
    :type 'string
    :group 'total-recall)

  (defcustom total-recall-python-venv
    (file-name-concat total-recall-dir "venv")
    ""
    :type 'string
    :group 'total-recall)

  (defun total-recall--check-python-version (cmd)
    (let
        (
         (cmd-path (executable-find cmd))
         version-output)

      (when (null cmd-path)
        (error
         "CMD does not refer to a Python executable. cmd = %s"
         cmd))

      (setq version-output
            (shell-command-to-string (concat cmd-path " --version")))

      (if
          (string-match
           "Python \\([0-9]+\\.[0-9]+\\(\\.[0-9]+\\)?\\)"
           version-output)
          (let ((version (match-string 1 version-output)))
            (when (not (version<= "3.11" version))
              (error
               "TOTAL-RECALL requires Python 3.8 or higher; `%s' is version %s"
               cmd-path
               version)))
        (error
         "Cannot determine Python version for `%s'. Ensure it is installed and in PATH"
         cmd-path))))

  (defun total-recall--conf-init ()
    (lambda (msg)
      (pcase msg
        (:root total-recall-root-dir)

        (:python-lib total-recall--lib-python-path)

        (:threshold total-recall-threshold)

        (:height total-recall-window-height)

        (:width total-recall-window-width)

        (:failure-key total-recall-key-failure)

        (:quit-key total-recall-key-quit)

        (:reveal-key total-recall-key-reveal)

        (:skip-key total-recall-key-skip)

        (:success-key total-recall-key-success)

        (:db-path total-recall-database)

        (:env total-recall-env)

        (:ripgrep total-recall-ripgrep-cmd)

        (:def-type total-recall-def-type)

        (:ex-type total-recall-ex-type)

        (:io-buffer-name total-recall-io-buffer-name)

        (:buffer-name total-recall-buffer-name)

        (:python
         (total-recall--check-python-version total-recall-python)
         total-recall-python)

        (:python-venv total-recall-python-venv)

        (:dir total-recall-dir)

        (:install-python-env
         (total-recall--check-python-version total-recall-python)
         (let* ((venv-path total-recall-python-venv)
                (venv-bin-dir
                 (if (eq system-type 'windows-nt)
                     (file-name-concat venv-path "Scripts")
                   (file-name-concat venv-path "bin")))
                (pip-executable
                 (file-name-concat venv-bin-dir
                                   (if (eq system-type 'windows-nt)
                                       "pip.exe"
                                     "pip")))
                (requirements-file
                 (file-name-concat total-recall--lib-python-path
                                   "prod-requirements.txt")))

           (unless (file-directory-p venv-path)
             (message "Creating virtual environment at %s..."
                      venv-path)
             (let ((create-venv-cmd
                    (format "%s -m venv %s"
                            total-recall-python
                            (shell-quote-argument venv-path))))
               (if (zerop (shell-command create-venv-cmd))
                   (message
                    "Virtual environment created successfully.")
                 (error
                  "Failed to create virtual environment at %s"
                  venv-path))))

           (message "Installing dependencies from %s..."
                    requirements-file)
           (let ((install-cmd
                  (format "%s install -r %s"
                          (shell-quote-argument pip-executable)
                          (shell-quote-argument requirements-file))))
             (if (zerop (shell-command install-cmd))
                 (message "Dependencies installed successfully.")
               (error
                "Failed to install dependencies from %s"
                requirements-file)))))

        (:graph-exec-path (file-name-concat total-recall--bin-path "graph"))

        (:selector-exec-path (file-name-concat total-recall--bin-path "selector"))

        (_ (error "Unexpected message. msg = %s" msg)))))

  ;; Interface

  (defun total-recall--conf-mk ()
    (total-recall--conf-init))

  (defun total-recall--conf-root (conf)
    (send conf :root))

  (defun total-recall--conf-python-lib (conf)
    (send conf :python-lib))

  (defun total-recall--conf-threshold (conf)
    (send conf :threshold))

  (defun total-recall--conf-env (conf)
    (send conf :env))

  (defun total-recall--conf-db-path (conf)
    (send conf :db-path))

  (defun total-recall--conf-def-type (conf)
    (send conf :def-type))

  (defun total-recall--conf-ex-type (conf)
    (send conf :ex-type))

  (defun total-recall--conf-ripgrep (conf)
    (send conf :ripgrep))

  (defun total-recall--conf-string (conf)
    (send conf :string))

  (defun total-recall--conf-failure-key (conf)
    (send conf :failure-key))

  (defun total-recall--conf-success-key (conf)
    (send conf :success-key))

  (defun total-recall--conf-quit-key (conf)
    (send conf :quit-key))

  (defun total-recall--conf-skip-key (conf)
    (send conf :skip-key))

  (defun total-recall--conf-reveal-key (conf)
    (send conf :reveal-key))

  (defun total-recall--conf-height (conf)
    (send conf :height))

  (defun total-recall--conf-width (conf)
    (send conf :width))

  (defun total-recall--conf-buffer-name (conf)
    (send conf :buffer-name))

  (defun total-recall--conf-io-buffer-name (conf)
    (send conf :io-buffer-name))

  (defun total-recall--conf-python (conf)
    (send conf :python))

  (defun total-recall--conf-python-venv (conf)
    (send conf :python-venv))

  (defun total-recall--conf-dir (conf)
    (send conf :dir))

  (defun total-recall--conf-install-python-env (conf)
    (send conf :install-python-env))

  (defun total-recall--conf-graph-exec-path (conf)
    (send conf :graph-exec-path))

  (defun total-recall--conf-selector-exec-path (conf)
    (send conf :selector-exec-path)))


(provide 'total-recall-conf)

;;; total-recall-conf.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
