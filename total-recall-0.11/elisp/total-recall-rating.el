;;; total-recall-rating.el --- Rating  -*- lexical-binding: t; coding:utf-8 -*-

;; :ID: 72db9687-443c-45ba-a40c-d99909a8006a

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

;; This code defines Rating, a type of objects represents a rating to its clients.

;;; Code:

;; Context

(require 'total-recall-utils)

(total-recall--context
  (epoch-p uuid-p)

  (defconst total-recall--rating-value '(:success :failure :skip))

  (defun total-recall--rating-value-p (x)
    (memq x total-recall--rating-value))

  (defun total-recall--rating-init (epoch id value)
    (unless (epoch-p epoch)
      (error "EPOCH is not an Epoch. epoch = %s" epoch))

    (unless (uuid-p id)
      (error "ID is not an UUID. id = %s" id))

    (unless (total-recall--rating-value-p value)
      (error "VALUE is not a Value. value = %s" value))

    (record 'total-recall--rating epoch id value))

  (defun total-recall--rating-elim (f)
    (lambda (rating)
      (unless (total-recall--rating-p rating)
        (error "RATING is not a Rating. rating = %s" rating))
      (funcall f (aref rating 1) (aref rating 2) (aref rating 3))))

  (defconst total-recall--rating->list
    (total-recall--rating-elim
      (lambda (epoch id value) `(,epoch ,id ,value))))

  ;; Interface

  (defun total-recall--rating-mk (epoch id value)
    (total-recall--rating-init epoch id value))

  (defun total-recall--rating-p (obj)
    (and (recordp obj) (eq (type-of obj) 'total-recall--rating)))

  (defun total-recall--rating-success-mk (epoch id)
    (total-recall--rating-mk epoch id :success))

  (defun total-recall--rating-failure-mk (epoch id)
    (total-recall--rating-mk epoch id :failure))

  (defun total-recall--rating-skip-mk (epoch id)
    (total-recall--rating-mk epoch id :skip))

  (defun total-recall--rating-data (rating)
    (funcall total-recall--rating->list rating)))

(provide 'total-recall-rating)

;;; total-recall-rating.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
