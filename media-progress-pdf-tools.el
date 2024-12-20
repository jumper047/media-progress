;;; media-progress-pdf-tools.el --- Display position where pdf-tools stopped
;; Copyright (C) 2023  Dmitriy Pshonko

;; Author: Dmitriy Pshonko <http://github.com/jumper047>
;; Keywords: files, convenience

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Package gets information about reading progress
;; of the pdf files opened by pdf-tools.
;; It required saveplace-pdf-view plugin to be installed and
;; set up, see https://github.com/nicolaisingh/saveplace-pdf-view

;;; Code:

;;; -*- lexical-binding:t -*-

(require 'subr-x)
(require 'media-progress-cache)

(declare-function pdf-info-number-of-pages "ext:pdf-info")
(defvar save-place-mode)
(defvar save-place-alist)

(defgroup media-progress-pdf-tools nil
  "Extract position information from mpv player."
  :group 'media-progress
  :prefix "media-progress-pdf-tools-")

(defcustom media-progress-pdf-tools-extensions '("pdf")
  "List of the extensions which should be checked for progress.
If you want to check all files - set variable to nil
\(not recommended for performance reasons\)"
  :type '(list)
  :group 'media-progress-pdf-tools)

(defun media-progress-pdf-tools--media-p (file)
  "Check if FILE should be checked for progress."
  (if (and (not (file-directory-p file)) media-progress-pdf-tools-extensions)
      (let ((ext (file-name-extension file)))
        (member ext media-progress-pdf-tools-extensions))))

(defun media-progress-pdf-tools-get-current-page (file)
  (when-let* ((mode-enabled save-place-mode)
              (bookmark (alist-get file save-place-alist nil nil 'equal)))
    (cdr (assq 'page (cddr (alist-get 'pdf-view-bookmark (aref bookmark 0) nil nil 'equal))))))

(defun media-progress-pdf-tools-get-number-of-pages (file)
  (if (fboundp 'pdf-info-number-of-pages)
      (pdf-info-number-of-pages file)))
(media-progress-cache-wrap-function #'media-progress-pdf-tools-get-number-of-pages)

(defun media-progress-pdf-tools-info (media-file)
  "Get progress info for MEDIA-FILE if possible.
Return (plugin-name current-pos-str duration-str progress)
or nil if no info found."

  (when-let* ((media-p (media-progress-pdf-tools--media-p media-file))
              (current-page (media-progress-pdf-tools-get-current-page media-file)))

    (let ((number-of-pages (media-progress-pdf-tools-get-number-of-pages media-file))
          progress)
      (when number-of-pages
        (setq progress (/ (float current-page) number-of-pages)))
      (list
       'pdf-tools
       (number-to-string current-page)
       (and number-of-pages (number-to-string number-of-pages))
       progress))))

(provide 'media-progress-pdf-tools)
;;; media-progress-pdf-tools.el ends here
