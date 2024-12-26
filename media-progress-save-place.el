;;; media-progress-save-place.el --- Mark files saved by save-place-mode
;; Copyright (C) 2024  Dmitriy Pshonko

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
;; of files mentioned in save-place-alist
;; It requires saveplace-mode to be installed and set up.

;;; Code:

;;; -*- lexical-binding:t -*-

(require 'subr-x)
(require 'media-progress-cache)

(defvar save-place-mode)
(defvar save-place-alist)
(defvar save-place-limit)

(defgroup media-progress-save-place nil
  "Mark files mentioned by saveplace mode."
  :group 'media-progress
  :prefix "media-progress-save-place-")

(defcustom media-progress-save-place-enabled 't
  "Enable displaying mark on files saved by saveplace mode."
  :type '(boolean)
  :group 'media-progress-save-place)

(defvar media-progress-save-place-entries nil)
(defvar media-progress-save-place-initialized nil)

(defun media-progress-save-place-read-all ()
  "Read all files from `save-place-alist' into hashmap."
  (setq media-progress-save-place-entries
        (make-hash-table :size save-place-limit :test 'equal))
  (dolist (save-place-element save-place-alist)
    (let ((elt-path (car save-place-element)))
      (unless (or (file-remote-p elt-path) (file-directory-p elt-path))
        (puthash elt-path 't media-progress-save-place-entries)))))

(defun media-progress-save-place-init-maybe ()
  "Initialize plugin if `save-place-mode' activated.
Returns 't if initialized successfully."
  (when save-place-mode
    (media-progress-save-place-read-all)
    (setq media-progress-save-place-initialized 't)))

(defun media-progress-save-place-file-opened (file)
  "Check if FILE was opened previously."
  (gethash file media-progress-save-place-entries nil))

(defun media-progress-save-place-info (file)
  "Get progress info for FILE if possible.
Return ('save-place nil nil nil) if file found or nil otherwise."
  (when (and (or media-progress-save-place-initialized (media-progress-save-place-init-maybe))
             (media-progress-save-place-file-opened file))
    (list 'save-place nil nil nil)))

(provide 'media-progress-save-place)
;;; media-progress-save-place.el ends here
