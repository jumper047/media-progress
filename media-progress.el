;;; media-progress.el --- Display position where media player stopped
;; Copyright (C) 2023  Dmitriy Pshonko

;; Author: Dmitriy Pshonko <http://github.com/jumper047>
;; Version: 0.1.0
;; Keywords: files, convenience
;; Homepage: https://github.com/jumper047/media-progress
;; Package-Requires: ((emacs "28.1"))

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

;; Package gets information about viewing progress of the media files.
;; It parses data, saved by media players - for now only mpv player supported.
;;
;; Preparations:
;; Install mpv player. You can save position on exit manually using
;; "Shift-Q" shortcut, or add these lines to the mpv config:
;;
;; keep-open=yes
;; save-position-on-quit=yes
;;
;; Optionally - install "mediainfo" app to display progress and "completed"
;; status.
;;
;; Usage:
;; Package provides function `media-progress-info-string' which will
;; return progress string for media file.

;;; Code:

;;; -*- lexical-binding:t -*-

(require 'subr-x)
(require 'media-progress-mpv)

(defgroup media-progress nil
  "Display position where mpv player stopped."
  :group 'dired
  :prefix "media-progress-")

(defcustom media-progress-display-function 'media-progress-make-string
  "Function used to display progress.
Function should receive 4 parameters:
- plugin name, hinting on data source
- current position as string
- media length/number of pages/etc, optional
- media progress, number in range of 0 to 0.99, optional.
Function should return string to display in file manager"
  :type 'function
  :group 'media-progress)

(defvar media-progress-format "Progress: %s%%"
  "Message with current progress in percents.")

(defvar media-progress-completed-message "Completed"
  "Message to indicate file was watched till the end.")

(defvar media-progress-fallback-format "Stopped at: %s"
  "Message with absolute position in case mediainfo is not installed.")

(defun media-progress-make-string (plugin pos len progress)
  (if (not (and len progress))
      (format media-progress-fallback-format pos)
    (if (>= progress media-progress-completed-threshold)
        media-progress-completed-message
      (format media-progress-format (round (* 100 progress))))))

(defcustom media-progress-completed-threshold 0.95
  "Progress treated as \"completed\".
\(value should be between 0 and 0.99\)"
  :type '(float)
  :group 'media-progress)

(defun media-progress-info-string (media-file)
  "Get progress string for MEDIA-FILE if possible.
Return an empty string if no info found."
  (if-let* ((media-info (cond ((media-progress-mpv-info media-file))
            (media-plugin (car media-info))
            (media-pos (cadr media-info))
            (media-length (caddr media-info))
            (media-progress (cadddr media-info)))
      (funcall media-progress-display-function media-plugin media-pos media-length media-progress)
    ""))

(provide 'media-progress)
;;; media-progress.el ends here
