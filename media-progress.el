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
(require 'media-progress-pdf-tools)
(require 'media-progress-save-place)

(declare-function nerd-icons-mdicon "ext:nerd-icons")

(defgroup media-progress nil
  "Display position where mpv player stopped."
  :group 'dired
  :prefix "media-progress-")

(defcustom media-progress-display-function 'media-progress-display-plain
  "Function used to display progress.
Function should receive 4 parameters:
- plugin name, hinting on data source
- current position as string
- media length/number of pages/etc, optional
- media progress, number in range of 0 to 0.99, optional.
Function should return string to display in file manager"
  :type 'function
  :group 'media-progress)

(defcustom media-progress-completed-threshold nil
  "Progress treated as \"completed\".
\(value should be between 0 and 0.99\)
Attention! This variable is obsolete and will be
dropped in foreseeing future! Please delete it from your
configuration files and use `media-progress-completed-percentage'
instead!"
  :type '(integer)
  :group 'media-progress)

(defcustom media-progress-completed-percentage 95
  "Progress treated as \"completed\".
\(value should be between 0 and 99\)"
  :type '(integer)
  :group 'media-progress)

(if media-progress-completed-threshold
    (setq media-progress-completed-percentage
          (round (* 100 media-progress-completed-threshold))))

(defvar media-progress-format "progress: %s%%"
  "Message with current progress in percents.")

(defvar media-progress-completed-message "completed"
  "Message to indicate file was watched till the end.")

(defvar media-progress-fallback-format "stopped at: %s"
  "Message with absolute position in case mediainfo is not installed.")

(defun media-progress-display-plain (plugin pos len progress)
  (if (not (and len progress))
      (format media-progress-fallback-format pos)
    (if (>= progress media-progress-completed-percentage)
        media-progress-completed-message
      (format media-progress-format progress))))

(defun media-progress-display-icons (plugin pos len progress)
  (if (not (and len progress))
      (cond ((eq plugin 'save-place) (format " %s " (nerd-icons-mdicon "nf-md-eye")))
            ((eq plugin 'pdf-tools) (format " %s p. %s " (nerd-icons-mdicon "nf-md-eye") pos))
            ((eq plugin 'mpv) (format " %s %s" (nerd-icons-mdicon "nf-md-eye") pos)))
    (if (> progress media-progress-completed-percentage) (format " %s " (nerd-icons-mdicon "nf-md-check"))
      (cond ((eq plugin 'pdf-tools) (format " %s p. %s/%s " (nerd-icons-mdicon "nf-md-eye") pos len))
            ((eq plugin 'mpv) (format " %s %s%% " (nerd-icons-mdicon "nf-md-eye") progress))))))

(defun media-progress-display-icons-minimal (plugin pos len progress)
  (if (and progress (> progress media-progress-completed-percentage))
      (format " %s " (nerd-icons-mdicon "nf-md-check"))
    (format " %s " (nerd-icons-mdicon "nf-md-eye"))))

(defun media-progress-info-string (media-file)
  "Get progress string for MEDIA-FILE if possible.
Return an empty string if no info found."
  (if-let* ((media-info (cond ((and
                                media-progress-mpv-enabled
                                (media-progress-mpv-info media-file)))
                              ((and
                                media-progress-pdf-tools-enabled
                                (media-progress-pdf-tools-info media-file)))
                              ((and
                                media-progress-save-place-enabled
                                (media-progress-save-place-info media-file))))))
      (funcall media-progress-display-function (car media-info) (cadr media-info) (caddr media-info) (cadddr media-info))
    ""))

(provide 'media-progress)
;;; media-progress.el ends here
