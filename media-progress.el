;;; media-progress.el --- Display position where media player stopped -*- lexical-binding:t -*-
;; Copyright (C) 2023  Dmitriy Pshonko

;; Author: Dmitriy Pshonko <http://github.com/jumper047>
;; Version: 0.1.0
;; Keywords: files, convenience
;; Homepage: https://github.com/jumper047/media-progress

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

;; This package provides function to extract progress time
;; from the media player config for displaying it in the file manager buffer.
;; keep-open=yes
;; save-position-on-quit=yes

;;; Code:

(defgroup media-progress nil
  "Display position where mpv player stopped."
  :group 'dired
  :prefix "media-progress-")

(defcustom media-progress-mpv-cfg-dir "~/.config/mpv"
  "Location of the mpv config directory."
  :type '(directory)
  :group 'media-progress)

(defcustom media-progress-prefer-with-path nil
  "Which type of hash to prefer if mpv has both?
When mpv saves position for current file, it can use
full path or just filename to identify the file (this
behavior can be controlled through config file or
command line switches). This plugin will check both ways
and display the one it'll found. This parameter controls
behavior of the plugin when both types met simultaneously."
  :type '(bool)
  :group 'media-progress)

(defcustom media-progress-completed-threshold 0.95
  "Percent of the progress treated as \"completed\".
\(value should be between 0 and 0.99\)"
  :type '(float)
  :group 'media-progress)

(defvar media-progress-mpv-watch-later-dir-name "watch_later"
  "Name of the directory inside mpv cfg dir containing watch_later files.")

(defvar media-progress-format "Progress: %s %%"
  "Message with current progress in percents.")

(defvar media-progress-completed-message "Completed"
  "Message to indicate file was watched till the end.")

(defvar media-progress-fallback-format "Stopped at: %s"
  "Message with absolute position in case mediainfo is not installed.")


(defvar media-progress-watched-time-format "%h:%.2m:%.2s"
  "Format of the time used in fallback message.")

(defvar media-progress-mediainfo-command "mediainfo"
  "Name or path to the \"mediainfo\" binary.")

(defvar media-progress-mediainfo-args "--Inform=\"General;%Duration%\""
  "Arguments to extract duration from media file with \"mediainfo\".")

(defun media-progress--get-watch-later-file (media-file)
  "Find path to the \"watch_later\" mpv file for for MEDIA-FILE."
  (let* ((possible-filenames (list media-file (file-name-nondirectory media-file)))
         (wl-filenames (mapcar (lambda (f) (upcase (md5 f))) possible-filenames))
         (wl-files (mapcar (lambda (f) (file-name-concat
                                        media-progress-mpv-cfg-dir
                                        media-progress-mpv-watch-later-dir-name f))
                           wl-filenames))
         ;; Later we'll go through list and get last existing. If we prefer
         ;; hashes with path, we should check it lately, because last successful
         ;; will be used
         (wl-files (if media-progress-prefer-with-path (reverse wl-files) wl-files))
         selected-wl-file)
    (dolist (wl-file wl-files selected-wl-file)
      (if (file-exists-p wl-file)
          (setq selected-wl-file wl-file)))))

(defun media-progress--parse-wl-line (line)
  "Parse LINE of the \"watch_later\" file as \(key . value\)."
  (let* ((key-val (string-split line "="))
         (key (intern (car key-val)))
         (val (cadr key-val)))
    (cons key val)))

(defun media-progress--extract-pos (wl-file)
  "Extract saved position \(as number of seconds\) from WL-FILE."
  (let* ((wl-file-content (with-temp-buffer
                            (insert-file-contents wl-file)
                            (buffer-string)))
         (wl-lines (string-split wl-file-content "\n"))
         (wl-alist (mapcar #'media-progress--parse-wl-line wl-lines)))
    (string-to-number (alist-get 'start wl-alist))))

(defun media-progress--get-duration (media-file)
  "Get duration of the MEDIA-FILE if mediainfo binary available."
  (when (executable-find media-progress-mediainfo-command)
    (/ (string-to-number
        (shell-command-to-string
         (string-join
          `(,media-progress-mediainfo-command
            ,media-progress-mediainfo-args
            ,(shell-quote-argument media-file)) " "))) 1000.0)))

(defun media-progress-info-string (media-file)
  "Get progress string for MEDIA-FILE if possible."
  (when-let* ((wl-file (media-progress--get-watch-later-file media-file)))

    ;; current-pos duration completed format-str subst)
    (let  ((current-pos (media-progress--extract-pos wl-file))
           (duration (media-progress--get-duration media-file)))
      (if duration
          (if (>= (/ (float current-pos) duration) media-progress-completed-threshold) ;completed if t
              media-progress-completed-message
            (format media-progress-format (round (* 100 (/ (float current-pos) duration)))))
        (format media-progress-fallback-format
                (format-seconds media-progress-watched-time-format current-pos))))))

(provide 'media-progress)
;;; media-progress.el ends here
