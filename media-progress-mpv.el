;;; media-progress-mpv.el --- Display position where mpv stopped
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

;; Package gets information about viewing progress
;; of the media files played by mpv.
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

;;; Code:

;;; -*- lexical-binding:t -*-

(require 'subr-x)

(defgroup media-progress-mpv nil
  "Extract position information from mpv player."
  :group 'media-progress
  :prefix "media-progress-mpv-")

(defcustom media-progress-mpv-cfg-dir (cond ((eq system-type "windows-nt")
                                             "~/mpv")
                                            ('t
                                             "~/.config/mpv"))
  "Location of the mpv config directory."
  :type '(directory)
  :group 'media-progress)

(defcustom media-progress-mpv-prefer-with-path nil
  "Which type of hash to prefer if mpv has both?
When mpv saves position for current file, it can use
full path or just filename to identify the file (this
behavior can be controlled through config file or
command line switches). This plugin will check both ways
and display the one it'll found. This parameter controls
behavior of the plugin when both types met simultaneously."
  :type '(bool)
  :group 'media-progress)

;; Shamelessly borrowed from dirvish - see dirvish-video-exts constant
(defcustom media-progress-mpv-extensions '("f4v" "rmvb" "wvx" "wmx" "wmv" "wm" "asx" "mk3d" "mkv" "fxm" "flv" "axv" "webm" "viv" "yt" "s1q" "smo" "smov" "ssw" "sswf" "s14" "s11" "smpg" "smk" "bk2" "bik" "nim" "pyv" "m4u" "mxu" "fvt" "dvb" "uvvv" "uvv" "uvvs" "uvs" "uvvp" "uvp" "uvvu" "uvu" "uvvm" "uvm" "uvvh" "uvh" "ogv" "m2v" "m1v" "m4v" "mpg4" "mp4" "mjp2" "mj2" "m4s" "3gpp2" "3g2" "3gpp" "3gp" "avi" "mov" "movie" "mpe" "mpeg" "mpegv" "mpg" "mpv" "qt" "vbs")
  "List of the extensions which should be checked for progress.
If you want to check all files - set variable to nil
\(not recommended for performance reasons\)"
  :type '(list)
  :group 'media-progress-mpv)

(defcustom media-progress-completed-threshold 0.95
  "Percent of the progress treated as \"completed\".
\(value should be between 0 and 0.99\)"
  :type '(float)
  :group 'media-progress)

(defvar media-progress-mpv-watch-later-dir-name "watch_later"
  "Name of the directory inside mpv cfg dir containing watch_later files.")

(defvar media-progress-mpv-watched-time-format "%h:%.2m:%.2s"
  "Format of the time used in fallback message.")

(defvar media-progress-mpv-mediainfo-command "mediainfo"
  "Name or path to the \"mediainfo\" binary.")

(defvar media-progress-mpv-mediainfo-args "--Inform=\"General;%Duration%\""
  "Arguments to extract duration from media file with \"mediainfo\".")

(defun media-progress-mpv--media-p (file)
  "Check if FILE should be checked for progress."
  (if (and (not (file-directory-p file)) media-progress-mpv-extensions)
      (let ((ext (file-name-extension file)))
        (member ext media-progress-mpv-extensions))))

(defun media-progress-mpv--get-watch-later-file (media-file)
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
         (wl-files (if media-progress-mpv-prefer-with-path (reverse wl-files) wl-files))
         selected-wl-file)
    (dolist (wl-file wl-files selected-wl-file)
      (if (file-exists-p wl-file)
          (setq selected-wl-file wl-file)))))

(defun media-progress-mpv--parse-wl-line (line)
  "Parse LINE of the \"watch_later\" file as \(key . value\)."
  (let* ((key-val (split-string line "="))
         (key (intern (car key-val)))
         (val (cadr key-val)))
    (cons key val)))

(defun media-progress-mpv--extract-pos (wl-file)
  "Extract saved position \(as number of seconds\) from WL-FILE."
  (let* ((wl-file-content (with-temp-buffer
                            (insert-file-contents wl-file)
                            (buffer-string)))
         (wl-lines (split-string wl-file-content "\n"))
         (wl-alist (mapcar #'media-progress-mpv--parse-wl-line wl-lines)))
    (string-to-number (alist-get 'start wl-alist))))

(defun media-progress-mpv--get-duration (media-file)
  "Get duration of the MEDIA-FILE if mediainfo binary available."
  (when (executable-find media-progress-mpv-mediainfo-command)
    (/ (string-to-number
        (shell-command-to-string
         (string-join
          `(,media-progress-mpv-mediainfo-command
            ,media-progress-mpv-mediainfo-args
            ,(shell-quote-argument media-file)) " "))) 1000.0)))

(defun media-progress-mpv-info (media-file)
  "Get progress info for MEDIA-FILE if possible.
Return (current-pos-str duration-str progress-percentage)
or nil if no info found."
  (when-let* ((media-p (media-progress-mpv--media-p media-file))
              (wl-file (media-progress-mpv--get-watch-later-file media-file)))
    (let* ((current-pos (media-progress-mpv--extract-pos wl-file))
           (current-pos-str (format-seconds media-progress-mpv-watched-time-format current-pos))
           (duration (media-progress-mpv--get-duration media-file))
           percentage
           duration-str)
      (when duration
        (setq
         percentage (/ (float current-pos) duration)
         duration-str (format-seconds media-progress-mpv-watched-time-format duration)))
      (list current-pos-str duration-str percentage))))

(provide 'media-progress-mpv)
;;; media-progress-mpv.el ends here
