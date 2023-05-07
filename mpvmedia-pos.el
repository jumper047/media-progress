;;; mpvmedia-pos.el --- See position you stopped playing media in mpv -*- lexical-binding:t -*-
;;; Code:

(defvar mpvmedia-pos-watch-later-dir "/home/psh/.config/mpv/watch_later/")
(defvar mpvmedia-pos-prefer-with-path nil)

(defvar mpvmedia-pos-format " [mpv stopped at: %s ]")
(defvar mpvmedia-pos-watched-time-format "%h:%.2m:%.2s")

(defun mpvmedia-pos-watch-later-file (media-file)
  (let* (;; (file (expand-file-name file))
         (possible-filenames (list media-file (file-name-nondirectory media-file)))
         (wl-filenames (mapcar (lambda (f) (upcase (md5 f))) possible-filenames))
         (wl-files (mapcar (lambda (f) (file-name-concat mpvmedia-pos-watch-later-dir f)) wl-filenames))
         ;; Later we'll go through list and get last existing. If we prefer
         ;; hashes with path, we should check it lately, because last successfull
         ;; will be used
         (wl-files (if mpvmedia-pos-prefer-with-path (reverse wl-files) wl-files))
         selected-wl-file)
    (dolist (wl-file wl-files selected-wl-file)
      (if (file-exists-p wl-file)
          (setq selected-wl-file wl-file)))))

(defun mpvmedia-pos--parse-wl-line (line)
  (let* ((key-val (string-split line "="))
         (key (intern (car key-val)))
         (val (cadr key-val)))
    (cons key val)))

(defun mpvmedia-pos--extract-pos (wl-file)
  (let* ((wl-file-content (with-temp-buffer
                            (insert-file-contents wl-file)
                            (buffer-string)))
         (wl-lines (string-split wl-file-content "\n"))
         (wl-alist (mapcar #'mpvmedia-pos--parse-wl-line wl-lines)))
    (format-seconds
     mpvmedia-pos-watched-time-format
     (string-to-number (alist-get 'start wl-alist)))))

(defun mpvmedia-pos-info-string (media-file)
  (when-let* ((wl-file (mpvmedia-pos-watch-later-file media-file))
              (mpvmedia-pos (mpvmedia-pos--extract-pos wl-file)))
    (format mpvmedia-pos-format mpvmedia-pos)))

(provide 'mpvmedia-pos)
;;; mpvmedia-pos.el ends here
