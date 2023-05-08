;;; mpvmedia-pos.el --- Display position where you stopped playing media in mpv -*- lexical-binding:t -*-

;;; Code:

(defvar mpvmedia-pos-watch-later-dir "~/.config/mpv/watch_later/")
(defvar mpvmedia-pos-prefer-with-path nil)

(defvar mpvmedia-pos-format "Progress: %s%%")
(defvar mpvmedia-pos-fallback-format "Stopped at: %s")
(defvar mpvmedia-pos-completed-message "Completed")
(defvar mpvmedia-pos-completed-threshold 0.95)
(defvar mpvmedia-pos-watched-time-format "%h:%.2m:%.2s")
(defvar mpvmedia-pos-mediainfo-command "mediainfo")
(defvar mpvmedia-pos-mediainfo-args "--Inform=\"General;%Duration%\"")

(defun mpvmedia-pos--get-watch-later-file (media-file)
  (let* (;; (file (expand-file-name file))
         (possible-filenames (list media-file (file-name-nondirectory media-file)))
         (wl-filenames (mapcar (lambda (f) (upcase (md5 f))) possible-filenames))
         (wl-files (mapcar (lambda (f) (file-name-concat mpvmedia-pos-watch-later-dir f)) wl-filenames))
         ;; Later we'll go through list and get last existing. If we prefer
         ;; hashes with path, we should check it lately, because last successful
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
    (string-to-number (alist-get 'start wl-alist))))

(defun mpvmedia-pos--get-duration (media-file)
  (when (executable-find mpvmedia-pos-mediainfo-command)
    (/ (string-to-number
        (shell-command-to-string
         (string-join
          `(,mpvmedia-pos-mediainfo-command
            ,mpvmedia-pos-mediainfo-args
            ,(shell-quote-argument media-file)) " "))) 1000.0)))

(defun mpvmedia-pos-info-string (media-file)
  (when-let* ((wl-file (mpvmedia-pos--get-watch-later-file media-file)))

    ;; current-pos duration completed format-str subst)
    (let  ((current-pos (mpvmedia-pos--extract-pos wl-file))
           (duration (mpvmedia-pos--get-duration media-file)))
      (if duration
          (if (>= (/ (float current-pos) duration) mpvmedia-pos-completed-threshold) ;completed if t
              mpvmedia-pos-completed-message
            (format mpvmedia-pos-format (round (* 100 (/ (float current-pos) duration)))))
        (format mpvmedia-pos-fallback-format
                (format-seconds mpvmedia-pos-watched-time-format current-pos))))))

(provide 'mpvmedia-pos)
;;; mpvmedia-pos.el ends here
