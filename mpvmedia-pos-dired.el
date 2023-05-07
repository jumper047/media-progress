;;; mpvmedia-pos-dired.el --- See position you stopped playing media in mpv in dired buffer -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:

(require 'dired)
(require 'dired-x)
(require 'dired-aux)
(require 'mpvmedia-pos)

(defgroup mpvmedia-pos-dired nil
  "Show git info in Dired."
  :group 'files
  :prefix "mpvmedia-pos-dired-")

(defface mpvmedia-pos-dired-mpv-info-face
  '((t (:inherit font-lock-comment-face)))
  "Face for commit message.")

(defcustom mpvmedia-pos-dired-auto-hide-details-p t
  "If details should get hidden automatically.

Uses function `dired-hide-details-mode' to hide details when showing mpv
info."
  :type 'boolean)

(defvar-local mpvmedia-pos-dired--restore-no-details nil
  "If no details view has to be restored.")

(defun mpvmedia-pos-dired--files ()
  "Return list of file names in Dired buffer."
  (save-excursion
    (let (files)
      (goto-char 1)
      (while (not (eobp))
        (when (dired-file-name-at-point)
          (push (expand-file-name (dired-file-name-at-point)) files))
        (dired-next-line 1))
      (nreverse files))))

(defun mpvmedia-pos-dired--insert-info ()
  "Insert git log messages into current buffer."
  (let* ((files (mpvmedia-pos-dired--files))
         (longest (mpvmedia-pos-dired--longest-line files))
         mpv-info)
    (save-excursion
      (with-silent-modifications
        (dolist (file files)
          (dired-goto-file file)
          (when (dired-file-name-at-point)
            (dired-move-to-end-of-filename t)
            (setq mpv-info (mpvmedia-pos-info-string file))
            (if mpv-info (insert (concat (make-string (1+ (- longest (length file))) ?\s) mpv-info)))))
        (mpvmedia-pos-dired--fontify-info (point-max))))))


(defun mpvmedia-pos-dired--longest-line (lines)
  "Find longest line length in a list of LINES."
  (let ((longest 0) length)
    (dolist (l lines)
      (setq length (length l))
      (if (> length longest) (setq longest length)))
    longest))

(defun mpvmedia-pos-dired--remove-info ()
  "Renove inserted git log messages."
  (save-excursion
    (with-silent-modifications
      (goto-char 1)
      (while (not (eobp))
        (when (dired-file-name-at-point)
          (dired-move-to-end-of-filename)
          (delete-region (point) (line-end-position)))
        (dired-next-line 1)))))

(defun mpvmedia-pos-dired--after-readin ()
  "mpvmedia-pos-dired hook called after a Dired buffer is modified."
  (mpvmedia-pos-dired--insert-info))

(defun mpvmedia-pos-dired--before-readin ()
  "mpvmedia-pos-dired hook called before a Dired buffer is modified."
  (when (bound-and-true-p mpvmedia-pos-dired-mode)
    (mpvmedia-pos-dired--remove-info)))

(defun mpvmedia-pos-dired--revert-buffer (&rest _r)
  "Recalculate git info after buffer is reverted."
  (when (bound-and-true-p mpvmedia-pos-dired-mode)
    (mpvmedia-pos-dired--remove-info)
    (mpvmedia-pos-dired--insert-info)))

(defun mpvmedia-pos-dired--fontify-info (limit)
  "Fonitfy inserted git info in Dired buffer.

LIMIT as required by font-lock hook."
  (save-excursion
    (goto-char (point-min))
    (while (< (point) limit)
      (when (dired-file-name-at-point)
        (dired-move-to-end-of-filename)
        (when (< (point) (line-end-position))
          (add-text-properties
           (1+ (point)) (line-end-position)
           (list 'font-lock-fontified t
                 'face 'mpvmedia-pos-dired-commit-message-face))))
      (dired-next-line 1))))

(defun mpvmedia-pos-dired--after-mode ()
  (when mpvmedia-pos-dired-auto-hide-details-p
    (unless (bound-and-true-p dired-hide-details-mode)
      (setq mpvmedia-pos-dired--restore-no-details t)
      (dired-hide-details-mode))))

(defun mpvmedia-pos-dired--on-first-change ()
  (mpvmedia-pos-dired--fontify-info (point-max)))

(defun mpvmedia-pos-dired--cleanup ()
  "Remove commit messages."
  (mpvmedia-pos-dired--remove-info)
  (when mpvmedia-pos-dired--restore-no-details
    (setq mpvmedia-pos-dired--restore-no-details nil)
    (dired-hide-details-mode -1))
  (advice-remove 'dired-revert #'mpvmedia-pos-dired--revert-buffer)
  (remove-hook 'dired-before-readin-hook #'mpvmedia-pos-dired--before-readin t)
  (remove-hook 'dired-after-readin-hook #'mpvmedia-pos-dired--after-readin t)
  (remove-hook 'after-change-major-mode-hook #'mpvmedia-pos-dired--after-mode t)
  (remove-hook 'first-change-hook #'mpvmedia-pos-dired--on-first-change t)
  (setq font-lock-keywords
        (remove '(mpvmedia-pos-dired--fontify-info) font-lock-keywords)))

;;;###autoload
(define-minor-mode mpvmedia-pos-dired-mode
  "Toggle git message info in current Dired buffer."
  :lighter " dgl"
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a Dired buffer"))
  (cond
   ((not mpvmedia-pos-dired-mode)
    (mpvmedia-pos-dired--cleanup))
   (t
    (font-lock-add-keywords nil '(mpvmedia-pos-dired--fontify-info))
    (advice-add 'dired-revert :after #'mpvmedia-pos-dired--revert-buffer)
    (add-hook 'dired-before-readin-hook #'mpvmedia-pos-dired--before-readin nil t)
    (add-hook 'dired-after-readin-hook #'mpvmedia-pos-dired--after-readin nil t)
    (add-hook 'after-change-major-mode-hook #'mpvmedia-pos-dired--after-mode nil t)
    (add-hook 'first-change-hook #'mpvmedia-pos-dired--on-first-change nil t)
    (mpvmedia-pos-dired--insert-info))))

(provide 'mpvmedia-pos-dired)
;;; mpvmedia-pos-dired.el ends here
