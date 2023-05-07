;; dired-mpv-pos-dirvish.el --- -*- lexical-binding: t -*-

;;; Code:

(require 'dirvish)
(require 'mpvmedia-pos)

(defface mpvmedia-pos-dirvish-face
  '((t (:inherit dired-ignored :underline nil :background unspecified)))
  "Face for commit message overlays."
  :group 'mpvmedia-pos-dirvish)

(dirvish-define-attribute mpvmedia-pos
  "Append git commit message to filename."
  :index 1
  :when (and (not (eq (dirvish-prop :vc-backend) 'Git)) ; don't clash with VC
             (not (dirvish-prop :remote))
             (> win-width 65))
  (let* ((progress-str (mpvmedia-pos-info-string f-name))
         (face (or hl-face 'dirvish-git-commit-message-face))
         (str (concat (substring (concat "  " progress-str) 0 -1) " ")))
    (add-face-text-property 0 (length str) face t str)
    `(left . ,str)))

(defun mpvmedia-pos-dirvish-setup ()
  (push '(mpvmedia-pos-dirvish mpvmedia-pos) dirvish-libraries)
  (push 'mpvmedia-pos dirvish-attributes))

(provide 'mpvmedia-pos-dirvish)
;;; mpvmedia-pos-dirvish.el ends here
