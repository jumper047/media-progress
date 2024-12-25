;;; media-progress-cache.el --- Persistent cache with restricted storage size
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

;; Module provides persistent cache with option to restrict storage size -
;; when storage exceeds its limits, exceeding elements will be dropped,
;; starting from those who was added first

;;; Code:

;;; -*- lexical-binding:t -*-

(defcustom media-progress-cache-file (locate-user-emacs-file "media-progress-cache")
  "File to store persistent cache data."
  :group 'media-progress
  :type 'file)

(defcustom media-progress-cache-limit 1000
  "Limit for max. number of cache elements stored.
In case of storage overflow, all exceeding elements will be dropped
from storage on next load, starting from ones who were added early."
  :group 'media-progress
  :type 'integer)

(defvar media-progress-cache-data nil
  "Variable contains pists, one for every storage name.
Those plists contains hasmap with cached data.")
(defvar media-progress-cache-history nil
  "Variable contains plists")
(defvar media-progress-cache-initialized nil)
(defvar media-progress-cache-version 1)
(defvar media-progress-cache--buffer-name " *Media Progress Cache*")

(defun media-progress-cache-storage-initialize (name)
  "Initialize new cache substorage for NAME."
  (let ((cache-table (make-hash-table :size media-progress-cache-limit :test 'equal)))
    (push (cons name cache-table) media-progress-cache-data)
    cache-table))

(defun media-progress-cache--data-to-write ()
  "Prepare data to write in persistent cache storage."
  (list
   (cons 'data media-progress-cache-data)
   (cons 'history media-progress-cache-history)
   (cons 'version media-progress-cache-version)))

(defun media-progress-cache-save ()
  "Save cache to persistent storage."
  (let ((file (expand-file-name media-progress-cache-file))
        (coding-system-for-write 'utf-8))
    (with-current-buffer (get-buffer-create media-progress-cache--buffer-name)
      (delete-region (point-min) (point-max))
      (insert (format ";;; -*- coding: %s; mode: lisp-data -*-\n"
                      coding-system-for-write))
      (let ((print-length nil)
            (print-level nil))
        (pp (media-progress-cache--data-to-write) (current-buffer)))
      (condition-case nil
          (write-region (point-min) (point-max) file)
        (file-error (message "Media Progress Cache: can't write %s" file)))
      (kill-buffer (current-buffer)))))

(defun media-progress-cache-load ()
  "Load cache from persistent storage."
  (let ((file (expand-file-name media-progress-cache-file)))
    (if (file-readable-p file)
        (with-current-buffer (get-buffer-create media-progress-cache--buffer-name)
          (delete-region (point-min) (point-max))
          (let (coding-system-for-read)
            (insert-file-contents file))
          (goto-char (point-min))
          (let ((storage
                 (with-demoted-errors "Error reading media-progress-cache- data: %S"
                   (car (read-from-string
                         (buffer-substring (point-min) (point-max)))))))
            (if (eq (alist-get 'version storage) media-progress-cache-version)
                (setq media-progress-cache-data (alist-get 'data storage)
                      media-progress-cache-history (alist-get 'history storage))))
          (kill-buffer (current-buffer))))
    nil))

(defun media-progress-cache-drop-old (n-elements)
  "Drop N-ELEMENTS, starting from oldest ones, from cache."
  (let* ((n-to-delete (- (length media-progress-cache-history) n-elements))
         (elts-to-delete (last media-progress-cache-history n-to-delete)))
    (dolist (elt-to-delete elts-to-delete)
      (let ((storage (alist-get (car elt-to-delete) media-progress-cache-data))
            (key (cdr elt-to-delete)))
        (remhash key storage)
        (delete elt-to-delete media-progress-cache-history)))))

(defun media-progress-cache-kill-emacs-hook ()
  "Hook to save cache on quit."
  (if media-progress-cache-initialized
      (media-progress-cache-save)))

(defun media-progress-cache-init ()
  "Initialize cache storage."
  (media-progress-cache-load)
  (media-progress-cache-drop-old media-progress-cache-limit)
  (add-hook 'kill-emacs-hook #'media-progress-cache-kill-emacs-hook)
  (setq media-progress-cache-initialized 't))

(defun media-progress-cache--get-storage (name)
  "Get hashmap for storage NAME, or create one if needed."
  (let ((storage (assoc name media-progress-cache-data)))
    (if storage
        (cdr storage)
      (media-progress-cache-storage-initialize name))))

(defun media-progress-cache-get (storage key)
  "Get KEY from cache storage named STORAGE."
  (when (not media-progress-cache-initialized)
    (media-progress-cache-init))
  (let ((storage-dict (media-progress-cache--get-storage storage)))
    (gethash key storage-dict nil)))

(defun media-progress-cache-put (storage key value)
  "Put VALUE into STORAGE by KEY."
  (when (not media-progress-cache-initialized)
    (media-progress-cache-init))
  (let ((storage-dict (media-progress-cache--get-storage storage)))
    (puthash key value storage-dict)
    (push (cons storage key) media-progress-cache-history)))

(defun media-progress-cache-invalidate (_)
  "Invalidate cache for media-progress."
  (interactive "P")
  (setq media-progress-cache-history nil
        media-progress-cache-data nil)
  (delete-file (expand-file-name media-progress-cache-file)))

(defmacro media-progress-cache-wrap-function (symbol)
  "Add caching to SYMBOL.
SYMBOL should be the function which receives one string argument."
  `(advice-add ,symbol :around (lambda (orig-fn arg)
                                 (let ((value (media-progress-cache-get ,symbol arg)))
                                   (if value
                                       value
                                     (setq value (apply orig-fn (list arg)))
                                     (media-progress-cache-put ,symbol arg value)
                                     value)))))

(provide 'media-progress-cache)
;;; media-progress-cache.el ends here
