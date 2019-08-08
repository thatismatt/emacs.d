;;; git-buffer-naming

;;; Commentary:
;; Rename the buffers based on the vc-git-root

;;; Code:

(require 'vc-git)

(defvar git-buffer-naming-separator " | ")

(defun git-buffer-naming-generate-buffer-name (filename)
  (when-let* ((project-path      (and filename (vc-git-root filename)))
              (parts             (split-string project-path "/" 'omit-nulls))
              (project-name      (car (last parts)))
              (project-sub-path  (file-relative-name filename project-path))
              (buffer-name-parts (seq-remove #'null (list (file-name-nondirectory filename)
                                                          project-name
                                                          (file-name-directory project-sub-path))))
              (new-buffer-name   (string-join buffer-name-parts git-buffer-naming-separator)))
    new-buffer-name))

(defun git-buffer-naming-rename-buffer (buffer filename)
  "Rename a buffer to include the vc-git-root."
  (when-let* ((new-buffer-name (git-buffer-naming-generate-buffer-name filename)))
    (with-current-buffer buffer (rename-buffer new-buffer-name)))
  buffer)

(defun git-buffer-naming-rename-current-buffer ()
  (interactive)
  (git-buffer-naming-rename-buffer (current-buffer) (buffer-file-name)))

(defun git-buffer-naming-rename-all-buffers ()
  (interactive)
  (dolist (buffer (buffer-list))
    (git-buffer-naming-rename-buffer buffer (buffer-file-name buffer))))

(defun git-buffer-naming-create-file-buffer-advice (cfb-fun filename &rest args)
  "Include the project name (i.e. vc-git-root) in a file's buffername."
  (git-buffer-naming-rename-buffer (apply cfb-fun filename args) filename))

;;;###autoload
(defun git-buffer-naming-load ()
  (interactive)
  (git-buffer-naming-rename-all-buffers)
  (advice-add 'create-file-buffer :around #'git-buffer-naming-create-file-buffer-advice))

(defun git-buffer-naming-unload ()
  (interactive)
  (advice-remove 'create-file-buffer #'git-buffer-naming-create-file-buffer-advice))

(provide 'git-buffer-naming)

;;; git-buffer-naming.el ends here
