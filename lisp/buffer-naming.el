;;; buffer-naming.el --- customizable buffer naming

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Rename the buffers based on the project.el project root or the vc-git-root

;;; Usage:

;; ;; turn on - add the advice
;; (buffer-naming-load)
;; ;; choose a buffer naming style:
;; (buffer-naming-set-fn 'git-buffer-naming-fn)
;; (buffer-naming-set-fn 'project-el-buffer-naming-fn)
;; (buffer-naming-set-fn 'generate-new-buffer-name) ;; similar to the default
;; ;; turn off - remove the advice
;; (buffer-naming-unload)

;;; Todo:
;; Handle files from jars
;;  - e.g. `buffer-file-name' = /home/matt/.m2/repository/listora/again/1.0.0/again-1.0.0.jar:again/core.clj

;;; Code:

(require 'vc-git)
(require 'project)

(defvar buffer-naming-separator " | ")

(defun git-buffer-naming-fn (filename)
  (when-let* ((project-path      (vc-git-root filename))
              (parts             (split-string project-path "/" 'omit-nulls))
              (project-name      (car (last parts)))
              (project-sub-path  (file-relative-name filename project-path))
              (buffer-name-parts (seq-remove #'null (list (file-name-nondirectory filename)
                                                          project-name
                                                          (file-name-directory project-sub-path))))
              (new-buffer-name   (string-join buffer-name-parts buffer-naming-separator)))
    new-buffer-name))

(defun buffer-naming-existing-parent-directory (filename)
  (if (or (null filename) ;; recursion safety net
          (file-directory-p filename))
      filename
    (buffer-naming-existing-parent-directory (file-name-directory (directory-file-name filename)))))

(defun project-el-buffer-naming-fn (filename)
  (when-let* ((directory         (buffer-naming-existing-parent-directory filename))
              (project           (project-current nil directory))
              (project-path      (project-root project))
              (parts             (split-string project-path "/" 'omit-nulls))
              (project-name      (car (last parts)))
              (project-sub-path  (file-relative-name filename project-path))
              (buffer-name-parts (seq-remove #'null (list (file-name-nondirectory filename)
                                                          project-name
                                                          (file-name-directory project-sub-path))))
              (new-buffer-name   (string-join buffer-name-parts buffer-naming-separator)))
    new-buffer-name))

(defvar buffer-naming-fn 'identity)

(defun buffer-naming-rename-buffer (buffer filename)
  "Rename a BUFFER visiting FILENAME as per `buffer-naming-fn'."
  (when-let* ((new-buffer-name (when filename (funcall buffer-naming-fn filename))))
    (with-current-buffer buffer (rename-buffer new-buffer-name)))
  buffer)

(defun buffer-naming-rename-current-buffer ()
  "Convenience version of `buffer-naming-rename-buffer' for the current buffer."
  (interactive)
  (buffer-naming-rename-buffer (current-buffer) (buffer-file-name)))

(defun buffer-naming-rename-all-buffers ()
  "Rename all buffers, as per current buffer naming configuration."
  (interactive)
  (dolist (buffer (buffer-list))
    (buffer-naming-rename-buffer buffer (buffer-file-name buffer))))

(defun buffer-naming-create-file-buffer-advice (cfb-fun filename &rest args)
  "Advice for `create-file-buffer' that updates a file's buffer name."
  (buffer-naming-rename-buffer (apply cfb-fun filename args) filename))

;;;###autoload
(defun buffer-naming-load ()
  (interactive)
  (buffer-naming-rename-all-buffers)
  (advice-add 'create-file-buffer :around #'buffer-naming-create-file-buffer-advice))

(defun buffer-naming-unload ()
  (interactive)
  (advice-remove 'create-file-buffer #'buffer-naming-create-file-buffer-advice))

(defun buffer-naming-set-fn (fn)
  (setq buffer-naming-fn fn)
  (buffer-naming-rename-all-buffers))

(provide 'buffer-naming)

;;; buffer-naming.el ends here
