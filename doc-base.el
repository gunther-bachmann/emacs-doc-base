(require 'dired+)

(defcustom doc-base--root (expand-file-name "~/documents/docbase") "root to place documents into")
(defcustom doc-base--books (expand-file-name "~/documents/books") "root of dir structure for documents")
(defcustom doc-base--valid-filename-chars "a-z0-9-._" "regexp character-set definition")

;; flow: inbox (function: doc-base--file)
;; - identify document in dired 
;; - execute doc-base-file command on this document
;; - normalize filename (in place)
;; - create hardlink in doc-base
;; - move/rename document into books dir tree structure through separate dired-sidebar

;; flow: refile in doc tree (function:)
;; - identify document in dired
;; - move/rename document into book dir tree structure through separate dired-sidebar

;; flow: persistent org reference to file (function:)
;; - get i-node of file
;; - get doc-base name for i-node
;; - provide reference to doc-base name

(defun doc-base--get-inode-of-file (file)
  "get inode of a file"
  (string-to-number (string-trim (shell-command-to-string (format "stat -c '%%i' %s" file)))))

(defun doc-base--find-inode-in-doc-base (inode)
  "get doc-base filename for an inode"
  (doc-base--find-inode-in inode doc-base--root))

(defun doc-base--find-inode-in-books (inode)
  "get doc-base filename for an inode"
  (doc-base--find-inode-in inode doc-base--books))

(defun doc-base--find-inode-in (inode folder)
  "get filename for an inode in folder"
  (let ((fn (string-trim (shell-command-to-string (format "find %s -inum %i" folder inode)))))
    (unless (string-empty-p fn)
      fn)))


;; (doc-base--find-inode-in-books (doc-base--get-inode-of-file "/home/pe/documents/docbase/20230608.anatomy_of_lisp.pdf"))

(defun doc-base--normalize-name (filename)
  "normalize the given base file name"
  (replace-regexp-in-string (format "[^%s]" doc-base--valid-filename-chars) "_" (downcase filename)))

(defun doc-base--normal-name-p (filename)
  "is this base file name already normalized?"
  (string-equal filename (doc-base--normalize-name filename)))

;; tree = (list "name" te1 te2 ...)
;; tree-elemnent = tree

;; (defun doc-base--read-directory-tree- (folder)
;;   (cons "<here>"
;;         (sort
;;          (--map (s-chop-prefix "/" it)
;;                 (--filter (not (seq-empty-p it))
;;                           (--map (s-chop-prefix folder it)
;;                                  (string-split (shell-command-to-string (format "find %s -maxdepth 1 -type d " folder))))))
;;          'string<)))

(defvar doc-base--previous-function-in-dired-sidebar #'dired-maybe-insert-subdir "cached binding of 'i' in dired-sidebar")
(defvar doc-base--file-to-move nil "currently selected file for filing")

(defun doc-base--refresh-views-on-dirs (folders)
  "revert all dired buffers showing the given folders"
  (-each folders
    (lambda (folder)
      (--each (dired-buffers-for-dir folder )
        (with-current-buffer it (revert-buffer))))))

(defun doc-base--insert-document-into-dired-sidebar ()
  "file/move/rename doc initially identified for filing 
into folder currently under cursor in the sidebar"
  (interactive)
  (if (and (equalp major-mode 'dired-sidebar-mode)
         (file-exists-p doc-base--file-to-move))
      (when-let* ((target (or (dired-file-name-at-point) doc-base--books))
                  (_ (file-exists-p target))
                  (target-folder (file-name-directory target))
                  (new-name (format "%s%s.%s" target-folder
                                    (file-name-base doc-base--file-to-move)
                                    (file-name-extension doc-base--file-to-move))))
        (unwind-protect
            (progn
              (dired-rename-file doc-base--file-to-move new-name nil)
              (doc-base--refresh-views-on-dirs
               (list (file-name-directory doc-base--file-to-move)
                     (file-name-directory new-name)))
              (setq doc-base--file-to-move nil)
              (message "filed to '%s'" new-name))
          (when doc-base--file-to-move (message "failed to name %s to %s" doc-base--file-to-move new-name))))
    (message "file '%s' not found for moving (maybe just old binding of 'i', will now be restored)"))
  (doc-base--cleanup-bindings))

(defun doc-base--provide-dired-sidebar-for-filing ()
  "open sidbar and bind 'i' for inserting previously selected document"
  (let ((default-directory doc-base--books))
    (when (dired-sidebar-buffer)
      (dired-sidebar-hide-sidebar))
    (when (not (equalp #'doc-base--insert-document-into-dired-sidebar
                     (keymap-lookup dired-sidebar-mode-map "i")))
      (setq doc-base--previous-function-in-dired-sidebar (keymap-lookup dired-sidebar-mode-map "i")))      
    (bind-key "i" #'doc-base--insert-document-into-dired-sidebar dired-sidebar-mode-map)
    (dired-sidebar-toggle-with-current-directory)))

(defun doc-base--cleanup-bindings ()
  "cleanup (possibly) dangling insert binding from dired-sidebar"
  (unbind-key "i" dired-sidebar-mode-map)
  (bind-key "i" doc-base--previous-function-in-dired-sidebar dired-sidebar-mode-map))

;; (defun doc-base--select-target-dir (folder)
;;   (let ((ivy-sort-functions-alist nil))
;;     (completing-read "file to: " (doc-base--read-directory-tree- doc-base--books) nil t)))

(defun doc-base--add-document-to-docbase (file)
  "add given document to the docbase (do renaming if necessary)
returning t on success, nil on abort"
  (let* ((name-base (file-name-base file))
         (name-ext (file-name-extension file))
         (normalized-base-proposed (doc-base--normalize-name name-base))
         (normalized-base (read-string "new filename: " normalized-base-proposed))
         (normalized-file (string-join (list normalized-base name-ext) "."))
         (dateprefix (format-time-string "%Y%m%d" (current-time)))
         (normalized (string-join (list (file-name-directory file) normalized-file) ""))
         (link-file-name-proposed (string-join (list dateprefix normalized-base name-ext) "."))
         (link-file-name (read-string "filename for docbase: " link-file-name-proposed))
         (link-name (string-join (list doc-base--root link-file-name) "/"))
         (rename-p (not (doc-base--normal-name-p name-base)))
         (msg (if rename-p
                  (format "rename %s\n-> %s,\nhardlinking %s" file normalized-file link-file-name)
                (format "hardlinking %s" link-file-name))))
    (if (yes-or-no-p msg)
        (progn
          (unwind-protect
              (progn
                (when rename-p (dired-rename-file file normalized nil))
                (dired-hardlink normalized link-name)
                (doc-base--refresh-views-on-dirs
                 (list (file-name-directory normalized)
                       (file-name-directory link-name)))
                (message (if rename-p "renaming and hardlinking done." "hardlinking done."))
                (setq doc-base--file-to-move normalized)
                t)
            (message (if rename-p "failed to rename and hardlink." "failed to hardlink."))
            nil))
      (message "aborted.")
      nil)))

(defun doc-base--file ()
  "interactive file a document under cursor in dired mode
(cannot be run on files already in the docbase, this is checked)"
  (interactive)
  (doc-base--cleanup-bindings)
  (setq doc-base--file-to-move nil)
  (if-let* ((_    (eq  major-mode 'dired-mode))
            (file (or (dired-get-file-for-visit) (make-temp-name "non-ex")))
            (_    (file-exists-p file))
            (_    (not (doc-base--find-inode-in-doc-base (doc-base--get-inode-of-file file))))
            (_    (not (string-prefix-p doc-base--root (file-name-directory file))))
            (_    (doc-base--add-document-to-docbase file)))
      (progn
        (message "use 'i' to file in sidebar")
        (doc-base--provide-dired-sidebar-for-filing))
      (gb/alert-message-notify-dunst '(:message "aborted filing, preconditions not met" :title "doc-base"))))

(provide 'doc-base)

