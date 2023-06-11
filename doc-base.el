(require 'dired+)
(require 'dired-sidebar)
(require 'bind-key)
(require 'dash)
(require 'compat)

(defgroup doc-base nil
  "filing documents from dired"
  :group 'dired)

(defcustom doc-base--root
  (expand-file-name "~/documents/docbase")
  "root to place documents into"
  :type 'string
  :group 'doc-base)
(defcustom doc-base--books
  (expand-file-name "~/documents/books")
  "root of dir structure for documents"
  :type 'string
  :group 'doc-base)
(defcustom doc-base--valid-filename-chars
  "a-z0-9-._"
  "regexp character-set definition"
  :type 'string
  :group 'doc-base)

(defvar doc-base--previous-function-in-dired-sidebar #'dired-maybe-insert-subdir "cached binding of 'i' in dired-sidebar")
(defvar doc-base--file-to-move nil "currently selected file for filing")
(defvar doc-base--source-dired-buffer nil "source of filing, needs refreshing")
(defvar doc-base--last-target nil "previous filing target")
(defvar doc-base--last-filing-universial-argument nil "universial argument of last filing operation")

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
  "get inode of a FILE"
  (string-to-number (string-trim (shell-command-to-string (format "stat -c '%%i' %s" file)))))

(defun doc-base--find-inode-in-doc-base (inode)
  "get doc-base filename for an INODE"
  (doc-base--find-inode-in inode doc-base--root))

(defun doc-base--find-inode-in-books (inode)
  "get doc-base filename for an INODE"
  (doc-base--find-inode-in inode doc-base--books))

(defun doc-base--find-inode-in (inode folder)
  "get filename for an INODE in FOLDER"
  (let ((fn (string-trim (shell-command-to-string (format "find %s -inum %i" folder inode)))))
    (unless (string-empty-p fn)
      fn)))

;; (doc-base--find-inode-in-books (doc-base--get-inode-of-file "/home/pe/documents/docbase/20230608.anatomy_of_lisp.pdf"))

(defun doc-base--normalize-name (filename)
  "normalize the given base FILENAME"
  (replace-regexp-in-string (format "[^%s]" doc-base--valid-filename-chars) "_" (downcase filename)))

(defun doc-base--normal-name-p (filename)
  "is this base FILENAME already normalized?"
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

(defun doc-base--refresh-views-on-dirs (folders)
  "revert all dired buffers showing the given FOLDERS"
  (-each (--filter it folders)
    (lambda (folder)
      (--each (dired-buffers-for-dir folder )
        (with-current-buffer it (revert-buffer))))))

(defun doc-base--insert-document-into-dired-sidebar ()
  "file/move/rename doc initially identified for filing 
 into folder currently under cursor in the sidebar"
  (interactive)
  (setq doc-base--last-target nil)
  (if (and (cl-equalp major-mode 'dired-sidebar-mode)
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
                     (file-name-directory new-name)                     
                     doc-base--root
                     doc-base--source-dired-buffer
                     doc-base--books))
              (setq doc-base--source-dired-buffer nil)
              (setq doc-base--file-to-move nil)
              (setq doc-base--last-target target-folder)
              (message "filed to '%s'" new-name))
          (when doc-base--file-to-move (message "failed to name %s to %s" doc-base--file-to-move new-name))))
    (message "file '%s' not found for moving (maybe just old binding of 'i', will now be restored)" doc-base--file-to-move))
  (doc-base--cleanup-bindings))

(defun doc-base--provide-dired-sidebar-for-filing ()
  "open sidbar and bind 'i' for inserting previously selected document"
  (let ((default-directory doc-base--books))
    (when (not (cl-equalp #'doc-base--insert-document-into-dired-sidebar
                     (keymap-lookup dired-sidebar-mode-map "i")))
      (setq doc-base--previous-function-in-dired-sidebar (keymap-lookup dired-sidebar-mode-map "i")))
    (bind-key "i" #'doc-base--insert-document-into-dired-sidebar dired-sidebar-mode-map)
    (if (and (dired-sidebar-buffer)
           (string-equal
            (expand-file-name
             (with-current-buffer (dired-sidebar-buffer)
               dired-directory))
            (format "%s/" doc-base--books)))
        (dired-sidebar-jump-to-sidebar)
      (when (dired-sidebar-buffer)
        (dired-sidebar-hide-sidebar))
      (dired-sidebar-toggle-with-current-directory))))

(defun doc-base--cleanup-bindings ()
  "cleanup (possibly) dangling insert binding from dired-sidebar"
  (unbind-key "i" dired-sidebar-mode-map)
  (bind-key "i" doc-base--previous-function-in-dired-sidebar dired-sidebar-mode-map))

;; (defun doc-base--select-target-dir (folder)
;;   (let ((ivy-sort-functions-alist nil))
;;     (completing-read "file to: " (doc-base--read-directory-tree- doc-base--books) nil t)))

(defun doc-base--add-document-to-docbase (file &optional yes)
  "add given FILE (document) to the docbase (do renaming if necessary)
 returning t on success, nil on abort"
  (let* ((name-base (file-name-base file))
         (name-ext (file-name-extension file))
         (normalized-base-proposed (doc-base--normalize-name name-base))
         (normalized-base (if yes normalized-base-proposed (read-string "new filename: " normalized-base-proposed)))
         (normalized-file (string-join (list normalized-base name-ext) "."))
         (dateprefix (format-time-string "%Y%m%d" (current-time)))
         (normalized (string-join (list (file-name-directory file) normalized-file) ""))
         (link-file-name-proposed (string-join (list dateprefix normalized-base name-ext) "."))
         (link-file-name (if yes link-file-name-proposed (read-string "filename for docbase: " link-file-name-proposed)))
         (link-name (string-join (list doc-base--root link-file-name) "/"))
         (rename-p (not (doc-base--normal-name-p name-base)))
         (msg (if rename-p
                  (format "rename %s\n-> %s,\nhardlinking %s" file normalized-file link-file-name)
                (format "hardlinking %s" link-file-name))))
    (if (or yes (yes-or-no-p msg))
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

(defun doc-base--file-repeat ()
  "repeat last filing operation on current file or marked files"
  (interactive)
  (if doc-base--last-target
      (progn
        (let ((files-to-process (--filter (and it (not (file-directory-p it)) (file-exists-p it))
                                          (dired-get-marked-files))))
          (-each files-to-process
            (lambda (file) (let ((normalized-file (doc-base--do-normalize file)))
                          (doc-base--do-add normalized-file)
                          (doc-base--do-file doc-base--last-target normalized-file))))
          (doc-base--refresh-views-on-dirs
           (list
            (expand-file-name default-directory)
            doc-base--books
            doc-base--root
            doc-base--last-target))))
    (message "no target known (did you file a document?)")))

(defun doc-base--do-normalize (file)
  "normalize the given FILE and execute rename"
  (let* ((name-base (file-name-base file))
         (name-ext (file-name-extension file))
         (normalized-base (doc-base--normalize-name name-base))
         (normalized-file (string-join (list normalized-base name-ext) "."))
         (normalized (string-join (list (file-name-directory file) normalized-file) ""))
         (rename-p (not (doc-base--normal-name-p name-base))))
    (when rename-p (dired-rename-file file normalized nil))
    normalized))

(defun doc-base--do-add (normalized-file)
  "add then given NORMALIZED-FILE to the doc base"
  (let* ((dateprefix (format-time-string "%Y%m%d" (current-time)))
         (normalized-base (file-name-base normalized-file))
         (name-ext (file-name-extension normalized-file))
         (link-file-name (string-join (list dateprefix normalized-base name-ext) "."))
         (link-name (string-join (list doc-base--root link-file-name) "/")))
    (dired-hardlink normalized-file link-name)))

(defun doc-base--do-file (target-folder normalized-file)
  "file this NORMALIZED-FILE to the given TARGET-FOLDER"
  (let* ((normalized-base (file-name-base normalized-file))
         (name-ext (file-name-extension normalized-file))
         (target-file (string-join (list doc-base--last-target normalized-base "." name-ext) "")))
    (dired-rename-file normalized-file target-file nil)))

(defun doc-base--file ()
  "interactive file a document under cursor in dired mode
 (cannot be run on files already in the docbase, this is checked)"
  (interactive)
  (doc-base--cleanup-bindings)
  (setq doc-base--last-filing-universial-argument nil)
  (setq doc-base--file-to-move nil)
  (setq doc-base--source-dired-buffer nil)
  (if-let* ((_    (eq  major-mode 'dired-mode))
            (file (or (dired-get-file-for-visit) (make-temp-name "non-ex")))
            (_    (file-exists-p file))
            (_    (not (doc-base--find-inode-in-doc-base (doc-base--get-inode-of-file file))))
            (_    (not (string-prefix-p doc-base--root (file-name-directory file))))
            (_    (doc-base--add-document-to-docbase file (not (equal current-prefix-arg nil)))))
      (progn
        (setq doc-base--last-filing-universial-argument current-prefix-arg)
        (setq doc-base--source-dired-buffer (current-buffer))
        (message "use 'i' to file in sidebar")
        (doc-base--provide-dired-sidebar-for-filing))
    (message "aborted filing, preconditions not met")))

(provide 'doc-base)
