(require 'dired+)
(require 'dired-sidebar)
(require 'bind-key)
(require 'dash)
(require 'compat)
(require 'ol)
(eval-when-compile (require 'cl-lib))

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

(defun doc-base--org-link-follow (path _)
  "open file referenced by document in docbase PATH"
  (if-let* ((inode (doc-base--get-inode-of-file (format "%s/%s" doc-base--root path)))
            (file  (doc-base--find-inode-in-books inode)))
      (find-file file)
    (find-file path)))

(defun doc-base--org-link-store-props (docbase-file)
  "get plist of DOCBASE-FILE for #'org-link-store-props"
  (when-let* ((link (concat "docbase:" (file-name-nondirectory docbase-file)))
              (description (replace-regexp-in-string "^[0-9.]*" "" (file-name-nondirectory docbase-file))))
    (list :type "docbase"
          :link link
          :description description)))

(ert-deftest doc-base--org-link-store-props-test ()
  "create plist from docbase file usable for #'org-link-store-props"
  (should (cl-equalp (list :type "docbase"
                           :link "docbase:20230609.prescheme.pdf"
                           :description "prescheme.pdf")
                     (doc-base--org-link-store-props "/home/pe/documents/docbase/20230609.prescheme.pdf"))))

(defun doc-base--org-link-store ()
  "Store a link to a dired file in the doc base."
  (when (memq major-mode '(dired-mode dired-sidebar-mode))
    ;; This is a man page, we do make this link.
    (when-let* ((file (dired-get-file-for-visit))
                (inode (doc-base--get-inode-of-file file))
                (docbase-file (doc-base--find-inode-in-doc-base inode)))
      (apply #'org-link-store-props 
       (doc-base--org-link-store-props docbase-file)))))

(defun doc-base--org-link-export (link description format _)
  (let ((path (format "%s" link))
        (desc (or description link)))
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
      (`latex (format "\\href{%s}{%s}" path desc))
      (`texinfo (format "@uref{%s,%s}" path desc))
      (`ascii (format "%s (%s)" desc path))
      (_ path))))

(org-link-set-parameters
 "docbase"
 :follow #'doc-base--org-link-follow
 :export #'doc-base--org-link-export
 :store  #'doc-base--org-link-store)

(defun doc-base--get-inode-of-file-command (file)
  "create shell command to get inode # of FILE"
  (format "stat -c '%%i' '%s'" file))

(ert-deftest doc-base--get-inode-of-file-command-test ()
  "test shell command generation for idnode retrieval"
  (should (string-equal "stat -c '%i' 'some file. with!.$spaces * andmore.txt'"
                        (doc-base--get-inode-of-file-command "some file. with!.$spaces * andmore.txt"))))

(defun doc-base--get-inode-of-file (file)
  "get inode of a FILE"
  (string-to-number (string-trim (shell-command-to-string (doc-base--get-inode-of-file-command file)))))

(defun doc-base--find-inode-in-doc-base (inode)
  "get doc-base filename for an INODE"
  (doc-base--find-inode-in inode doc-base--root))

(defun doc-base--find-inode-in-books (inode)
  "get doc-base filename for an INODE"
  (doc-base--find-inode-in inode doc-base--books))

(defun doc-base--find-inode-in-command (inode folder)
    (format "find '%s' -inum %i" folder inode))

(ert-deftest doc-base--find-inode-in-command-test ()
  "test shell command generation for locating inodes in folder"
  (should (string-equal "find 'some & file.txt' -inum 4711"
                        (doc-base--find-inode-in-command 4711 "some & file.txt"))))

(defun doc-base--find-inode-in (inode folder)
  "get filename for an INODE in FOLDER"
  (let ((fn (string-trim (shell-command-to-string (doc-base--find-inode-in-command inode folder)))))
    (unless (string-empty-p fn)
      fn)))

(defun doc-base--normalize-name (filename)
  "normalize the given base FILENAME"
  (replace-regexp-in-string (format "[^%s]" doc-base--valid-filename-chars) "_" (downcase filename)))

(ert-deftest doc-base--normalize-name-test () 
  "test file name normalisation"
  (should (string-equal "2__s_sd.ldner.pa_11d"
                        (doc-base--normalize-name "2;'sösd.ldner.pa+11d"))))

(defun doc-base--normal-name-p (filename)
  "is this base FILENAME already normalized?"
  (string-equal filename (doc-base--normalize-name filename)))

(defun doc-base--refresh-views-on-dirs (folders)
  "revert all dired buffers showing the given FOLDERS"
  (-each (--filter it folders)
    (lambda (folder)
      (--each (dired-buffers-for-dir folder )
        (with-current-buffer it (revert-buffer))))))

(defun doc-base--insert-document-into-dired-sidebar--gen-new-name (target-folder file)
    "generate new name for current marked file to put into TARGET-FOLDER"
  (format "%s%s" target-folder
          (file-name-nondirectory file)))

(ert-deftest doc-base--insert-document-into-dired-sidebar--new-filename-test ()
  "test generation of new filename for target folder"
  (should (string-equal (doc-base--insert-document-into-dired-sidebar--gen-new-name "/my/some-folder/" "/some/other/location/test.file.txt")
                        "/my/some-folder/test.file.txt")))

(defun doc-base--insert-document-into-dired-sidebar ()
  "file/move/rename doc initially identified for filing 
 into folder currently under cursor in the sidebar"
  (interactive)
  (setq doc-base--last-target nil)
  (if (and (cl-equalp major-mode 'dired-sidebar-mode)
         (file-exists-p doc-base--file-to-move))
      (when-let* ((target        (or (dired-file-name-at-point) doc-base--books))
                  (_             (file-exists-p target))
                  (target-folder (file-name-directory target))
                  (new-name      (doc-base--insert-document-into-dired-sidebar--gen-new-name
                                  target-folder
                                  doc-base--file-to-move)))
        (unwind-protect
            (progn
              (dired-rename-file doc-base--file-to-move new-name nil)
              (doc-base--refresh-views-on-dirs
               (list (file-name-directory doc-base--file-to-move)
                     (file-name-directory new-name)                     
                     doc-base--root
                     (with-current-buffer doc-base--source-dired-buffer
                       default-directory)
                     doc-base--books))
              (setq doc-base--source-dired-buffer nil)
              (setq doc-base--file-to-move nil)
              (setq doc-base--last-target target-folder)
              (message "filed to '%s'" new-name))
          (when doc-base--file-to-move (message "failed to name %s to %s" doc-base--file-to-move new-name))))
    (message "file '%s' not found for moving (maybe just old binding of 'i', will now be restored)" doc-base--file-to-move))
  (doc-base--cleanup-bindings))

(defun doc-base--bind-insert-key-for-dired-sidebar ()
  "bind 'i' to doc-base--insert-... command, keeping old binding for restoring later"
  (unless (cl-equalp #'doc-base--insert-document-into-dired-sidebar
                     (keymap-lookup dired-sidebar-mode-map "i"))
    (setq doc-base--previous-function-in-dired-sidebar (keymap-lookup dired-sidebar-mode-map "i")))
  (bind-key "i" #'doc-base--insert-document-into-dired-sidebar dired-sidebar-mode-map))

(defun doc-base--provide-dired-sidebar-for-filing ()
  "open sidbar and bind 'i' for inserting previously selected document"
  (doc-base--bind-insert-key-for-dired-sidebar)
  (doc-base--open-sidebar))

(defun doc-base--cleanup-bindings ()
  "cleanup (possibly) dangling insert binding from dired-sidebar"
  (unbind-key "i" dired-sidebar-mode-map)
  (bind-key "i" doc-base--previous-function-in-dired-sidebar dired-sidebar-mode-map))

(defun doc-base--add-document-to-docbase (file &optional yes)
  "add given FILE (document) to the docbase (do renaming if necessary)
 returning t on success, nil on abort"
  (let* ((normalized-name-proposed (doc-base--normalize-name (file-name-nondirectory file)))
         (normalized-name          (if yes
                                       normalized-name-proposed
                                     (read-string "new filename: " normalized-name-proposed)))
         (normalized               (format "%s%s" (file-name-directory file) normalized-name))
         (link-name-proposed       (doc-base--do-add--link-name normalized-name))
         (link-name                (if yes
                                       link-name-proposed
                                     (read-string "filename for docbase: " link-name-proposed)))
         (link                     (format "%s/%s" doc-base--root link-name))
         (rename-p                 (not (string-equal normalized-name (file-name-nondirectory file))))
         (msg                      (if rename-p
                                       (format "rename %s\n-> %s,\nhardlinking %s"
                                               file
                                               normalized-name
                                               link-name)
                                     (format "hardlinking %s" link-name))))
    (if (or yes (yes-or-no-p msg))
        (progn
          (unwind-protect
              (progn
                (when rename-p (dired-rename-file file normalized nil))
                (dired-hardlink normalized link)
                (doc-base--refresh-views-on-dirs
                 (list (file-name-directory normalized)
                       (file-name-directory link)))
                (message (if rename-p "renaming and hardlinking done." "hardlinking done."))
                (setq doc-base--file-to-move normalized)
                t)
            (message (if rename-p "failed to rename and hardlink." "failed to hardlink."))
            nil))
      (message "aborted.")
      nil)))

(defun doc-base--normalized-file (file)
  (let* ((file-name       (file-name-nondirectory file))
         (normalized-file (doc-base--normalize-name file-name)))
    (format "%s%s" (file-name-directory file) normalized-file)))

(defun doc-base--do-normalize (file)
  "normalize the given FILE and execute rename"
  (let* ((normalized (doc-base--normalized-file file)))
    (unless (string-equal file normalized)
      (dired-rename-file file normalized nil))
    normalized))

(defun doc-base--do-add--link-name (normalized-file &optional ctime)
  (let ((dateprefix (format-time-string "%Y%m%d" (or ctime (current-time)))))
    (format "%s.%s" dateprefix (file-name-nondirectory normalized-file))))

(ert-deftest doc-base--do-add--link-name-test ()
  "test the generated link name for the doc base"
  (should (string-equal
           "20230618.some-normalized.file.pdf"
           (doc-base--do-add--link-name "/a/b/c/some-normalized.file.pdf"
                                        '(25743 2096 727303 465000)))))

(defun doc-base--do-add (normalized-file)
  "add th egiven NORMALIZED-FILE to the doc base"
  (dired-hardlink normalized-file
                  (format "%s/%s" doc-base--root (doc-base--do-add--link-name normalized-file))))

(defun doc-base--do-file (target-folder normalized-file)
  "file this NORMALIZED-FILE to the given TARGET-FOLDER"
  (dired-rename-file normalized-file
                     (format "%s%s" doc-base--last-target (file-name-nondirectory normalized-file))
                     nil))

;;;###autoload
(defun doc-base--open-sidebar ()
  "open sidbar into docbase (e.g. to inspect current filing tree)"
  (interactive)
  (let ((default-directory doc-base--books))
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

;;;###autoload
(defun doc-base--file-repeat ()
  "repeat last filing operation on current file or marked files"
  (interactive)
  (if doc-base--last-target
      (progn
        (let ((files-to-process
               (--filter (and it
                            (not (file-directory-p it))
                            (file-exists-p it))
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

;;;###autoload
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
