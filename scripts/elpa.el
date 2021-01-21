(setq package-user-dir
      (expand-file-name (format "%s/../.elpa/%s/elpa"
                                (file-name-directory load-file-name)
                                emacs-version)))
(package-initialize)
(add-to-list 'load-path default-directory)
