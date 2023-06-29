;;; org-navigator 

(require 'org-refile)

(defun org-navigator-set-view ()
  "If org-navigator-set-view is non-nil, move point to the top of the screen and show the current subheading's children."
  (interactive)
  (if org-navigator-set-view
      (progn
        (recenter 0)
        (if (buffer-base-buffer) 
            (org-cycle) 
          (org-cycle))
        (org-show-children))))

(defun org-navigator-composable-refile (FILE HEADLINE ACTION)
  "Jump, file or clock in to HEADLINE, located in FILE.
  How it behaves will depend on the value of ACTION.
  If ACTION is 'file-to, file the current heading to HEADLINE.
  If ACTION is 'go-to, go to HEADLINE.
  If ACTION is 'go-to-narrow, go to HEADLINE and narrow buffer to that headline.
  If ACTION is 'go-to-indirect, go to HEADLINE and open in a narrowed indirect buffer.
  If ACTION is 'clock-in, clock in to HEADLINE and return to point.
  If org-navigator-fold-first is non-nil, fold the file first (so only one tree is unfolded at a time).
  If org-navigator-set-view is non-nil, move point to the top of the screen and show the current subheading's children."
  (interactive)
  (let
      ((pos (save-excursion
	      (find-file FILE)
	      (org-find-exact-headline-in-buffer HEADLINE))))
    (cond  
     ((eq ACTION 'file-to) ;;File to
      (if (symbolp #'intuitive-tab-line-store-tabs)
	  (intuitive-tab-line-store-tabs))
      (progn
        (let ((refile-arg nil)) 
          (org-refile refile-arg nil (list HEADLINE FILE nil pos) nil)
          ;;If filing from a capture buffer, this line will finalize the capture buffer
          (if (string= "CAPTURE" (substring (buffer-name (current-buffer)) 0 7)) (org-capture-finalize))))
      (if (symbolp #'intuitive-tab-line-restore-tabs)
      (intuitive-tab-line-restore-tabs)))
     ((eq ACTION 'go-to) ;;Go to
      (progn 
        (if org-navigator-fold-first (progn (find-file FILE)
					    (org-overview)))
        (let ((refile-arg '(4))) 
          (org-refile refile-arg nil (list HEADLINE FILE nil pos) nil))
        (org-navigator-set-view)))
     ((eq ACTION 'go-to-narrow) ;;Go to and narrow
      (progn 
        (let ((refile-arg '(4))) 
          (org-refile refile-arg nil (list HEADLINE FILE nil pos) nil))
        (org-narrow-to-subtree)
        (org-navigator-set-view)))
     ((eq ACTION 'go-to-indirect) ;;Go to in indirect buffer
      (progn 
        (let ((refile-arg '(4))) 
          (org-refile refile-arg nil (list HEADLINE FILE nil pos) nil))
        (org-tree-to-indirect-buffer '(4))
        (org-navigator-set-view)))
     ((eq ACTION 'clock-in) ;;Clock in, stay at current point
      (if (symbolp #'intuitive-tab-line-store-tabs)
	  (intuitive-tab-line-store-tabs))
      (let ((refile-arg '(4))) 
        (save-excursion 
          (progn (org-refile refile-arg nil (list HEADLINE FILE nil pos) nil)
                 (org-clock-in))))
      (if (symbolp #'intuitive-tab-line-restore-tabs)
	  (intuitive-tab-line-restore-tabs))))))

(defun org-navigator-composable-set-key (KEYPRESS TARGET FILENAME FILETO GOTO NARROW INDIRECT CLOCKIN)
  "Define keyboard shortcuts for org-navigator-composable-refile to TARGET in FILENAME.
  The shortcut keys will be made up of their respective prefix, with KEYPRESS appended.
  For example: the shortcut for 'file-to will be set to \"org-navigator-file-to-prefix + KEYPRESS\".
  If a particular prefix key is nil, no shortcuts will be set for that action.
  Example: the shortcuts org-navigator-composable-refile function filing to TARGET
  If FILETO, GOTO, NARROW, INDIRECT or CLOCKIN are nil, no keyboard shortcut will be set to that function."
  (let ((filekeypress (vconcat (kbd (concat org-navigator-file-to-prefix KEYPRESS))))
        (filecomment (concat "File To " TARGET))
        (jumpkeypress (vconcat (kbd (concat org-navigator-go-to-prefix KEYPRESS))))
        (jumpcomment (concat "Jump To " TARGET))
        (narrowkeypress (vconcat (kbd (concat org-navigator-open-and-narrow-prefix KEYPRESS))))
        (narrowcomment (concat "Jump To " TARGET " and narrow"))
        (indirectkeypress (vconcat (kbd (concat org-navigator-open-in-indirect-prefix KEYPRESS))))
        (indirectcomment (concat "Jump To " TARGET " in an indirect buffer"))
        (clockinkeypress (vconcat (kbd (concat org-navigator-clock-in-prefix KEYPRESS))))
        (clockcomment (concat "Clock In To " TARGET)))
    (if (and org-navigator-file-to-prefix FILETO)
        (define-key org-navigator-key-map filekeypress `(,filecomment . (lambda () (interactive) (org-navigator-composable-refile ,FILENAME ,TARGET 'file-to)))))
    (if (and org-navigator-go-to-prefix GOTO)
        (define-key org-navigator-key-map jumpkeypress `(,jumpcomment . (lambda () (interactive) (org-navigator-composable-refile ,FILENAME ,TARGET 'go-to)))))
    (if (and org-navigator-open-and-narrow-prefix NARROW)
        (define-key org-navigator-key-map narrowkeypress `(,narrowcomment . (lambda () (interactive) (org-navigator-composable-refile ,FILENAME ,TARGET 'go-to-narrow)))))
    (if (and org-navigator-open-in-indirect-prefix INDIRECT)
        (define-key org-navigator-key-map indirectkeypress `(,indirectcomment . (lambda () (interactive) (org-navigator-composable-refile ,FILENAME ,TARGET 'go-to-indirect)))))
    (if (and org-navigator-clock-in-prefix CLOCKIN)
        (define-key org-navigator-key-map clockinkeypress `(,clockcomment . (lambda () (interactive) (org-navigator-composable-refile ,FILENAME ,TARGET 'clock-in)))))))

(defun org-navigator-set-from-property ()
  "Test if ORG-NAV-KEY property is set at point. If it is, add elisp code to org-navigator-config-file to define shortcuts to jump to this point, with org-navigator-composable-set-key."
  (let ((jump-key (cdr (assoc "ORG-NAV-KEY" (org-entry-properties)))))
    (if jump-key
        (progn
          (if (member jump-key org-navigator-all-shortcuts)
              (add-to-list 'org-navigator-duplicates jump-key))
          (add-to-list 'org-navigator-all-shortcuts jump-key)
          (let ((filename (buffer-file-name (current-buffer)))
                (heading (org-get-heading)))
            (append-to-file (concat "(org-navigator-composable-set-key \""
				    jump-key "\" \""
				    heading "\" \""
				    filename "\" t t t t t)\n")
			    nil org-navigator-config-file))))))

(defun org-navigator-set-key ()
  "Prompt the user to set ORG-NAV-KEY for the current subheading."
  (interactive)
  (org-set-property "ORG-NAV-KEY" (read-string "Enter the key as a string:")))

(defun org-navigator-set-all ()
  "First, this function will delete the contents of org-navigator-config-file (to clear any previous org-navigator shortcuts.
  Next, go through all org-agenda-files and run org-navigator-set-from-property for each subheading. This will write functions to define shortcut keys for any subheading where ORG-NAV-KEY is set in org-navigator-config-file.
  Finally, evaluate org-navigator-config-file, to load all the shortcuts.
  Note: if your agenda files are long, this could take some time. It is recommended to run this manually as needed, rather than as part of your config."
  (interactive)
  (write-region
   "
;;DO NOT EDIT THIS FILE MANUALLY.
;;This file will be automatically over-written whenever 'org-navigator-set-all' is called. Any custom edits to this file will be lost permanently.
"
   nil
   org-navigator-config-file)
  (org-map-entries 'org-navigator-set-from-property nil org-agenda-files)
  (load-file org-navigator-config-file))

(defun org-navigator-load-auto-config ()
  (interactive)
  (if (file-exists-p org-navigator-config-file)
      (load-file org-navigator-config-file)))

(provide 'org-navigator)

;;; org-navigator.el ends here

