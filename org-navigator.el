;;; org-navigator 

(setq org-navigator-fold-first t)
(setq org-navigator-set-view t)
(setq org-navigator-key-map evil-normal-state-map)
(setq org-navigator-all-shortcuts '()) 
(setq org-navigator-duplicates '()) 
(setq org-navigator-config-file "~/dotfiles/emacs/org-navigator/shortcut-definitions.el") 

(setq org-navigator-file-to-prefix "<leader>J ")
(setq org-navigator-go-to-prefix "<leader>j ")
(setq org-navigator-open-and-narrow-prefix nil)
(setq org-navigator-open-in-indirect-prefix "<leader>c ")
(setq org-navigator-clock-in-prefix "<leader>l ")

(defun org-navigator-set-view ()
  "Set the view and visibility after jumping to a heading."
  (interactive)
  (if org-navigator-set-view
      (progn
        (recenter 0)
        (if (buffer-base-buffer) 
            (org-cycle) 
          (org-cycle))
        (org-show-children))))

(defun org-navigator-composable-refile (FILE HEADLINE ACTION)
  "Jump, file or clock in to HEADLINE.
  How it behaves will depend on the value of ACTION.
  If ACTION is 'file-to, file the current heading to HEADLINE.
  If ACTION is 'go-to, go to HEADLINE.
  If ACTION is 'go-to-narrow, go to HEADLINE and narrow buffer to that headline.
  If ACTION is 'go-to-indirect, go to HEADLINE and open in a narrowed indirect buffer.
  If ACTION is 'clock-in, clock in to HEADLINE and return to point."
  (interactive)
  (let
      ((pos (save-excursion
	      (find-file FILE)
	      (org-find-exact-headline-in-buffer headline))))
    (cond  
     ((eq ACTION 'file-to) ;;File to
      (progn
        (let ((refile-arg nil)) 
          (org-refile refile-arg nil (list HEADline FILE nil pos) nil)
          ;;If filing from a capture buffer, this line will finalize the capture buffer
          (if (string= "CAPTURE" (substring (buffer-name (current-buffer)) 0 7)) (org-capture-finalize)))))
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
      (let ((refile-arg '(4))) 
        (save-excursion 
          (progn (org-refile refile-arg nil (list HEADLINE FILE nil pos) nil)
                 (org-clock-in))))))))

(defun org-navigator-composable-set-key (KEYPRESS TARGET FILENAME FILETO GOTO NARROW INDIRECT CLOCKIN)
  "Programmatically set shortcuts with org-navigator-composable-refile."
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
  (let ((jump-key (cdr (assoc "JUMP-KEY" (org-entry-properties)))))
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
  "Prompt the user to set JUMP-KEY for the current subheading."
  (interactive)
  (org-set-property "JUMP-KEY" (read-string "Enter the key as a string:")))

(defun org-navigator-set-all ()
  "Go through all org-agenda-files and set jump shortcuts for any headings where JUMP-KEY property is set. "
  (interactive)
  (write-region "" nil org-navigator-config-file)
  (org-map-entries 'org-navigator-set-from-property nil org-agenda-files)
  (load-file org-navigator-config-file))

(provide 'org-navigator)

;;; org-navigator.el ends here

