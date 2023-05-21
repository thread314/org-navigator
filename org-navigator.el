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
  (interactive)
  (if org-navigator-set-view
      (progn
        (recenter 0)
        (if (buffer-base-buffer) 
            (org-cycle) 
          (org-cycle))
        (org-show-children))))

(defun org-navigator-composable-refile (file headline action)
  (interactive)
  (let
      ((pos (save-excursion
              (find-file file)
              (org-find-exact-headline-in-buffer headline))))
    (cond  
     ((eq action 'file-to) ;;File to
      (progn
        (let ((refile-arg nil)) 
          (org-refile refile-arg nil (list headline file nil pos) nil)
          ;;If filing from a capture buffer, this line will finalize the capture buffer
          (if (string= "CAPTURE" (substring (buffer-name (current-buffer)) 0 7)) (org-capture-finalize)))))
     ((eq action 'go-to) ;;Go to
      (progn 
        (if org-navigator-fold-first (progn (find-file file)
                                            (org-overview)))
        (let ((refile-arg '(4))) 
          (org-refile refile-arg nil (list headline file nil pos) nil))
        (org-navigator-set-view)))
     ((eq action 'go-to-narrow) ;;Go to and narrow
      (progn 
        (let ((refile-arg '(4))) 
          (org-refile refile-arg nil (list headline file nil pos) nil))
        (org-narrow-to-subtree)
        (org-navigator-set-view)))
     ((eq action 'go-to-indirect) ;;Go to in indirect buffer
      (progn 
        (let ((refile-arg '(4))) 
          (org-refile refile-arg nil (list headline file nil pos) nil))
        (org-tree-to-indirect-buffer '(4))
        (org-navigator-set-view)))
     ((eq action 'clock-in) ;;Clock in, stay at current point
      (let ((refile-arg '(4))) 
        (save-excursion 
          (progn (org-refile refile-arg nil (list headline file nil pos) nil)
                 (org-clock-in))))))))

(defun org-navigator-composable-set-key (keypress target filename fileto goto narrow indirect clockin)
  (interactive)
  (let ((filekeypress (vconcat (kbd (concat org-navigator-file-to-prefix keypress))))
        (filecomment (concat "File To " target))
        (jumpkeypress (vconcat (kbd (concat org-navigator-go-to-prefix keypress))))
        (jumpcomment (concat "Jump To " target))
        (narrowkeypress (vconcat (kbd (concat org-navigator-open-and-narrow-prefix keypress))))
        (narrowcomment (concat "Jump To " target " and narrow"))
        (indirectkeypress (vconcat (kbd (concat org-navigator-open-in-indirect-prefix keypress))))
        (indirectcomment (concat "Jump To " target " in an indirect buffer"))
        (clockinkeypress (vconcat (kbd (concat org-navigator-clock-in-prefix keypress))))
        (clockcomment (concat "Clock In To " target)))
    (if (and org-navigator-file-to-prefix fileto)
        (define-key org-navigator-key-map filekeypress `(,filecomment . (lambda () (interactive) (org-navigator-composable-refile ,filename ,target 'file-to)))))
    (if (and org-navigator-go-to-prefix goto)
        (define-key org-navigator-key-map jumpkeypress `(,jumpcomment . (lambda () (interactive) (org-navigator-composable-refile ,filename ,target 'go-to)))))
    (if (and org-navigator-open-and-narrow-prefix narrow)
        (define-key org-navigator-key-map narrowkeypress `(,narrowcomment . (lambda () (interactive) (org-navigator-composable-refile ,filename ,target 'go-to-narrow)))))
    (if (and org-navigator-open-in-indirect-prefix indirect)
        (define-key org-navigator-key-map indirectkeypress `(,indirectcomment . (lambda () (interactive) (org-navigator-composable-refile ,filename ,target 'go-to-indirect)))))
    (if (and org-navigator-clock-in-prefix clockin)
        (define-key org-navigator-key-map clockinkeypress `(,clockcomment . (lambda () (interactive) (org-navigator-composable-refile ,filename ,target 'clock-in)))))))

(defun org-navigator-set-from-property ()
  (interactive)
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
  (interactive)
  (org-set-property "JUMP-KEY" (read-string "Enter the key as a string:")))

(defun org-navigator-set-all ()
  (interactive)
  (write-region "" nil org-navigator-config-file)
  (org-map-entries 'org-navigator-set-from-property nil org-agenda-files)
  (load-file org-navigator-config-file))

(provide 'org-navigator)

;;; org-navigator.el ends here

