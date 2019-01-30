(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(grep-find-command
   "find -L . -not -path \"*.svn*\" -not -path \"*.pt.py\" -not -path \"*compressed*\" -type f -print0 | xargs -0 -e grep -n -e ")
 '(magit-diff-use-overlays nil)
 '(make-backup-files nil)
 '(menu-bar-mode t)
 '(mouse-wheel-mode t nil (mwheel))
 '(shell-mode-hook (quote (compilation-shell-minor-mode)))
 '(show-paren-mode t nil (paren))
 '(tempbuf-life-extension-ratio 4)
 '(tempbuf-minimum-timeout 86400)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-mail-address "daniel.nouri@gmail.com")
)
