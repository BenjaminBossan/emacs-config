;; custom.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Libraries in ~/emacs.d/lib should take precedence over other
;; libraries with same name
;; http://www.emacswiki.org/emacs/LoadPath
(let ((default-directory "~/.emacs.d/lib/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; package
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; ben's additions start
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'whitespace-mode)

; don't show the startup screen
(setq inhibit-startup-screen 1)

; don't show the menu bar
(menu-bar-mode 1)

; don't show the tool bar
(require 'tool-bar)
(tool-bar-mode 0)

; don't show the scroll bar
(scroll-bar-mode 0)

; deactivate blinking cursor
(blink-cursor-mode 0)

;; Switch to previous buffer
(global-set-key (kbd "C-q") 'mode-line-other-buffer)

;; backwards kill word
(global-set-key (kbd "C-l") 'backward-kill-word)

;; move forward one paragrahp
(global-set-key (kbd "C-o") 'forward-paragraph)
;; move forward one paragrahp # 2
(global-set-key (kbd "C-.") 'forward-paragraph)
;; move backward one paragrahp
(global-set-key (kbd "C-,") 'backward-paragraph)

;; indent text block left and right
(global-set-key (kbd "M-<left>") 'python-indent-shift-left)
(global-set-key (kbd "M-<right>") 'python-indent-shift-right)

;; python docstring style line breaks
(global-set-key (kbd "M-Q") 'python-fill-paragraph-function)

;; list of search results in file
(global-set-key (kbd "<f9>") 'list-matching-lines)

;; macro for running pytest
(fset 'runpytest
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 111 24 98 115 104 101 return 18 112 121 46 116 101 115 116 32 return return] 0 "%d")) arg)))
(global-set-key (kbd "C-x p") 'runpytest)

;; ben's additions end

;; Josef's keybindings
;; add pdb breakpoint
(defun python-add-breakpoint ()
  (interactive)
  (newline-and-indent)
  (insert "import pdb; pdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import pdb; pdb.set_trace()"))
(global-set-key "\M-p" 'python-add-breakpoint)

;; Daniel's keybindings

(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'confirm-exit-emacs)

;; Magit https://magit.vc/manual/magit/Getting-started.html#Getting-started
(global-set-key (kbd "C-x g") 'magit-status)

;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.php
(require 'org-install)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; Misc customization
(tool-bar-mode -1)
(setq frame-title-format "%b - Emacs")
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(column-number-mode 1)
(setq truncate-partial-width-windows nil)
(defun confirm-exit-emacs ()
  "ask for confirmation before exiting emacs"
  (interactive)
  (if (yes-or-no-p "Are you sure you want to exit? ")
      (save-buffers-kill-emacs)))
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings 'super) ;; easier window switching

;; http://unix.stackexchange.com/questions/19494/how-to-colorize-text-in-emacs
(defun ansi-color-apply-on-region-int (beg end)
  "interactive version of func"
  (interactive "r")
  (ansi-color-apply-on-region beg end))

;; tempbuf-mode
(require 'tempbuf)
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'w3-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'Man-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'view-mode-hook 'turn-on-tempbuf-mode)
;; It may also be reasonable to activate it by default on any visited
;; file buffer (buffers with unsaved content will not get automatically
;; deleted, anyway):
(add-hook 'find-file-hooks 'turn-on-tempbuf-mode)

;; ido
(require 'ido)
(ido-mode 1)
(setq ido-show-dot-for-dired 1)

;; Set environment variable for git
(setenv "GIT_PAGER" "cat")

;; Magit https://magit.vc/manual/magit/Getting-started.html#Getting-started
(global-magit-file-mode 1)

;; python-mode.el
(require 'python-mode)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; http://www.flycheck.org/en/latest/user/installation.html
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
;;(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)

;; Tramp
(require 'tramp)
(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
;; https://www.emacswiki.org/emacs/TrampAndDocker
;; Open files in Docker containers like so: /docker:drunk_bardeen:/etc/passwd
(push
 (cons
  "docker"
  '((tramp-login-program "docker")
    (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-args ("-i") ("-c"))))
 tramp-methods)
(defadvice tramp-completion-handle-file-name-all-completions
  (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | perl -we 'use strict; $_ = <>; m/^(.*)NAMES/ or die; my $offset = length($1); while(<>) {substr($_, 0, $offset, q()); chomp; for(split m/\\W+/) {print qq($_:\n)} }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it))

;; Open Matlab files in octave-mode
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.php
(require 'org-install)
(setq org-log-done t)
;; (add-hook 'org-mode-hook #'visual-line-mode)
;; (add-hook 'org-mode-hook #'auto-fill-mode)
(setq org-startup-indented t)

(if (file-exists-p "~/.emacs.d/desktop.el")
    (load "~/.emacs.d/desktop"))

(setenv "PATH" (concat "~/bin:" (getenv "PATH")))
(setq exec-path (append '("/sw/bin") exec-path))

;; start on full screen
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
(split-window-right)


;; add move line https://www.emacswiki.org/emacs/MoveLine
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)


;;deactivate sound
(setq visible-bell t)


;;http://ergoemacs.org/emacs/whitespace-mode.html
(progn
 ;; Make whitespace-mode with very basic background coloring for whitespaces.
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))

  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end
  ;; of line char and “▷” for tab.
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [182 10]) ; LINE FEED,
          (tab-mark 9 [9655 9] [92 9]) ; tab
          )))
