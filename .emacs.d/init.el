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

;; move to other window, more ergonomic than "C-x o"
(global-set-key (kbd "C-o") 'other-window)

;; move forward one paragrahp # 2
(global-set-key (kbd "C-.") 'forward-paragraph)
;; move backward one paragrahp
(global-set-key (kbd "C-,") 'backward-paragraph)

;; special movement for programming modes
(defun my-backw-paragraph-or-fun-start ()
  "Beginning of paragraph or def if Python mode."
  (interactive)
  (if (equal major-mode 'python-mode)
      (beginning-of-python-def-or-class)
    (backward-paragraph)))

(defun my-forw-paragraph-or-fun-end ()
  "End of paragraph or def if Python mode."
  (interactive)
  (if (equal major-mode 'python-mode)
      (end-of-python-def-or-class)
    (forward-paragraph)))

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
;; https://github.com/flycheck/flycheck/issues/1437
(setq flycheck-python-pylint-executable "pylint")
;; use "pylintrc" instead of ".pylintrc" for config file
(setq flycheck-pylintrc "pylintrc")


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


;; delete marked text when typing
(pending-delete-mode t)


;; expand region, requires expand-region package
(global-set-key (kbd "C-+") 'er/expand-region)
(global-set-key (kbd "C--") (lambda () (interactive) (er/expand-region -1)))


;; IVY-SWIPER-COUNSEL STACK
;; replaces everything search related
;; use C-n, C-p to navigate through listed findings
;; use M-o to show options for selection

;; requires ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(setq ivy-use-selectable-prompt t) ;; make file open prompt selectable, if names overlap
;; requires swiper
(global-set-key "\C-s" 'swiper)
;; requires counsel
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c r") 'counsel-git-grep)
(global-set-key (kbd "C-c C-o") 'ivy-occur)
;; regex fuzzy matching
;; (setq ivy-re-builders-alist
;;       '((t . ivy--regex-fuzzy)))

;; requires wgrep package
;; makes grep results editable

;; E.g., if you have ivy candidates, press C-c C-o (ivy-occur) to
;; bring them to their own buffer, then C-x C-q (or just "e")
;; (ivy-wgrep-change-to-wgrep-mode) to make the buffer editable, then
;; C-c C-c to apply the changes. This can be used, for instance, to
;; rename a variable in a whole folder with counsel-get-grep.
(require 'wgrep)
(add-hook 'ivy-occur-grep-mode-hook
          (lambda () (local-set-key (kbd "e") #'ivy-wgrep-change-to-wgrep-mode)))

;; hydra, "sticky" modifier keys, requires hydra
(defhydra hydra-move (global-map "C-ö")
  "move like in modal editor"
  ("p" previous-line "line u")
  ("n" next-line "line d")
  ("k" previous-line)  ;; as in vim
  ("j" next-line)  ;; as in vim
  ("," backward-paragraph "§ u")
  ("." forward-paragraph "§ d")
  (";" my-backw-paragraph-or-fun-start "§ u")
  (":" my-forw-paragraph-or-fun-end "§ d")
  ("f" forward-char "char fw")
  ("b" backward-char "char bw")
  ("w" forward-word "word fw")  ;; as in vim
  ("F" forward-word "word fw")
  ("W" backward-word "word bw")
  ("B" backward-word "word bw")
  ("a" my-beginning-of-line-or-prev-line-dwim "line bg")
  ("e" my-end-of-line-or-next-line-dwim "line end")
  ("v" scroll-up-command "page up")
  ("V" scroll-down-command "page dw")
  ("<" beginning-of-buffer "file bg")
  (">" end-of-buffer "file end")
  ("C-SPC" set-mark-command "mk")
  ("m" set-mark-command)
  ("c" kill-ring-save "cp")
  ("d" my-cut-or-delete-dwim "cut")
  ("D" backward-kill-word "bk kill word")
  ("x" kill-region)
  ("y" yank "yank")
  ("u" undo "undo")
  ("h" mark-whole-buffer "mk all")
  ("s" swiper "search")
  ("r" rectangle-mark-mode "rect")
  ("+" er/expand-region)
  ("-" (lambda () (interactive) (er/expand-region -1)))
  ("#" comment-dwim "comment")
  ("<up>" move-line-up "line up")
  ("<down>" move-line-down "line down")
  ("o" other-window "other window")
  ("q" nil "quit")
  ("i" nil "quit"))

;; The most common move commands now activate hydra-move; e.g., when
;; pressing C-n to move to the next line, hydra-move is activated, so
;; that just pressing n will move one line further. That means that
;; for instance pressing C-n n n moves 3 lines forward, C-p b moves
;; back one line and one character, etc. The reasoning behind this is
;; that often I don't bother to activate the move hydra (C-ö) because
;; the first command doesn't do anything, making it less
;; economical. However, I often want to move more than, say, one word
;; forward, so I would like to activate the move hydra nonetheless.

(defun my-next-line-hydra ()
  "Move next line and activate move hydra."
  (interactive)
  (hydra-move/next-line))
(global-set-key (kbd "C-n") 'my-next-line-hydra)
;; (define-key minibuffer-local-map "C-n" [next-line])
;; de-activate in ivy-minibuffer
(define-key ivy-minibuffer-map (kbd "C-n") #'ivy-next-line)

(defun my-prev-line-hydra ()
  "Move prev line and activate move hydra."
  (interactive)
  (hydra-move/previous-line))
(global-set-key (kbd "C-p") 'my-prev-line-hydra)
;; de-activate in ivy-minibuffer
(define-key ivy-minibuffer-map (kbd "C-p") #'ivy-previous-line)

(defun my-forward-char-hydra ()
  "Move forward char and activate move hydra."
  (interactive)
  (hydra-move/forward-char))
(global-set-key (kbd "C-f") 'my-forward-char-hydra)

(defun my-backward-char-hydra ()
  "Move backward char and activate move hydra."
  (interactive)
  (hydra-move/backward-char))
(global-set-key (kbd "C-b") 'my-backward-char-hydra)

(defun my-forward-word-hydra ()
  "Move forward word and activate move hydra."
  (interactive)
  (hydra-move/forward-word))
(global-set-key (kbd "M-f") 'my-forward-word-hydra)

(defun my-backward-word-hydra ()
  "Move backward word  and activate move hydra."
  (interactive)
  (hydra-move/backward-word))
(global-set-key (kbd "M-b") 'my-backward-word-hydra)

(defun my-forward-paragraph-hydra ()
  "Move forward paragraph and activate move hydra."
  (interactive)
  (hydra-move/forward-paragraph))
(global-set-key (kbd "C-.") 'my-forward-paragraph-hydra)
(global-set-key (kbd "C-:") 'hydra-move/my-forw-paragraph-or-fun-end)

(defun my-backward-paragraph-hydra ()
  "Move backward paragraph and activate move hydra."
  (interactive)
  (hydra-move/backward-paragraph))
(global-set-key (kbd "C-,") 'my-backward-paragraph-hydra)
(global-set-key (kbd "C-;") 'hydra-move/my-backw-paragraph-or-fun-start)


;; hydra for flycheck, see https://github.com/abo-abo/hydra/wiki/Flycheck
(defhydra hydra-flycheck
    (:pre (flycheck-list-errors)
     :post (quit-windows-on "*Flycheck errors*")
     :hint nil)
  "Errors"
  ("f" flycheck-error-list-set-filter "Filter")
  ("j" flycheck-next-error "next")
  ("n" flycheck-next-error "next")
  ("k" flycheck-previous-error "prev")
  ("p" flycheck-previous-error "prev")
  ("g" flycheck-first-error "First")
  ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q" nil))

;; Run "C-c ! !" to go to the first flycheck error and start the hydra
(global-set-key (kbd "C-c ! !") 'hydra-flycheck/flycheck-next-error)

;;;;;;;;;;;;;;
;; ORG MODE ;;
;;;;;;;;;;;;;;

;; where to find org files
(setq org-agenda-files (list "~/Dropbox/Wohnung/Umzug/Aufgaben.org"
                             "~/work/orga"))
(setq org-default-notes-file "~/work/orga/notes.org")

;; org mode TODO states
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "WAITING" "DONE")))

;; agenda view 1 month
(setq org-agenda-span 'month)

;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.php
(require 'org-install)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.php
(require 'org-install)
(setq org-log-done t)
;; (add-hook 'org-mode-hook #'visual-line-mode)
;; (add-hook 'org-mode-hook #'auto-fill-mode)
(setq org-startup-indented t)

;; when using org-refile (C-c C-w), discovers trees from all
;; registered org files up to given level
(setq org-refile-targets '((nil :maxlevel . 2)
                           (org-agenda-files :maxlevel . 1)))
;; tip: Use C-u C-c C-w to jump to the heading without moving anything


;;;;;;;;;;;;;;;;;;
;; END ORG MODE ;;
;;;;;;;;;;;;;;;;;;

;; reload buffers when opening
(desktop-save-mode 1)

;; Set C-w to backward kill word if no text selected, more sane
;; behavior IMO
(defun my-kill-word-or-region-dwim ()
  "If region active kill it from START to END else backward kill word."
  ;; Don't use `(interactive "r") (start end)` since that doesn't work
  ;; when no mark is set (e.g. in a completely new buffer).
  (interactive)
  (let ((start (mark)) (end (point)))
  (if (use-region-p)
      (kill-region start end)
    (backward-kill-word 1))))
(global-set-key (kbd "C-w") 'my-kill-word-or-region-dwim)

(defun my-cut-or-delete-dwim (&optional arg)
  "If region active kill it from start to end else kill ARG words (default=1)."
  ;; Don't use `(interactive "r") (start end)` since that doesn't work
  ;; when no mark is set (e.g. in a completely new buffer). This
  ;; command is not attached to a keybinding but is used during the
  ;; move hydra to somewhat emulate vim's delete word.
  (interactive "p")
  (or arg (setq arg 1))
  (let ((start (mark)) (end (point)))
    (if (use-region-p)
        (kill-region start end)
      (kill-word arg))))

;; Set C-e to go to the end of the line or, when already at the end,
;; to the end of the next line
(defun my-end-of-line-or-next-line-dwim ()
  "Move to EOL; if already EOL, move next line and EOL."
  (interactive)
  (if
      (eq (line-end-position) (point))
      (move-end-of-line 2)
    (move-end-of-line 1)))
(global-set-key (kbd "C-e") 'my-end-of-line-or-next-line-dwim)

;; Set C-a to go to the beginning of the line or, when already at the
;; beginning, to the beginning of the previous line
(defun my-beginning-of-line-or-prev-line-dwim ()
  "Move to BOL; if already BOL, move prev line and BOL."
  (interactive)
  (if
      (eq (line-beginning-position) (point))
      (move-beginning-of-line 0)
    (move-beginning-of-line nil)))
(global-set-key (kbd "C-a") 'my-beginning-of-line-or-prev-line-dwim)


;;;;;;;;;;;
;; LOOKS ;;
;;;;;;;;;;;

;; add themes folder
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; use zenburn as default
(load-theme 'zenburn t)

;; hide scroll bar
(scroll-bar-mode -1)

;; requires powerline package; adds a nice looking powerline to the bottom of emacs
(require 'powerline)
(powerline-default-theme)

;; requires pyvenv; use pyvenv-workon to choose python env
(setenv "WORKON_HOME" "~/anaconda3/envs/")

;; highlight cursor, requires beacon.el (https://github.com/Malabarba/beacon)
(beacon-mode 1)

;; CRUX commands, requires crux package

;; smart move to beginning of line: first indented, then very start, then toggle
(global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
;; override C-a in org mode, which has its own command
;; (define-key org-mode-map (kbd "C-a") nil)

;; First kill to end of line, then kill the whole line
(global-set-key (kbd "C-k") 'crux-smart-kill-line)
;; override C-k in org mode, which has its own command
;; (define-key org-mode-map (kbd "C-k") nil)


;; google marked region or prompt
;; https://batsov.com/articles/2011/11/19/why-emacs/
(defun my-invoke-google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))
(global-set-key (kbd "C-c C-g") 'my-invoke-google)

(require 'org-re-reveal)

(global-set-key (kbd "<S-SPC>") 'dabbrev-expand)


(defun my-switch-shell-run-last-cmd ()
  "Switch to shell buffer and run last cmd."
  (interactive)
  (let ((bname (buffer-name)))
    (if (not (string= bname "*shell*"))
        (other-window 1))
    (switch-to-buffer "*shell*")
    (end-of-buffer)
    (comint-previous-input 1)
    (comint-send-input)
    (if (not (string= bname "*shell*"))
        (other-window 1))))
(global-set-key (kbd "C-S-o") 'my-switch-shell-run-last-cmd)
