;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright 2019 Christian Michael Baum. MIT Licensed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Most of the following code has to do with/were added by the package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;https://emacs.stackexchange.com/questions/51721/failed-to-download-gnu-archive
(setq package-check-signature nil)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom-set-variable (see generated comment below)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(auth-source-save-behavior nil)
 '(aweshell-auto-suggestion-p nil)
 '(beacon-color "#f2777a")
 '(custom-enabled-themes '(doom-horizon))
 '(custom-safe-themes t)
 '(eclim-eclipse-dirs '("/usr/lib/eclipse"))
 '(eclim-executable
   "~/.eclipse/org.eclipse.platform_4.10.0_155965261_linux_gtk_x86_64/plugins/org.eclim_2.8.0/bin/eclim")
 '(eclimd-default-workspace "~/prog/eclipse")
 '(eclimd-executable
   "~/.eclipse/org.eclipse.platform_4.10.0_155965261_linux_gtk_x86_64/eclimd")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'light)
 '(line-number-mode nil)
 '(package-selected-packages
   '(nyan-mode transmission sudoku "howdoi" howdoi treemacs "treemacs" "treemacs" company-jedi jedi eshell-prompt-extras forge "fortune" company-irony-c-headers howdoyou aweshell jdee helm-eww pacmacs bind-key gitlab typescript-mode awesome-tab helm-tramp helm-wikipedia helm flycheck-irony irony gdb-mi hydra lv quelpa-use-package quelpa meson-mode rust-mode kotlin-mode package-selected-packages docker dockerfile-mode esh-autosuggest eshell-syntax-highlighting eshell-outline eshell-git-prompt snow fsharp-mode jazz-theme purp-theme use-package php-mode god-mode evil all-the-icons all-the-icons-dired all-the-icons-gnus vterm yaml-mode ggtags flycheck-clang-analyzer free-keys powershell flymd npm-mode ac-emacs-eclim no-littering markdown-mode web-mode rainbow-mode eclim company company-emacs-eclim flycheck-popup-tip magit color-theme-sanityinc-tomorrow test-c chess neotree tide tss dotnet spacemacs-theme doom-themes dumb-jump flycheck color-theme-modern))
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install selected packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; setup package and use-package
(package-initialize)

;; override the default http with https
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))

;; add melpa to the front
(add-to-list 'package-archives
            '("melpa" . "https://melpa.org/packages/") t)

;; Bootstrap `use-package'
;; http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
;; use-package autoloads will make sure it get pulled in at the right time
;; read "package autoloads":  http://www.lunaryorn.com/2014/07/02/autoloads-in-emacs-lisp.html
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; bootstrap quelpa to use-package
(setq quelpa-upgrade-interval 7)

(use-package quelpa-use-package
  :demand t
  :init
  (setq quelpa-use-package-inhibit-loading-quelpa t)
  (unless (package-installed-p 'quelpa-use-package)
    (quelpa
     '(quelpa-use-package
       :fetcher git
       :url "https://github.com/quelpa/quelpa-use-package.git"))))

(require 'quelpa-use-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quelpa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package gdb-mi :quelpa (gdb-mi :fetcher git
;;                                    :url "https://github.com/weirdNox/emacs-gdb.git"
;;                                    :files ("*.el" "*.c" "*.h" "Makefile")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; base config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove temp files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; remove lock files
(setq create-lockfiles nil)

;; hide splash screen

(setq inhibit-splash-screen t)

;; disable tab indent globally
;; (setq-default indent-tabs-mode nil)
;; I changed my mind lol
(setq-default tab-width 4)

;; bind compile
(global-set-key (kbd "C-x g") 'compile)

;; dumb jump
(global-set-key (kbd "C-M-g") 'dumb-jump-go)

;; string manipulation
(require 'subr-x)

;; custom elisp files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))

(setq text-quoting-style 'grave)

;; fundamental mode
(setq initial-major-mode 'fundamental-mode) 
(setq totoro-ascii
"                             jM$$s              (M$$c
                            (M$$$Se            8X$440
                            EE$$$$j     '.    .M$$$$0
                            MM$$$$p,     |    ;MM$$$`
                             M$$$$X;nnnnnnNnnnMM$$$p
                             MMM$$$$$nnnnnnNnn$$$$$,
                           ,SMM$$$$$$$$$$$$$$$$$$$$p,
                          ,SMMM\"._`;$$$$$333$$$r`,_\"$m    ..
                 ~~+,...,O$KK$$ \"%‘$$$$$M###M$$.(%\" /)$\\-’‘
                ~.....,.OMMMO4$$p;q$$$$$$$$$$$$p;qo$$$=-~’’’’‘‘
                      _MMMMMM$$$$$$$$$$$$$$$$$$$$$$$$$$$-~.,,,_
                 ~--’’’MMMMMM$$$$$$$$$$$MMM$$$$$$$$$$$$$p,
                    .eMMMMMMM$$$$$$$$$$$$$$$$$$$$$$$$$$$$$c
                   oMMMMMMMMMM$r\"`              \"C$$$$$$$$$$,
                 0MMMMMMMMM\"`          ____         ‘^4$$$$$$$,
                4MMMMMMMP:  ,,,,    .oXP\"\"tP   .OMM;.  ‘\"$$.$$$$
               0PMMMMMP:::.O*\"\"t’              ’‘ ‘’M.    ’$.$$$$
              dMMMMMMP:::         ,.,     ,ooo,      __    ’$$$$$;
             ,MMMMMM{:::.mm.    oD\"\"oM   ;^\"\"^&;.  .$\"\"$.   ’$$$$$.
             ;MMMMMM:::i’\"^t‘                                $$$$$$             
             #MMMMMM::::                                     ’$$$$$$
             IMMMMMM:::.                                      $$$$$$
             {MMMMM&::::                                      $$$$$$
              ’MMMMM::::                                     ’$$$$$’
               ’MMM:::::.                                    $$$$$’
                ’MM::::::._,,_                       _..._ ..’$$$$’
                 VV::::;d$$$$$op.                 .cdM’$$$$$:$$$’
                   :::;MM$$$$$$$$.              .MM;$$$$$$$$o$$’
                    ::MM$$$$$$$$$$.           .;MMM$$$$$$$$$;
                     :MM$$$$$$$$$$$::........::MMX$$$$$$$$$j
                      ’MX$$$$$$$$$$::::::::::::MMX$$$$$$$$$‘
")

;; Tab width to 4 
(setq default-tab-width 4)

;; Bind right click to context menu
(global-set-key [mouse-3] 'menu-bar-open)

;; Unbind F10 because I like it for other apps
(global-set-key [f10] nil)

;; Disable kill buffer confirmation
;https://emacs.stackexchange.com/a/14511
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
;https://emacs.stackexchange.com/a/52677
(setq confirm-kill-processes nil)

(setq eshell-banner-message "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-scratch config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(require 'multi-scratch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; awesome-tab config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package awesome-tab
  :quelpa (awesome-tab :fetcher github :repo "manateelazycat/awesome-tab"),
  :files ("*.el"))

;; Setup git packages
;; https://github.com/manateelazycat/awesome-tab


(require 'awesome-tab)
(awesome-tab-mode t)

(defun kill-matching-buffers-no-ask (regexp)
  "Kill buffers whose names match REGEXP, without asking."
  (interactive)
  (cl-letf (((symbol-function 'kill-buffer-ask) #'kill-buffer)) (kill-matching-buffers regexp)))

(defun awesome-tab-click-close-tab (event)
  "CLose buffer (obtained from EVENT) on clicking header line"
  (interactive "e")
  (let ((selected-tab-name
         (substring (string-trim (car (posn-string (event-start event)))) 2)))
    (when (eq 3 (- (length selected-tab-name)
                   (or (cl-search "..." selected-tab-name) 0)))
      (setq selected-tab-name (substring selected-tab-name 0 -3)))
    (unless (string-match-p "^%-$" selected-tab-name)
      (kill-matching-buffers-no-ask selected-tab-name))))

(global-set-key (kbd "<C-next>") 'awesome-tab-forward)
(global-set-key (kbd "<C-prior>") 'awesome-tab-backward)
(global-set-key [tab-line mouse-2] #'awesome-tab-click-close-tab)
(global-set-key [tab-line mouse-3] #'multi-scratch-new)

;; Tab groups

(defun awesome-tab-buffer-groups-mine ()
  "`awesome-tab-buffer-groups' control buffers' group rules.

Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `awesome-tab-get-group-name' with project name."
  (list
   (cond
    ((and
      (<= 8 (length (buffer-name)))
      (or
       (string-equal "*scratch" (substring (buffer-name) 0 8))
       (string-equal "*multi-s" (substring (buffer-name) 0 8)))
      )
     "Scratch")
    ((memq major-mode '(magit-process-mode
                        magit-status-mode
                        magit-diff-mode
                        magit-log-mode
                        magit-file-mode
                        magit-blob-mode
                        magit-blame-mode
                        ))
     "Magit")
    ((or
      (string-equal "*PowerShell*" (buffer-name))
      (derived-mode-p 'shell-mode)
      (derived-mode-p 'eshell-mode)
      (derived-mode-p 'term-mode)
      (derived-mode-p 'vterm-mode))
     "Terminal")
    ((derived-mode-p 'emacs-lisp-mode)
     "Elisp")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode")
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs")
    (t
     (awesome-tab-get-group-name (current-buffer))))))

(setq awesome-tab-buffer-groups-function 'awesome-tab-buffer-groups-mine)

;; tab height

(setq awesome-tab-height 110)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aesthetic settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(column-number-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-frame-font "Source Code Pro-10")
(setq ring-bell-function 'ignore)

;; all-the-icons neotree
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; all-the-icons poor performance in windows fix
(setq inhibit-compacting-font-caches t)

;; hide messages buffer

(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; hide startup message
;;(put 'inhibit-startup-echo-area-message 'saved-value t)
;;(setq inhibit-startup-echo-area-message (user-login-name))


;; default frame size
(add-to-list 'default-frame-alist '(height . 36))
(add-to-list 'default-frame-alist '(width . 100))

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(95 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Side bar settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: Neotree is no longer ther preferred side bar. Let's use treemacs!

;; https://emacs.stackexchange.com/questions/37678/neotree-window-not-resizable
;; Set the neo-window-width to the current width of the
;; neotree window, to trick neotree into resetting the
;; width back to the actual window width.
;; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
(setq neo-window-width 30)

(global-set-key [f8] 'treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (string= system-type "windows-nt")
  (setq explicit-shell-filename "C:/msys64/usr/bin/bash.exe")
  (setenv "SHELL" shell-file-name)
  (require 'fakecygpty)
  (fakecygpty-activate)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode)
(setq completion-ignore-case  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML / CSS / PHP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'html-mode-hook 'display-line-numbers-mode)
(add-hook 'css-mode-hook 'display-line-numbers-mode)
(add-hook 'scss-mode-hook 'display-line-numbers-mode)
(add-hook 'php-mode-hook 'display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .NET/ Omnisharp setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copied from Omnisharp-emacs README

(add-hook 'csharp-mode-hook 'omnisharp-mode)
(eval-after-load
    'company
  '(add-to-list 'company-backends 'company-omnisharp))
(add-hook 'csharp-mode-hook #'company-mode)
(add-hook 'csharp-mode-hook #'flycheck-mode)
(add-hook 'csharp-mode-hook #'display-line-numbers-mode)

;; Customized 
(setq omnisharp-server-executable-path "/usr/local/bin/omnisharp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; emacs-eclim
(when (string= system-type "gnu/linux")
  (setq eclimd-autostart t)
  (defun my-java-mode-hook ()
    (eclim-mode t))
  (add-hook 'java-mode-hook 'my-java-mode-hook)
  (company-emacs-eclim-setup)
  (ac-emacs-eclim-config))
(add-hook 'java-mode-hook #'display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typescript setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copied from tide's README
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(setq company-tooltip-align-annotations t)
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook #'display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup initial term page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Running Make with the closest Makefile
;; Modified, from https://www.emacswiki.org/emacs/CompileCommand
(require 'cl-lib) ; If you don't have it already

(cl-defun get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (expand-file-name
     (loop 
      for d = default-directory then (expand-file-name ".." d)
      if (file-exists-p (expand-file-name file d))
      return d                                        ;			return (substring d 0 (* -1 (string-width file)))
      if (equal d root)
			return nil))))
(require 'compile)
(add-hook 'c-mode-hook (lambda () (set (make-local-variable 'compile-command) (format "cd %s && make -k" (get-closest-pathname)))))

;;4 tabs, linux style (from GNU style)

(setq c-default-style "linux"
      c-basic-offset 4)

;; Emacs as C++ IDE
;; https://syamajala.github.io/c-ide.html

;; Source code completion using Irony (from Emacs as C++ IDE)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Using Company mode

(add-hook 'c-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'objc-mode-hook 'company-mode)

;; Using flycheck mode

(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'objc-mode-hook 'flycheck-mode)

;; Using Company with Irony (from Emacs as a C++ IDE)

(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-irony))

;; Header file completion with company-irony-c-headers

(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

;; Syntax checking with Flycheck

(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

;; line #s
(add-hook 'c++-mode-hook #'display-line-numbers-mode)
(add-hook 'c-mode-hook #'display-line-numbers-mode)

;; Integrating Irony with Flycheck

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; From Irony's README:
;; Windows performance tweaks

(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))



;; Remember to run irony-install-server

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; How to setup (WIP)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Download these:

;; https://github.com/manateelazycat/awesome-tab
;; https://www.emacswiki.org/emacs/multi-scratch.el

;; add above to elisp folder

;; I can't remember how to setup Melpa but it is not exactly automatic
;; even though I tried to make it automatic.
;; You may have to run emacs multiple times and run install-selected-packages

;; On Windows, follow these instructions: https://github.com/d5884/fakecygpty
;; on Windows, install msys such that C:/msys64/usr/bin/bash.exe exists

;; M-x all-the-icons-install-fonts
(put 'upcase-region 'disabled nil)

;; run irony-install-server

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aliases and scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun webmify (file)
  "Converts a file to webm using 'ffmpeg'"
  (shell-command
   (concat "ffmpeg -i " default-directory file " -vf scale=640:-1 -pass 1 -y -c:v libvpx -lag-in-frames 25 -auto-alt-ref 1 -b:v 400K -deadline best -cpu-used 0 -an -f webm nul && ffmpeg -i "  default-directory file" -vf scale=640:-1 -pass 2 -y -c:v libvpx -lag-in-frames 25 -auto-alt-ref 1 -b:v 400K -deadline best -cpu-used 0 -an " default-directory file "_out.webm &")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eshell config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'esh-module)
(add-to-list 'eshell-modules-list 'eshell-tramp)

(use-package aweshell
  :quelpa (aweshell :fetcher github :repo "manateelazycat/aweshell"),
  :files ("*.el"))

(setq eshell-rc-script "export DOTNET_CLI_TELEMETRY_OPTOUT=1")

(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))

(add-hook 'eshell-mode-hook #'company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tramp-allow-unsafe-temporary-files t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vterm setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
(add-hook 'vterm-mode
  (lambda ()
    (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eshell)


