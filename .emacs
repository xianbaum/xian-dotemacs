;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright 2019 Christian Michael Baum. MIT Licensed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Most of the following code has to do with/were added by the package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;https://emacs.stackexchange.com/questions/51721/failed-to-download-gnu-archive
(setq package-check-signature nil)
(package-initialize)

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
 '(ansi-color-names-vector
   ["#2b2a27" "#ff5d38" "#98be65" "#bcd42a" "#51afef" "#c678dd" "#46D9FF" "#ede0ce"])
 '(beacon-color "#f2777a")
 '(custom-enabled-themes (quote (doom-tomorrow-day)))
 '(custom-safe-themes t)
 '(eclim-eclipse-dirs (quote ("/usr/lib/eclipse")))
 '(eclim-executable
   "~/.eclipse/org.eclipse.platform_4.10.0_155965261_linux_gtk_x86_64/plugins/org.eclim_2.8.0/bin/eclim")
 '(eclimd-default-workspace "~/prog/eclipse")
 '(eclimd-executable
   "~/.eclipse/org.eclipse.platform_4.10.0_155965261_linux_gtk_x86_64/eclimd")
 '(fci-rule-color "#5B6268")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(jdee-db-active-breakpoint-face-colors (cons "#2b2a27" "#ff5d38"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#2b2a27" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#2b2a27" "#3f444a"))
 '(line-number-mode nil)
 '(objed-cursor-color "#ff5d38")
 '(package-selected-packages
   (quote
    (php-mode god-mode evil all-the-icons all-the-icons-dired all-the-icons-gnus vterm yaml-mode ggtags flycheck-clang-analyzer flycheck-irony company-irony company-irony-c-headers free-keys powershell flymd npm-mode ac-emacs-eclim irony no-littering markdown-mode web-mode rainbow-mode eclim company company-emacs-eclim flycheck-popup-tip magit borland-blue-theme color-theme-sanityinc-tomorrow xresources-theme test-c chess purp-theme jazz-theme seethru neotree tide tss dotnet spacemacs-theme doom-themes dumb-jump omnisharp flycheck color-theme-modern)))
 '(pdf-view-midnight-colors (cons "#ede0ce" "#2b2a27"))
 '(rustic-ansi-faces
   ["#2b2a27" "#ff5d38" "#98be65" "#bcd42a" "#51afef" "#c678dd" "#46D9FF" "#ede0ce"])
 '(vc-annotate-background "#2b2a27")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#a4c551")
    (cons 60 "#b0cc3d")
    (cons 80 "#bcd42a")
    (cons 100 "#c1a623")
    (cons 120 "#c5781c")
    (cons 140 "#cb4b16")
    (cons 160 "#c95a58")
    (cons 180 "#c7699a")
    (cons 200 "#c678dd")
    (cons 220 "#d96fa6")
    (cons 240 "#ec666f")
    (cons 260 "#ff5d38")
    (cons 280 "#cf563c")
    (cons 300 "#9f5041")
    (cons 320 "#6f4a45")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil)
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
(setq-default indent-tabs-mode nil)

;; bind compile
(global-set-key (kbd "C-x g") 'compile)

;; string manipulation
(require 'subr-x)

;; custom elisp files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))

(setq text-quoting-style 'grave)

;; fundamental mode
(setq initial-major-mode 'fundamental-mode) 
(setq initial-scratch-message
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
                        $$$$$$$$$$0::::::::::::9MM$$$$$$$$‘")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-scratch config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'multi-scratch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; awesome-tab config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(global-set-key [header-line mouse-2] #'awesome-tab-click-close-tab)
(global-set-key [header-line mouse-3] #'multi-scratch-new)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aesthetic settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(column-number-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(seethru 95)
(set-default-font "Source Code Pro-10")
(setq ring-bell-function 'ignore)

;; all-the-icons neotree
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; all-the-icons poor performance in windows fix
(setq inhibit-compacting-font-caches t)

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
;; .NET/ Omnisharp setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copied from Omnisharp-emacs README

(add-hook 'csharp-mode-hook 'omnisharp-mode)
(eval-after-load
    'company
  '(add-to-list 'company-backends 'company-omnisharp))
(add-hook 'csharp-mode-hook #'company-mode)
(add-hook 'csharp-mode-hook #'flycheck-mode)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup initial term page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/")

(global-set-key [f8] 'neotree-toggle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Running Make with the closest Makefile
;; Modified, from https://www.emacswiki.org/emacs/CompileCommand
(require 'cl) ; If you don't have it already

(defun* get-closest-pathname (&optional (file "Makefile"))
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
(add-hook 'c-mode-hook (lambda () (set (make-local-variable 'compile-command) (format "cd %s && make -i" (get-closest-pathname)))))

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
