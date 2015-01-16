(require 'package)
(add-to-list 'package-archives
'("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; ================= MY MODIFICATIONS ===============================
;; ****** SPECIFIC FOR WINDOWS  *******************
;; Use 10-pt Consolas as default font
;;(set-face-attribute 'default nil
;;                    :family "Consolas" :height 120)
;; ************************************************

;; store all backup and autosave files in the one dir
(setq backup-directory-alist `(("." . "~/.tilda")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; self added definitions of standard funtions
(menu-bar-showhide-tool-bar-menu-customize-disable)
(menu-bar-no-scroll-bar)
(menu-bar-mode -99)
(global-linum-mode)

;; set custom themes folder and font
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(set-face-attribute 'default nil
                    :family "Ubuntu Mono"
                    :height 130
                    :weight 'normal
                    :width 'normal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; =============== MY MODIFICATIONS END ===============================

(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(defconst demo-packages
'(anzu
company
company-c-headers
duplicate-thing
ggtags
helm
helm-gtags
helm-swoop
function-args
clean-aindent-mode
comment-dwim-2
dtrt-indent
ws-butler
iedit
yasnippet
gist
sr-speedbar
smartparens
sml-mode
projectile
volatile-highlights
undo-tree
zygospore
monokai-theme))

(defun install-packages ()
"Install all required packages."
(interactive)
(unless package-archive-contents
(package-refresh-contents))
(dolist (package demo-packages)
(unless (package-installed-p package)
(package-install package))))

(install-packages)

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'setup-helm)
(require 'setup-helm-gtags)
;; (require 'setup-ggtags)
(require 'setup-cedet)
(require 'setup-editing)

(windmove-default-keybindings)

;; function-args
(require 'function-args)
(fa-config-default)
(define-key c-mode-map [(tab)] 'moo-complete)
(define-key c++-mode-map [(tab)] 'moo-complete)
(define-key c-mode-map (kbd "M-o") 'fa-show)
(define-key c++-mode-map (kbd "M-o") 'fa-show)
(define-key c-mode-map (kbd "M-j") 'fa-jump)
(define-key c++-mode-map (kbd "M-j") 'fa-jump)
;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(delete 'company-semantic company-backends)
(define-key c-mode-map [(control tab)] 'company-complete)
(define-key c++-mode-map [(control tab)] 'company-complete)

;; company-c-headers
(add-to-list 'company-backends 'company-c-headers)

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq
c-default-style "linux" ;; set style to "linux"
)

(global-set-key (kbd "RET") 'newline-and-indent) ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
(interactive)
(setq-local compilation-read-command nil)
(call-interactively 'compile)))

;; setup GDB
(setq
;; use gdb-many-windows by default
gdb-many-windows t

;; Non-nil means display source file containing the main routine at startup
gdb-show-main t
)

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; Package: ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)

(require 'gist)
;; Package: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; Package: projejctile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

;; Sr-speedbar: show directory of current buffer
(require 'sr-speedbar)
(setq speedbar-show-unknown-files t )

;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("0eebf69ceadbbcdd747713f2f3f839fe0d4a45bd0d4d9f46145e40878fc9b098" default)))
 '(scroll-bar-mode nil)
 '(semantic-c-dependency-system-include-path
   (quote
    ("/home/romanjoe/dev/linux-cortexm-1.12.0/linux/drivers" "/home/romanjoe/dev/linux-cortexm-1.12.0/linux/arch" "/home/romanjoe/dev/linux-cortexm-1.12.0/linux/include" "/home/romanjoe/dev/linux-cortexm-1.12.0/linux/kernel" "/home/romanjoe/dev/stm32/stm32_discovery_arm_gcc/STM32F4-Discovery_FW_V1.1.0/Libraries/CMSIS/ST/STM32F4xx/Include" "/home/romanjoe/dev/stm32/stm32_discovery_arm_gcc/STM32F4-Discovery_FW_V1.1.0/Libraries/CMSIS/Include" "/home/romanjoe/dev/stm32/stm32_discovery_arm_gcc/STM32F4-Discovery_FW_V1.1.0/Libraries/STM32F4xx_StdPeriph_Driver/inc")))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
