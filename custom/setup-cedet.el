(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-stickyfunc-mode 1)

(semantic-mode 1)

(defun alexott/cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'c-mode-hook 'alexott/cedet-hook)
(add-hook 'c++-mode-hook 'alexott/cedet-hook)

;; Enable EDE only in C/C++
(require 'ede)
(global-ede-mode)

(provide 'setup-cedet)

(defvar all-gud-modes
  '(gud-mode comint-mode gdb-locals-mode gdb-frames-mode  gdb-breakpoints-mode)
  "A list of modes when using gdb")
(defun kill-all-gud-buffers ()
  "Kill all gud buffers including Debugger, Locals, Frames, Breakpoints.
Do this after `q` in Debugger buffer."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (member major-mode all-gud-modes)
          (setq count (1+ count))
          (kill-buffer buffer)
          (delete-other-windows))) ;; fix the remaining two windows issue
      (message "Killed %i buffer(s)." count))))

(add-hook 'gud-mode-hook
          '(lambda ()
             (global-set-key (kbd "<f7>") 'gud-next)
             (global-set-key (kbd "<f8>") 'gud-step)
             (global-set-key (kbd "<f9>") 'gud-nexti)
;;             (global-set-key (kbd "<f10>") 'gud-next)
))
