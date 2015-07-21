;;; buffer-pack.el --- Buffer manipulation

;;; Commentary:

;;; Code:

(require 'install-packages-pack)
(install-packages-pack/install-packs '(multiple-cursors
                                       move-text
                                       git-gutter
                                       s
                                       dash
                                       projectqile
                                       ace-jump-mode
                                       buffer-move
                                       iy-go-to-char
                                       popwin
                                       dockerfile-mode
                                       markdown-toc
                                       company
                                       ht
                                       nix-mode
                                       iedit
                                       switch-window))

(require 'switch-window)

(custom-set-variables
 '(switch-window-shortcut-style 'qwerty))

(require 'iedit)
(require 'markdown-toc)
(require 'multiple-cursors)
(require 'git-gutter)
(require 'buffer-move)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Extend the default company mode mapping to the one
(add-hook 'company-mode-hook (lambda ()
                               (interactive)
                               (define-key company-active-map (kbd "C-h") 'delete-backward-char)
                               (define-key company-active-map (kbd "M-?") 'company-show-doc-buffer)
                               (define-key company-active-map (kbd "C-n") 'company-select-next)
                               (define-key company-active-map (kbd "C-p") 'company-select-previous)
                               (define-key company-active-map (kbd "C-/") 'company-complete)))

(require 'projectile)
(projectile-global-mode)

;; Ace jump mode
(require 'ace-jump-mode)

;; go to char
(require 'iy-go-to-char)

;; use popwin to master the popup buffer and C-g to stop them
(require 'popwin)
(popwin-mode 1)

;; (require 'dired)
;; (add-hook 'dired-mode-hook
;;           (define-key dired-mode-map (kbd "q") (lambda () (interactive) (quit-window t))))

(require 's)

(require 'etags)

(require 'whitespace)
(setq whitespace-line-column 80);; increase this if not happy about 80 columns

;; activate clipboard
(setq x-select-enable-clipboard t)

(defun rotate-windows ()
  "Rotate your windows."
  (interactive)
  (cond
   ((not (> (count-windows) 1)) (message "You can't rotate a single window!"))
   (t
    (let ((i 1)
          (num-windows (count-windows)))
      (while  (< i num-windows)
        (let* ((w1 (elt (window-list) i))
               (w2 (elt (window-list) (+ (% i num-windows) 1)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1  b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1+ i))))))))

(defun buffer-pack/--goto-line (n)
  "Internal 'goto-line' to go the line number N."
  (goto-char (point-min))
  (forward-line (1- n)))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (let ((git-gutter-activated-p git-gutter-mode))
    (unwind-protect
        (progn
          (when git-gutter-activated-p (git-gutter-mode 0))
          (linum-mode 1)
          (buffer-pack/--goto-line (read-number "Goto line: ")))
      (progn
        (linum-mode -1)
        (when git-gutter-activated-p (git-gutter-mode 1))))))

;; Auto refresh buffers (not active by default)
;;(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(add-hook 'ido-setup-hook
          (lambda () ;; ~ to go straight home, // to go in /
            (define-key ido-file-completion-map (kbd "~") (lambda ()
                                                            (interactive)
                                                            (if (looking-back "/")
                                                                (insert "~/")
                                                              (call-interactively 'self-insert-command))))))

(setq
 inhibit-splash-screen t ;; Do not show a splash screen.
 echo-keystrokes 0.1    ;; Show incomplete commands while typing them.
 visible-bell t         ;; Flash the screen on errors.
 column-number-mode t)  ;; column number in the modeline

(defalias 'yes-or-no-p 'y-or-n-p) ;; "y" resp. "n" instead of "yes" resp. "no".

;; -----------------------------

(require 'ht) ;; ease hash-table manipulation
(require 'dash)

(eval-after-load "dash" '(dash-enable-font-lock))

(defvar BUFFER-PACK/LAST-BUFFER (ht-create)
  "Last buffer visited when switching to term.")

(defconst BUFFER-PACK/TERM-BUFFER "buffer-pack-term"
  "The term buffer's name in which the term buffer is running.")

(defun buffer-pack/term-name (buffer-name)
  "Compute the BUFFER-NAME's name."
  (format "*%s*" buffer-name))

(defun buffer-pack/switch-to-process (buffer-name fn)
  "Switch to process corresponding to buffer BUFFER-NAME.
If BUFFER-NAME does not exist, then spawn the process with FN and switch to it."
  (-if-let (buffer-process- (get-buffer-process (buffer-pack/term-name buffer-name)))
      (let ((buffer (process-buffer buffer-process-)))
        (if (buffer-live-p buffer)
            (pop-to-buffer buffer)
          (funcall fn)))
    (funcall fn)))

(defun buffer-pack/switch-to-term! ()
  "Select the term buffer, when possible in an existing window.
The buffer chosen is based on the file open in the current buffer."
  (interactive)
  (let ((current-buf (current-buffer)) ;; current-buffer-name from which we switch to
        (term-buffer (buffer-pack/switch-to-process BUFFER-PACK/TERM-BUFFER (lambda () (ansi-term "zsh" buffer-name)))))
    (ht-set BUFFER-PACK/LAST-BUFFER term-buffer current-buf)))

(defun buffer-pack/switch-to-last-buffer! ()
  "Switch to the last buffer from whence we come to term."
  (interactive)
  (let ((term-buffer (current-buffer)));; this is the term buffer
    (message "current-buffer '%s' " (buffer-name term-buffer))
    (-when-let (last-buffer (ht-get BUFFER-PACK/LAST-BUFFER term-buffer))
      (message "Trying to switch from '%s' to '%s'" (buffer-name term-buffer) last-buffer)
      (when (buffer-live-p last-buffer)
        (pop-to-buffer last-buffer)))))

(defun buffer-pack/switch-to-term-or-get-back-to-buffer! ()
  "If on terminal switch to last buffer from whence we came.
Otherwise, we go inside a terminal."
  (interactive)
  (funcall (if (string= (buffer-pack/term-name BUFFER-PACK/TERM-BUFFER) (buffer-name (current-buffer)));; on buffer
               'buffer-pack/switch-to-last-buffer!
             'buffer-pack/switch-to-term!)))

;; -----------------------------

(defun buffer-pack/backward-upcase-word! ()
  "Backward upper case word."
  (interactive)
  (upcase-word -1))

(defvar buffer-pack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c j") 'ace-jump-mode)
    (define-key map (kbd "C-c g f") 'iy-go-to-char)
    (define-key map (kbd "C-c g b") 'iy-go-to-char-backward)

    (define-key map (kbd "M-/") 'company-complete)
    (define-key map (kbd "C-h") 'delete-backward-char)
    (define-key map (kbd "C-M-h") 'backward-kill-word)
    (define-key map (kbd "M-?") 'help-command)

    ;; multiple-cursors
    (define-key map (kbd "C->") 'mc/mark-next-like-this)
    (define-key map (kbd "C-<") 'mc/mark-previous-like-this)
    (define-key map (kbd "C-c C-<") 'mc/mark-all-like-this)

    (define-key map [remap goto-line] 'goto-line-with-feedback)

    (define-key map (kbd "C-x C-r") 'rgrep)

    (define-key map (kbd "C-+") 'text-scale-increase)
    (define-key map (kbd "C--") 'text-scale-decrease)

    (define-key map (kbd "C-w") 'kill-region)
    (define-key map (kbd "C-y") 'yank)

    (define-key map (kbd "C-v") (lambda () (interactive) (forward-line 10)))
    (define-key map (kbd "M-v") (lambda () (interactive) (forward-line -10)))

    (define-key map (kbd "C-c r r") (lambda () (interactive) (revert-buffer nil t)))

    ;;scroll other window
    (define-key map (kbd "C-M-]") 'scroll-other-window)
    (define-key map (kbd "C-M-[") 'scroll-other-window-down)

    ;; Align your code in a pretty way.
    (define-key map (kbd "C-x \\") 'align-regexp)

    ;; Window switching.
    (define-key map (kbd "C-x O") (lambda () (interactive) (other-window -1)))  ;; back one
    (define-key map (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

    ;;window and buffer movement
    (define-key map (kbd "C-c w r") 'rotate-windows)
    (define-key map (kbd "C-c w p") 'buf-move-up)
    (define-key map (kbd "C-c w n") 'buf-move-down)
    (define-key map (kbd "C-c w b") 'buf-move-left)
    (define-key map (kbd "C-c w f") 'buf-move-right)
    (define-key map (kbd "C-c w .") 'shrink-window-horizontally)
    (define-key map (kbd "C-c w ,") 'enlarge-window-horizontally)
    (define-key map (kbd "C-c w /") (lambda () (interactive) (enlarge-window -1)))
    (define-key map (kbd "C-c w '") (lambda () (interactive) (enlarge-window 1)))
    (define-key map (kbd "C-c w s") 'switch-window)

    (define-key map (kbd "C-c b u") 'browse-url-at-point)
    (define-key map (kbd "C-c b U") 'browse-url)

    (define-key map (kbd "C-c M-z") 'buffer-pack/switch-to-term-or-get-back-to-buffer!)

    (define-key map (kbd "C-M-SPC") 'er/expand-region)
    (define-key map (kbd "C-c b ;") 'iedit-mode)

    map)
  "Keymap for Buffer-pack mode.")

(define-minor-mode buffer-pack-mode
  "Minor mode to consolidate Emacs' buffer-pack extensions.

\\{buffer-pack-mode-map}"
  :lighter " BP"
  :keymap buffer-pack-mode-map)

(define-globalized-minor-mode global-buffer-pack-mode buffer-pack-mode buffer-pack-on)

(defun buffer-pack-on ()
  "Turn on `buffer-pack-mode'."
  (buffer-pack-mode +1))

(global-buffer-pack-mode)

;; Override some default mapping to the minibuffer
;; (add-hook 'minibuffer-setup-hook (lambda ()
;;                                    (define-key minibuffer-local-map (kbd "C-h") 'backward-kill-char)
;;                                    (define-key minibuffer-local-map (kbd "C-M-h") 'backward-kill-word)))

(global-prettify-symbols-mode 1)

(setq load-prefer-newer t)

(provide 'buffer-pack)
;;; buffer-pack.el ends here
