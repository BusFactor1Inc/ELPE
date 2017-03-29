(setq lexical-binding t)
(require 'cl) ;; We're just like that

;; Turn on pareditmode by default
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

;; Add paredit mode to eshell
(add-hook 'eshell-mode-hook 'enable-paredit-mode)

(global-set-key "\M-;" 'other-window)
(global-set-key "\M-'" 'lisp-complete-symbol)

(set-frame-size (selected-frame) 160 78)

(info "/usr/share/info/eintr")

(split-window-horizontally)
(find-file "~/.emacs")
(switch-to-buffer ".emacs")
(split-window-vertically)
(switch-to-buffer "*scratch*")
(new-frame)
(eshell)
(other-window 1)
(split-window-vertically)

(switch-to-buffer "*info*")
(set-frame-size (selected-frame) 80 37)

(eldoc-mode t)

(defun screen-dimensions-macos ()
  (car (read-from-string (shell-command-to-string "system_profiler SPDisplaysDataType | grep Resolution | while read a width x height xs; do echo '(' $width '.'  $height ')'; done"))))

;;(setq rcirc-server-alist '(("irc.freenode.net" :channels ("#elpe"))))
;;(irc nil)
;;(setq rcirc-default-nic "burton-1`")
;;(irc nil)
;;(rcirc-join-channels (get-process "irc.freenode.net") (list "#ELPE"))
;;(switch-to-buffer "#ELPE@irc.freenode.net")
;;(rename-buffer "*Help Chat*")
(other-window 1)
(delete-other-windows)

(new-frame)

;; center windows on screen
(defun layout-windows-centered-on-screen ()
  (let* ((dimensions (screen-dimensions-macos))
	 (width (car dimensions))
	 (height (cdr dimensions))
	 (padding 8)

	 (frames (frame-list))
	 (frame-1 (car frames))
	 (frame-2 (cadr frames))
	 (frame-3 (caddr frames))
	 
	 (frame-1-w (frame-pixel-width frame-1))
	 (frame-1-h (frame-pixel-height frame-1))

	 (frame-2-w (frame-pixel-width frame-2))
	 (frame-2-h (frame-pixel-height frame-2))

	 (frame-3-w (frame-pixel-width frame-3))
	 (frame-3-h (frame-pixel-height frame-3))
	 
	 (frame-3-x (/ (- width frame-1-w frame-3-w) 2))
	 (frame-3-y (/ (- height frame-3-h) 2))

         ;; Better placement on small monitors.
         (when (< frame-3-x 0) (setf frame-3-x 0))
         (when (< frame-3-y 0) (setf frame-3-y 0))

	 (frame-2-x (+ frame-3-x frame-3-w padding))
	 (frame-2-y frame-3-y)

	 (frame-1-x (+ frame-3-x frame-3-w padding))
	 (frame-1-y (- (+ frame-3-y frame-3-h) frame-1-h)))

    (set-frame-position frame-1 frame-1-x frame-1-y)
    (set-frame-position frame-2 frame-2-x frame-2-y)
    (set-frame-position frame-3 frame-3-x frame-3-y)))

;; Run a given function-symbol (zero argument function) at a later time
(defmacro* later (function-symbol &optional (time "1 sec"))
  `(run-at-time ,time nil ',function-symbol))

;;(run-at-time "1 sec" nil 'layout-windows-centered-on-screen)
(later layout-windows-centered-on-screen)

;; smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1)

;; Override M-x shell to use eshell
(defun shell ()
  (interactive)
  (switch-to-buffer "*eshell*"))

;; Enable show-paren-mode
(show-paren-mode 1)

;; Bind M-, to toggle-paredit-mode
(setq *paredit-state* 1)
(defun toggle-paredit-mode ()
  (interactive)
  (message "Paredit Mode Toggled")
  (paredit-mode (setf *paredit-state* (if (=  *paredit-state* 0) 1 0))))

(defun* keybind-toggle-paredit-mode (&optional (keyspec "\M-,"))
  (global-set-key keyspec 'toggle-paredit-mode))

(later keybind-toggle-paredit-mode)

(defun site-start ()
  (interactive)
  (other-window 1)
  (find-file "~/ELPE/lisp/site-start.el")
  (end-of-buffer))

; (later site-start) ;; Uncomment to get out of beginner mode.

