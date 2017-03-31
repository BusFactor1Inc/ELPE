(setq lexical-binding t)
(require 'cl) ;; We're just like that

(require 'paredit)

(require 'grep-buffers)
(require 'web-server)

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

(defun layout-windows-centered-on-screen-4k ()
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

	 (frame-2-x (+ frame-3-x frame-3-w padding))
	 (frame-2-y frame-3-y)

	 (frame-1-x (+ frame-3-x frame-3-w padding))
	 (frame-1-y (- (+ frame-3-y frame-3-h) frame-1-h)))

    (set-frame-position frame-1 frame-1-x frame-1-y)
    (set-frame-position frame-2 frame-2-x frame-2-y)
    (set-frame-position frame-3 frame-3-x frame-3-y)))

;; center windows on screen
(defun layout-windows-centered-on-screen-air ()
  (let* ((frames (frame-list))
         (frame-1 (car frames))
         (frame-2 (cadr frames))
         (frame-3 (caddr frames)))
    (set-frame-size frame-3 120 52)
    (set-frame-size frame-2 74 26)
    (set-frame-size frame-1 74 20)
    
    (let* ((dimensions (screen-dimensions-macos))
           (width (car dimensions))
           (height (cdr dimensions))
           (padding 8)
           
           (frame-1-w (frame-pixel-width frame-1))
           (frame-1-h (frame-pixel-height frame-1))
           
           (frame-2-w (frame-pixel-width frame-2))
           (frame-2-h (frame-pixel-height frame-2))
           
           (frame-3-w (frame-pixel-width frame-3))
           (frame-3-h (frame-pixel-height frame-3))
           
           (frame-3-x 0)
           (frame-3-y 0)
           
           (frame-2-x (+ frame-3-x frame-3-w padding))
           (frame-2-y frame-3-y)
           
           (frame-1-x (+ frame-3-x frame-3-w padding))
           (frame-1-y (- (+ frame-3-y frame-3-h padding)
                         frame-1-h)))
      (set-frame-position frame-1 frame-1-x frame-1-y)
      (set-frame-position frame-2 frame-2-x frame-2-y)
      (set-frame-position frame-3 frame-3-x frame-3-y))))

;; Run a given function-symbol (zero argument function) at a later time
(defmacro* later (function-symbol &optional (time "1 sec"))
  `(run-at-time ,time nil ,function-symbol))

;;(run-at-time "1 sec" nil 'layout-windows-centered-on-screen)
(defvar layout-windows-function
  (let* ((dimensions (screen-dimensions-macos))
         (width (car dimensions))
         (height (cdr dimensions)))
    (if (= width 3840)
        #'layout-windows-centered-on-screen-4k
      #'layout-windows-centered-on-screen-air)))

(later layout-windows-function)

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

(later 'keybind-toggle-paredit-mode)

(defun site-start ()
  (interactive)
  (other-window 1)
  (find-file "~/ELPE/lisp/site-start.el")
  (end-of-buffer))

; (later Site-start) ;; Uncomment to get out of beginner mode.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Web Server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar web-server-handlers ())
(ws-start
 (lambda (request)
   (with-slots (process headers) request
     (if web-server-handlers
         (dolist (handler web-server-handlers)
           (funcall handler request process headers))
       (progn
         (ws-response-header process 200 '("Content-type" . "text/html"))
         (process-send-string process "Emacs Lisp Programming Environment (ELPE)")))))
 8080)

(defun web-server-respond-ok (connection content-type)
  "Called at start of web-server handler to indicate that the
request is ok with the given content type."
  (ws-response-header connection 200 `("Content-type" . ,content-type)))

(defun web-server-write (connection string)
  "Write 'string' to the web server process 'connection'"
  (process-send-string connection string))

