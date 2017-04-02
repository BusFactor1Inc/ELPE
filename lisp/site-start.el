;; site-start.el -*- mode: emacs lisp -*-

(setq lexical-binding t)
(require 'cl) ;; We're just like that

(require 'grep-buffers)
(require 'web-server)
(require 'winner)
(winner-mode t)

(setq display-time-day-and-date t)
(display-time-mode t)

;; Turn on pareditmode by default
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(package-initialize)

;; Add paredit mode to eshell
(add-hook 'eshell-mode-hook 'enable-paredit-mode)
(add-hook 'minibuffer-setup-hook 'enable-paredit-mode)
(setq eshell-banner-message "Emacs Lisp Programming Environment (ELPE) shell.\n\nYou can evaluate Emacs Lisp code at the prompt\nand run shell commands like ls, cp, mv and cat.\n\n")

(global-set-key "\M-;" 'other-window)
(global-set-key "\C-xo" (lambda () (interactive) (other-frame -1)))
;(global-set-key "\M-'" 'lisp-complete-symbol)

(set-frame-size (selected-frame) 160 78)

;(info "/Applications/Emacs Lisp Programming Environment (ELPE).app/Contents/Resources/info/eintr")

(split-window-horizontally)
(find-file "~/.elpe")
(switch-to-buffer ".elpe")
(when (= 0 (length (buffer-string)))
  (insert ";; .elpe -*- mode: emacs-lisp -*-\n")
  (insert ";;\n")
  (insert ";; This is your initization file that is loaded at startup.\n")
  (insert ";; Save your edits using the toolbar or by using the File menu."))
(switch-to-buffer "*scratch*")
(beginning-of-buffer)
(insert ";; The *scratch* Buffer\n;;")
(newline)
(insert ";; Control-j:") (newline)
(insert ";;     Evaluate when at the end\n;;     of an expression.\n;;")

(newline)
(insert ";; Control-Command-x:\n;;     Evaluate when inside\n;;     an expression.\n;;\n")
(insert ";; To run the tutorial,\n;; type Control-j\n;; at the end of the next line:\n\n")
(insert "(help-with-tutorial)")
(new-frame)
(eshell)
(other-window 1)

(split-window-vertically)

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
    (set-frame-size frame-2 74 27)
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
                         frame-1-h
                         -16)))
      (set-frame-position frame-1 frame-1-x frame-1-y)
      (set-frame-position frame-2 frame-2-x frame-2-y)
      (set-frame-position frame-3 frame-3-x frame-3-y))))

;; Run a given function-symbol (zero argument function) at a later time
(defmacro* later (function-symbol &optional (time "1 sec"))
  `(run-at-time ,time nil ,function-symbol))

;;(run-at-time "1 sec" nil 'layout-windows-centered-on-screen)
(defvar on-macbook-air nil)
(defvar layout-windows-function
  (let* ((dimensions (screen-dimensions-macos))
         (width (car dimensions))
         (height (cdr dimensions)))
    (if (= width 3840)
        #'layout-windows-centered-on-screen-4k
      (progn
        (setf on-macbook-air t)
        #'layout-windows-centered-on-screen-air))))

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

(switch-to-buffer ".elpe")
(other-frame 2)
(other-window 1)
(when on-macbook-air
  (enlarge-window-horizontally 15))

(setf *expert-mode* (y-or-n-p "Expert mode?"))

(let ((site-start "~/ELPE/lisp/site-start.el"))
  (if (and *expert-mode*
           (file-exists-p site-start))
      (find-file site-start)
    (eww-open-file "/Applications/Emacs Lisp Programming Environment (ELPE).app/Contents/Resources/eintr.html")))

(other-window 1)
;(split-window-vertically)
(switch-to-buffer "*scratch*")
(other-window 1)
(enlarge-window -6)

(load-theme 'leuven)

(other-frame 1)
(switch-to-buffer "*eshell*")
(other-frame 1)
(switch-to-buffer ".elpe")
(end-of-buffer)
(calc)
(other-frame 1)
(other-window 1)
(split-window-vertically)
(enlarge-window 12)
(switch-to-buffer "*info*")
(other-frame 2)
(other-window 2)
(other-frame 1)

(switch-to-buffer "*scratch*")
(other-window 1)
(switch-to-buffer "*ELPE Help*")
(insert "Start by reading the manual.\n")
(enlarge-window-horizontally 1)

(defun save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer, then kill this Emacs process.
With prefix ARG, silently save all file-visiting buffers without asking.
If there are active processes where `process-query-on-exit-flag'
returns non-nil and `confirm-kill-processes' is non-nil,
asks whether processes should be killed.
Runs the members of `kill-emacs-query-functions' in turn and stops
if any returns nil.  If `confirm-kill-emacs' is non-nil, calls it."
  (interactive "P")
  ;; Don't use save-some-buffers-default-predicate, because we want
  ;; to ask about all the buffers before killing Emacs.
  (save-some-buffers arg t)
  (let ((confirm confirm-kill-emacs))
    (and
     (or (not (memq t (mapcar (function
                               (lambda (buf) (and (buffer-file-name buf)
                                                  (buffer-modified-p buf))))
                              (buffer-list))))
         (progn (setq confirm nil)
                (yes-or-no-p "Modified buffers exist; exit anyway? ")))
     (or (not (fboundp 'process-list))
         ;; process-list is not defined on MSDOS.
         (not confirm-kill-processes)
         (let ((processes (process-list))
               active)
           (while processes
             (and (memq (process-status (car processes)) '(run stop open listen))
                  (process-query-on-exit-flag (car processes))
                  (setq active t))
             (setq processes (cdr processes)))
           (or (not active)
               (with-current-buffer-window
                (get-buffer-create "*Process List*") nil
                #'(lambda (window _value)
                    (with-selected-window window
                      (unwind-protect
                          (progn
                            (setq confirm nil)
                            ;(yes-or-no-p "Active processes exist; kill them and exit anyway? ")
                            t)
                        (when (window-live-p window)
                          (quit-restore-window window 'kill)))))
                (list-processes t)))))
     ;; Query the user for other things, perhaps.
     (run-hook-with-args-until-failure 'kill-emacs-query-functions)
     (or (null confirm)
         (funcall confirm "Really exit The Emacs Lisp Programming Environment? "))
     (kill-emacs))))

(global-hl-line-mode)
