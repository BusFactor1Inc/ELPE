(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (leuven)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(kill-buffer)
(global-set-key "\M-;" 'other-window)
(global-set-key "\M-'" 'lisp-complete-symbol)

(set-frame-size (selected-frame) 160 78)

(defun screen-dimensions-macos ()
  (car (read-from-string (shell-command-to-string "system_profiler SPDisplaysDataType | grep Resolution | while read a width x height xs; do echo '(' $width '.'  $height ')'; done"))))

(defun layout-window-centered-on-screen ()
  (let* ((dimensions (screen-dimensions-macos))
	 (width (car dimensions))
	 (height (cdr dimensions))

	 (frames (frame-list))
	 (frame-1 (car frames))
	 
	 (frame-1-w (frame-pixel-width frame-1))
	 (frame-1-h (frame-pixel-height frame-1))

	 (frame-1-x (/ (- width frame-1-w ) 2))
	 (frame-1-y (/ (- height frame-1-h ) 2)))

    (set-frame-position frame-1 frame-1-x frame-1-y)))

(layout-window-centered-on-screen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq rcirc-server-alist '(("irc.freenode.net")))
(irc nil)
(rcirc-join-channels (get-process "irc.freenode.net") (list "#ELPE"))
(switch-to-buffer "#ELPE@irc.freenode.net")

(split-window-horizontally)
(split-window-vertically)
(find-file ".emacs")
(other-window 1)
(switch-to-buffer "#ELPE@irc.freenode.net")
(info "rcirc")
(other-window 3)
(switch-to-buffer "*info*")
(other-window 2)
(switch-to-buffer "*irc.freenode.net*")
(split-window-vertically)
(cd "~")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (leuven)))
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(other-window 1)
