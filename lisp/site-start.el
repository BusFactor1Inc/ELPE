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

(setq rcirc-server-alist '(("irc.freenode.net" :channels ("#elpe"))))
;(irc nil)

(switch-to-buffer "*info*")
(set-frame-size (selected-frame) 80 37)
