(setq inferior-lisp-program "/opt/local/bin/sbcl")
(add-to-list 'load-path "~/.emacs.d/plugins/slime/")
(require 'slime)
;; (slime-setup)
(slime-setup '(slime-fancy))  ; load almost all slime
;; (require 'slime-autoloads) ; I guess it auoload slime mode to lisp file

(global-set-key (kbd "<f5>") 'slime-eval-buffer)