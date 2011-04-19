(add-to-list 'load-path "~/.emacs.d/plugins/cedet/common")
(require 'cedet)
(require 'semantic-ia)
 
;; Enable EDE (Project Management) features
(global-ede-mode 1)
 
(semantic-load-enable-excessive-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)
 
;; Enable SRecode (Template management) minor-mode.
(global-srecode-minor-mode 1)

;;should load after cedet
(add-to-list 'load-path "~/.emacs.d/plugins/ecb-2.40")
;;If you want to load the complete ECB at (X)Emacs-loadtime (Advantage: All ECB-options available after loading ECB. Disadvantage: Increasing loadtime2):
;;(require 'ecb)
;;If you want to load the ECB first after starting it by ecb-activate (Advantage: Fast loading3. Disadvantage: ECB- and semantic-options first available after starting ECB):
(require 'ecb-autoloads)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
