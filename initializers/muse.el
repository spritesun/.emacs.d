(add-to-list 'load-path "~/.emacs.d/plugins/muse")
(require 'muse-mode)
(require 'muse-publish)
(require 'muse-html)
(require 'muse-colors)
;; (require 'muse-latex)
;; (require 'muse-texinfo)
;; (require 'muse-docbook)

(require 'muse-project)

(setq muse-project-alist
      `(("Meditations" (,@(muse-project-alist-dirs "~/Documents/muse/pages")
                        :default "index")
         ,@(muse-project-alist-styles "~/Documents/muse/pages"
                                      "~/Documents/muse/public_html"
                                      "html"))))

(add-hook 'muse-mode-hook
 (lambda ()
 (local-set-key (kbd "C-c C-c") 'muse-follow-name-at-point)
 )
)

(setq muse-file-extension nil
           muse-mode-auto-p t)
(add-hook 'find-file-hooks 'muse-mode-maybe)

(setq muse-html-header "~/Documents/muse/templates/header.html")
(setq muse-html-footer "~/Documents/muse/templates/footer.html")
