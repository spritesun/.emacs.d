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
      '(("Meditations" ("~/Documents/muse/pages" :default "index")
         (:base "html" :path "~/Documents/muse/public_html")
         (:base "pdf" :path "~/Documents/muse/pdf"))))