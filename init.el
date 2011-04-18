(add-to-list 'load-path "~/.emacs.d/plugins")

(defun files-in-below-directory (directory)
  "List the .el files in DIRECTORY and in its sub-directories."
  ;; Although the function will be used non-interactively,
  ;; it will be easier to test if we make it interactive.
  ;; The directory will have a name such as
  ;;  "/usr/local/share/emacs/22.1.1/lisp/"
  (interactive "Directory name: ")
  (let (el-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (cond
       ;; check to see whether filename ends in `.el'
       ;; and if so, append its name to a list.
       ((equal ".el" (substring (car (car current-directory-list)) -3))
        (setq el-files-list
              (cons (car (car current-directory-list)) el-files-list)))
       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))
        ;; decide whether to skip or recurse
        (if
            (equal "."
                   (substring (car (car current-directory-list)) -1))
            ;; then do nothing since filename is that of
            ;;   current directory or parent, "." or ".."
            ()
          ;; else descend into the directory and repeat the process
          (setq el-files-list
                (append
                 (files-in-below-directory
                  (car (car current-directory-list)))
                 el-files-list)))))
      ;; move to the next filename in the list; this also
      ;; shortens the list so the while loop eventually comes to an end
      (setq current-directory-list (cdr current-directory-list)))
    ;; return the filenames
    el-files-list))

(mapc 'load (files-in-below-directory "~/.emacs.d/initializers"))

; 
; ;Time-stamp: <spritesun 04/16/2011 11:46:22>
; 
; ;;外部el文件
; ;(add-to-list 'load-path "~/.emacs.d/")
; ;;自己的el文件，我把自己的el文件分为了多个模块
; ;(add-to-list 'load-path "~/.emacs.d/module/")
; 
; ;;所有的module我都加了mo作为文件名扩展，这是很必须的，因为重名的elisp互相引用很可能造成死循环。当然elisp执行的时候并不会真的陷入死循环，而是报告这是一个无效的递归。
; 
; ;;基本设置
; (load "basic-mo")
; ;;启动全屏
; ;(load "window-mo")
; ;;一些奇怪或是神奇的设置，我尚未理解。
; (load "magic-mo")
; ;;一些很多人使用，但我觉得没用，或者不起作用的设置。
; ;(load "unused-mo")
; ;;日历设置
; (load "calendar-mo")
; ;;cedet我没配置好，和php冲突
; ;(load "cedet-mo")
; ;;html,javascript,php,vb script,css，用处似乎不大
; ;(Load "html-script-mo")
; ;;php是一种web应用开发语言,这里使用的是php-mode
; ;(load "php-mo")
; ;;css-mode
; ;;(load "css-mo")
; ;;javascript模式
; ;;(load "js-mo")
; ;; tweak
; (load "tweak-mo")
; ;;python模式
; ;(load "python-mo")
; ;;ruby模式
; ;(load "ruby-mo")
; ;;YAML模式
; ;(load "yaml-mo")
; ;;Rails模式
; ;(load "rails-mo")
; ;;Ri模式, has bug now
; ;(load "ri-mo")
; ;;nxhtml模式
; ;(load "nxml-mo")
; ;;RSpec模式
; ;(load "rspec-mo")
; ;;Smart-compile模式
; ;(load "smart-compile-mo")
; ;;subversion
; ;(load "svn-mo")
; ;;mmm可以支持多个主模式共同作用与一个文件，这里使用的主要是php+html的主模式,感觉支持的不是太好
; ;(load "mmm-mo")
; ;;buffer的分组并可视化显示,这个似乎不太好用，而且有些冲突
; ;(load "tabbar-mo")
; ;;这是我见过的最方便的切换buffer，寻找文件的扩展了。
; (load "ido-mo")
; ;;设置时间显示
; (load "time-mo")
; ;;设置字体
; ;(load "font-mo")
; ;;远程编辑
; ;;(load "tramp-mo")
; ;;备份的设置
; (load "backup-mo")
; ;;版本控制
; (load "version-control-mo")
; ;;让你的emacs配色更漂亮
; (load "color-mo")
; ;; 代码折叠
; ;;(load "hideshow-mo")
; ;;显示行号的一些工具
; (load "nl-mo")
; ;;记录修改改时间
; (load "time-stamp-mo")
; ;;把语法加亮的文件输出成彩色 HTML 文件, has debug currently
; ;(load "htmlize-mo")
; 
; ;;(load "gmail-mo")
; 

