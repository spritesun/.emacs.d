;;关闭烦人的出错时的提示声
(setq visible-bell t)

;;关闭起动时的那个“开机画面”
(setq inhibit-startup-message t)

;;显示列号
(setq column-number-mode t)

;;用一个很大的 kill ring. 这样防止我不小心删掉重要的东西
(setq kill-ring-max 200)

;;把 fill-column 设为 60. 这样的文字更好读。
(setq default-fill-column 60)

;;不用 TAB 字符来indent, 这会引起很多奇怪的错误。编辑 Makefile 的时候也不用担心，因为 makefile-mode 会把 TAB 键设置成真正的 TAB 字符，并且加亮显示的。
(setq-default indent-tabs-mode nil)
(setq default-tab-width 8)
(setq tab-stop-list ())

;;设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插 入两个空格。
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;;可以递归的使用 minibuffer。我经常需要这么做。
(setq enable-recursive-minibuffers t)

;;防止页面滚动时跳动， scroll-margin 3 可以在靠近屏幕边沿3行时就开始滚动，可以很好的看到上下文。
(setq scroll-margin 3
      scroll-conservatively 10000)

;;把缺省的 major mode 设置为 text-mode, 而不是几乎什么功能也 没有的 fundamental-mode.
(setq default-major-mode 'text-mode)

;;括号匹配时显示另外一边的括号，而不是烦人的跳到另一个括号。
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;;光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。
(mouse-avoidance-mode 'animate)

;;在标题栏显示buffer的名字
(setq frame-title-format "emacs@%b")

;;让 Emacs 可以直接打开和显示图片。
(auto-image-file-mode)

;;进行语法加亮。
(global-font-lock-mode t)

;;把这些缺省禁用的功能打开。
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)

;;设置一下备份时的版本控制，这样更加安全
(setq version-control t)
(setq kept-new-versions 3)
(setq delete-old-versions t)
(setq kept-old-versions 2)
(setq dired-kept-versions 1)

;;一个简单的办法设置 auto-mode-alist, 免得写很多 add-to-list.
(mapcar
 (function (lambda (setting)
	     (setq auto-mode-alist
		   (cons setting auto-mode-alist))))
 '(("\\.xml$".  sgml-mode)
   ("\\\.bash" . sh-mode)
   ("\\.rdf$".  sgml-mode)
   ("\\.session" . emacs-lisp-mode)
   ("\\.l$" . c-mode)
   ("\\.css$" . css-mode)
   ("\\.cfm$" . html-mode)
   ("gnus" . emacs-lisp-mode)
   ("\\.idl$" . idl-mode)))

;;设置有用的个人信息。这在很多地方有用。
(setq user-full-name "Long Sun")
(setq user-mail-address "spritesun@gmail.com")

;;让 dired 可以递归的拷贝和删除目录。
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

(defalias 'yes-or-no-p 'y-or-n-p)