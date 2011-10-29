;;; recentf.el --- setup a menu of recently opened files

;; Copyright (C) 1999, 2000, 2001, 2002, 2003
;;   Free Software Foundation, Inc.

;; Author: David Ponce <david@dponce.com>
;; Created: July 19 1999
;; Maintainer: FSF
;; Keywords: files

(defconst recentf-version "$Revision: 1.26 $")

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package maintains a menu for visiting files that were operated
;; on recently.  When enabled a new "Open Recent" submenu is displayed
;; in the "Files" menu.  The recent files list is automatically saved
;; across Emacs sessions.  You can customize the number of recent
;; files displayed, the location of the menu and others options (see
;; the source code for details).
;;
;; To install and use, put this file on your Emacs-Lisp load path and
;; add the following into your ~/.emacs startup file:
;;
;;  (require 'recentf)
;;  (recentf-mode 1)

;;; History:
;;

;;; Code:
(require 'easymenu)
(require 'wid-edit)
(require 'timer)
(eval-when-compile (require 'cl))

;;; Compatibility
;;
(if (fboundp 'overlay-lists)
    (defalias 'recentf-overlay-lists
      'overlay-lists)
  (defalias 'recentf-overlay-lists
    '(lambda () (list (extent-list)))))

(if (fboundp 'delete-overlay)
    (defalias 'recentf-delete-overlay
      'delete-overlay)
  (defalias 'recentf-delete-overlay
    'delete-extent))

;;; Internal data
;;
(defvar recentf-list nil
  "List of recently opened files.")

(defvar recentf-data-cache nil
  "Cache of data used to build the recentf menu.
The menu is rebuilt when this data has changed.")

;;; Customization
;;
(defgroup recentf nil
  "Maintain a menu of recently opened files."
  :version "21.1"
  :group 'files)

(defgroup recentf-filters nil
  "Group to customize recentf menu filters.
You should define the options of your own filters in this group."
  :group 'recentf)

(defcustom recentf-max-saved-items 20
  "*Maximum number of items of the recent list that will be saved.
nil means to save the whole list.
See the command `recentf-save-list'."
  :group 'recentf
  :type 'integer)

(defcustom recentf-save-file "~/.recentf"
  "*File to save the recent list into."
  :group 'recentf
  :type 'file)

(defcustom recentf-exclude nil
  "*List of regexps for filenames excluded from the recent list."
  :group 'recentf
  :type '(repeat regexp))

(defun recentf-menu-customization-changed (variable value)
  "Function called when the recentf menu customization has changed.
Set VARIABLE with VALUE, and force a rebuild of the recentf menu."
  (when (featurep 'recentf)
    ;; Unavailable until recentf has been loaded.
    (recentf-clear-data))
  (set-default variable value))

(defcustom recentf-menu-title "Open Recent"
  "*Name of the recentf menu."
  :group 'recentf
  :type 'string
  :set 'recentf-menu-customization-changed)

(defcustom recentf-menu-path (if (featurep 'xemacs)
                                 '("File")
                               '("files"))
  "*Path where to add the recentf menu.
If nil add it at top level (see also `easy-menu-change')."
  :group 'recentf
  :type '(choice (const :tag "Top Level" nil)
                 (sexp :tag "Menu Path"))
  :set 'recentf-menu-customization-changed)

(defcustom recentf-menu-before (if (featurep 'xemacs)
                                   "Open..."
                                 "open-file")
  "*Name of the menu before which the recentf menu will be added.
If nil add it at end of menu (see also `easy-menu-change')."
  :group 'recentf
  :type '(choice (string :tag "Name")
                 (const :tag "Last" nil))
  :set 'recentf-menu-customization-changed)

(defcustom recentf-menu-action 'recentf-find-file
  "*Function to invoke with a filename item of the recentf menu.
The default is to call `recentf-find-file' to edit the selected file."
  :group 'recentf
  :type 'function
  :set 'recentf-menu-customization-changed)

(defcustom recentf-max-menu-items 10
  "*Maximum number of items in the recentf menu."
  :group 'recentf
  :type 'integer
  :set 'recentf-menu-customization-changed)

(defcustom recentf-menu-filter nil
  "*Function used to filter files displayed in the recentf menu.
nil means no filter.  The following functions are predefined:

- `recentf-sort-ascending'
    Sort menu items in ascending order.
- `recentf-sort-descending'
    Sort menu items in descending order.
- `recentf-sort-basenames-ascending'
    Sort menu items by filenames sans directory in ascending order.
- `recentf-sort-basenames-descending'
    Sort menu items by filenames sans directory in descending order.
- `recentf-sort-directories-ascending'
    Sort menu items by directories in ascending order.
- `recentf-sort-directories-descending'
    Sort menu items by directories in descending order.
- `recentf-show-basenames'
    Show filenames sans directory in menu items.
- `recentf-show-basenames-ascending'
    Show filenames sans directory in ascending order.
- `recentf-show-basenames-descending'
    Show filenames sans directory in descending order.
- `recentf-relative-filter'
    Show filenames relative to `default-directory'.
- `recentf-arrange-by-rule'
    Show sub-menus following user defined rules.
- `recentf-arrange-by-mode'
    Show a sub-menu for each major mode.
- `recentf-arrange-by-dir'
    Show a sub-menu for each directory.
- `recentf-filter-changer'
    Manage a ring of filters.

The filter function is called with one argument, the list of menu
elements used to build the menu and must return a new list of menu
elements (see `recentf-make-menu-element' for menu element form)."
  :group 'recentf
  :type '(radio (const nil)
                (function-item recentf-sort-ascending)
                (function-item recentf-sort-descending)
                (function-item recentf-sort-basenames-ascending)
                (function-item recentf-sort-basenames-descending)
                (function-item recentf-sort-directories-ascending)
                (function-item recentf-sort-directories-descending)
                (function-item recentf-show-basenames)
                (function-item recentf-show-basenames-ascending)
                (function-item recentf-show-basenames-descending)
                (function-item recentf-relative-filter)
                (function-item recentf-arrange-by-rule)
                (function-item recentf-arrange-by-mode)
                (function-item recentf-arrange-by-dir)
                (function-item recentf-filter-changer)
                function)
  :set 'recentf-menu-customization-changed)

(defcustom recentf-menu-append-commands-flag t
  "*non-nil means to append command items to the menu."
  :group 'recentf
  :type 'boolean
  :set 'recentf-menu-customization-changed)
(make-obsolete-variable 'recentf-menu-append-commands-p
                        'recentf-menu-append-commands-flag)

(defcustom recentf-keep-non-readable-files-flag nil
  "*non-nil means to keep non readable files in the recent list."
  :group 'recentf
  :type 'boolean)
(make-obsolete-variable 'recentf-keep-non-readable-files-p
                        'recentf-keep-non-readable-files-flag)

(defcustom recentf-auto-cleanup 'mode
  "*Define when to automatically cleanup the recent list.
The following values can be set:

- `mode'
    Cleanup when turning the mode on (default).
- `never'
    Never cleanup the list automatically.
- A number
    Cleanup each time Emacs has been idle that number of seconds.
- A time string
    Cleanup at specified time string, for example at \"11:00pm\".

Setting this variable directly does not take effect;
use \\[customize].

See also the command `recentf-cleanup', that can be used to manually
cleanup the list."
  :group 'recentf
  :type '(radio (const  :tag "When mode enabled"
                        :value mode)
                (const  :tag "Never"
                        :value never)
                (number :tag "When idle that seconds"
                        :value 300)
                (string :tag "At time"
                        :value "11:00pm"))
  :set (lambda (variable value)
         (set-default variable value)
         (when (featurep 'recentf)
           ;; Unavailable until recentf has been loaded.
           (recentf-auto-cleanup))))

(defcustom recentf-load-hook nil
   "*Normal hook run at end of loading the `recentf' package."
  :group 'recentf
  :type 'hook)

(defcustom recentf-filename-handler nil
  "Function to call to process filename handled by recentf.
It is passed a filename to give a chance to transform it.
If it returns nil, the filename is left unchanged."
  :group 'recentf
  :type 'function)

;;;###autoload
(defcustom recentf-mode nil
  "Toggle recentf mode.
When recentf mode is enabled, it maintains a menu for visiting files
that were operated on recently.
Setting this variable directly does not take effect;
use either \\[customize] or the function `recentf-mode'."
  :set (lambda (variable value)
         (when (featurep 'recentf)
           (funcall variable (or value 0))))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'recentf
  :require 'recentf)

(defcustom recentf-mode-hook nil
  "Hook run at the end of function `recentf-mode'."
  :group 'recentf
  :type 'hook)

;;; Utilities
;;
(defconst recentf-case-fold-search
  (memq system-type '(vax-vms windows-nt cygwin))
  "Non-nil if recentf searches and matches should ignore case.")

(defsubst recentf-string-equal (s1 s2)
  "Return non-nil if strings S1 and S2 have identical contents.
Ignore case if `recentf-case-fold-search' is non-nil."
  (if recentf-case-fold-search
      (string-equal (downcase s1) (downcase s2))
    (string-equal s1 s2)))

(defsubst recentf-string-lessp (s1 s2)
  "Return non-nil if string S1 is less than S2 in lexicographic order.
Ignore case if `recentf-case-fold-search' is non-nil."
  (if recentf-case-fold-search
      (string-lessp (downcase s1) (downcase s2))
    (string-lessp s1 s2)))

(defun recentf-string-member (elt list)
  "Return non-nil if ELT is an element of LIST.
The value is actually the tail of LIST whose car is ELT.
ELT must be a string and LIST a list of strings.
Ignore case if `recentf-case-fold-search' is non-nil."
  (while (and list (not (recentf-string-equal elt (car list))))
    (setq list (cdr list)))
  list)

(defsubst recentf-trunc-list (l n)
  "Return from L the list of its first N elements."
  (let (nl)
    (while (and l (> n 0))
      (setq nl (cons (car l) nl)
            n  (1- n)
            l  (cdr l)))
    (nreverse nl)))

(defun recentf-dump-variable (variable &optional limit)
  "Insert a \"(setq VARIABLE value)\" in the current buffer.
When the value of VARIABLE is a list, optional argument LIMIT
specifies a maximum number of elements to insert.  By default insert
the full list."
  (let ((value (symbol-value variable)))
    (if (atom value)
        (insert (format "\n(setq %S %S)\n" variable value))
      (when (and (integerp limit) (> limit 0))
        (setq value (recentf-trunc-list value limit)))
      (insert (format "\n(setq %S\n      '(" variable))
      (dolist (e value)
        (insert (format "\n        %S" e)))
      (insert "\n        ))\n"))))

(defvar recentf-auto-cleanup-timer nil
  "Timer used to automatically cleanup the recent list.
See also the option `recentf-auto-cleanup'.")

(defun recentf-auto-cleanup ()
  "Automatic cleanup of the recent list."
  (when (timerp recentf-auto-cleanup-timer)
    (cancel-timer recentf-auto-cleanup-timer))
  (when recentf-mode
    (setq recentf-auto-cleanup-timer
          (cond
           ((eq 'mode recentf-auto-cleanup)
            (recentf-cleanup)
            nil)
           ((numberp recentf-auto-cleanup)
            (run-with-idle-timer
             recentf-auto-cleanup t 'recentf-cleanup))
           ((stringp recentf-auto-cleanup)
            (run-at-time
             recentf-auto-cleanup nil 'recentf-cleanup))))))

;;; File functions
;;
(defsubst recentf-push (filename)
  "Push FILENAME into the recent list, if it isn't there yet.
If it is there yet, move it at the beginning of the list.
If `recentf-case-fold-search' is non-nil, ignore case when comparing
filenames."
  (let ((m (recentf-string-member filename recentf-list)))
    (and m (setq recentf-list (delq (car m) recentf-list)))
    (push filename recentf-list)))

(defsubst recentf-expand-file-name (name)
  "Convert filename NAME to absolute, and canonicalize it.
See also the function `expand-file-name'.
If defined, call the function `recentf-filename-handler' to post
process the canonical name."
  (let* ((directory-sep-char ?/) ;; Native MS-Windows XEmacs needs that
         (filename (expand-file-name name)))
    (or (and recentf-filename-handler
             (funcall recentf-filename-handler filename))
        filename)))

(defun recentf-include-p (filename)
  "Return t if FILENAME match none of the `recentf-exclude' regexps."
  (let ((case-fold-search recentf-case-fold-search)
        (rl recentf-exclude))
    (while (and rl (not (string-match (car rl) filename)))
      (setq rl (cdr rl)))
    (null rl)))

(defsubst recentf-add-file (filename)
  "Add or move FILENAME at the beginning of the recent list.
Does nothing it if it matches any of the `recentf-exclude' regexps."
  (setq filename (recentf-expand-file-name filename))
  (when (recentf-include-p filename)
    (recentf-push filename)))

(defsubst recentf-remove-if-non-readable (filename)
  "Remove FILENAME from the recent list, if file is not readable.
Return non-nil if FILENAME has been removed."
  (unless (file-readable-p filename)
    (let ((m (recentf-string-member
              (recentf-expand-file-name filename) recentf-list)))
      (and m (setq recentf-list (delq (car m) recentf-list))))))

(defun recentf-find-file (filename)
  "Edit file FILENAME using `find-file'.
If the file does not exist or is non readable, and
`recentf-keep-non-readable-files-flag' is nil, it is not edited and
its name is removed from the recent list."
  (if (and (not recentf-keep-non-readable-files-flag)
           (recentf-remove-if-non-readable filename))
      (message "File `%s' not found" filename)
    (find-file filename)))

(defsubst recentf-directory-compare (f1 f2)
  "Compare absolute filenames F1 and F2.
First compare directories, then filenames sans directory.
Return non-nil if F1 is less than F2."
  (let ((d1 (file-name-directory f1))
        (d2 (file-name-directory f2)))
    (if (recentf-string-equal d1 d2)
        (recentf-string-lessp (file-name-nondirectory f1)
                              (file-name-nondirectory f2))
      (recentf-string-lessp d1 d2))))

;;; Menu building
;;
(defvar recentf-menu-items-for-commands
  (list ["Cleanup list"
         recentf-cleanup
;;;         :help "Remove all non-readable and excluded files from the recent list"
         :active t]
        ["Edit list..."
         recentf-edit-list
;;;         :help "Edit the files that are kept in the recent list"
         :active t]
        ["Save list now"
         recentf-save-list
;;;         :help "Save the list of recently opened files now"
         :active t]
        ["Options..."
         (customize-group "recentf")
;;;         :help "Customize recently opened files menu and options"
         :active t]
        )
  "List of menu items for recentf commands.")

(defvar recentf-menu-filter-commands nil
  "This variable can be used by menu filters to setup their own command menu.
If non-nil it must contain a list of valid menu-items to be appended
to the recent file list part of the menu.  Before calling a menu
filter function this variable is reset to nil.")

(defsubst recentf-elements (n)
  "Return a list of the first N elements of the recent list."
  (recentf-trunc-list recentf-list n))

(defsubst recentf-make-menu-element (menu-item menu-value)
  "Create a new menu-element.
A menu element is a pair (MENU-ITEM . MENU-VALUE), where MENU-ITEM is
the menu item string displayed.  MENU-VALUE is the file to be open
when the corresponding MENU-ITEM is selected.  Or it is a
pair (SUB-MENU-TITLE . MENU-ELEMENTS) where SUB-MENU-TITLE is a
sub-menu title and MENU-ELEMENTS is the list of menu elements in the
sub-menu."
  (cons menu-item menu-value))

(defsubst recentf-menu-element-item (e)
  "Return the item part of the menu-element E."
  (car e))

(defsubst recentf-menu-element-value (e)
  "Return the value part of the menu-element E."
  (cdr e))

(defsubst recentf-set-menu-element-item (e item)
  "Change the item part of menu-element E to ITEM."
  (setcar e item))

(defsubst recentf-set-menu-element-value (e value)
  "Change the value part of menu-element E to VALUE."
  (setcdr e value))

(defsubst recentf-sub-menu-element-p (e)
  "Return non-nil if menu-element E defines a sub-menu."
  (consp (recentf-menu-element-value e)))

(defsubst recentf-make-default-menu-element (file)
  "Make a new default menu element with FILE.
This a menu element (FILE . FILE)."
  (recentf-make-menu-element file file))

(defsubst recentf-menu-elements (n)
  "Return a list of the first N default menu elements from the recent list.
See also `recentf-make-default-menu-element'."
  (mapcar 'recentf-make-default-menu-element
          (recentf-elements n)))

(defun recentf-apply-menu-filter (filter l)
  "Apply function FILTER to the list of menu-elements L.
It takes care of sub-menu elements in L and recursively apply FILTER
to them.  It is guaranteed that FILTER receives only a list of single
menu-elements (no sub-menu)."
  (if (and l (functionp filter))
      (let ((case-fold-search recentf-case-fold-search)
            (directory-sep-char ?/)
            elts others)
        ;; split L into two sub-listes, one of sub-menus elements and
        ;; another of single menu elements.
        (dolist (elt l)
          (if (recentf-sub-menu-element-p elt)
              (push elt elts)
            (push elt others)))
        ;; Apply FILTER to single elements.
        (when others
          (setq others (funcall filter (nreverse others))))
        ;; Apply FILTER to sub-menu elements.
        (setq l nil)
        (dolist (elt elts)
          (recentf-set-menu-element-value
           elt (recentf-apply-menu-filter
                filter (recentf-menu-element-value elt)))
          (push elt l))
        ;; Return the new filtered menu element list.
        (nconc l others))
    l))

(defun recentf-make-menu-items ()
  "Make menu items from the recent list."
  (setq recentf-menu-filter-commands nil)
  (let ((file-items
         (mapcar 'recentf-make-menu-item
                 (recentf-apply-menu-filter
                  recentf-menu-filter
                  (recentf-menu-elements recentf-max-menu-items)))))
    (append (or file-items (list ["No files" t
;;;                                  :help "No recent file to open"
                                  :active nil]))
            (and (< recentf-max-menu-items (length recentf-list))
                 (list ["More..." recentf-open-more-files
;;;                        :help "Open files that are not in the menu"
                        :active t]))
            (and recentf-menu-filter-commands
                 (cons "---"
                       recentf-menu-filter-commands))
            (and recentf-menu-append-commands-flag
                 (cons "---"
                       recentf-menu-items-for-commands)))))

(defsubst recentf-make-menu-item (elt)
  "Make a menu item from menu element ELT."
  (let ((item  (recentf-menu-element-item  elt))
        (value (recentf-menu-element-value elt)))
    (if (recentf-sub-menu-element-p elt)
        (cons item (mapcar 'recentf-make-menu-item value))
      (vector item (list recentf-menu-action value)
;;;              :help (concat "Open " value)
              :active t))))

(defun recentf-clear-data ()
  "Clear data used to build the recentf menu.
This force a rebuild of the menu."
  (easy-menu-remove-item nil recentf-menu-path recentf-menu-title)
  (setq recentf-data-cache nil))

;;; Predefined menu filters
;;
(defsubst recentf-sort-ascending (l)
  "Sort the list of menu elements L in ascending order.
The MENU-ITEM part of each menu element is compared."
  (sort (copy-sequence l)
        #'(lambda (e1 e2)
            (recentf-string-lessp
             (recentf-menu-element-item e1)
             (recentf-menu-element-item e2)))))

(defsubst recentf-sort-descending (l)
  "Sort the list of menu elements L in descending order.
The MENU-ITEM part of each menu element is compared."
  (sort (copy-sequence l)
        #'(lambda (e1 e2)
            (recentf-string-lessp
             (recentf-menu-element-item e2)
             (recentf-menu-element-item e1)))))

(defsubst recentf-sort-basenames-ascending (l)
  "Sort the list of menu elements L in ascending order.
Only filenames sans directory are compared."
  (sort (copy-sequence l)
        #'(lambda (e1 e2)
            (recentf-string-lessp
             (file-name-nondirectory (recentf-menu-element-value e1))
             (file-name-nondirectory (recentf-menu-element-value e2))))))

(defsubst recentf-sort-basenames-descending (l)
  "Sort the list of menu elements L in descending order.
Only filenames sans directory are compared."
  (sort (copy-sequence l)
        #'(lambda (e1 e2)
            (recentf-string-lessp
             (file-name-nondirectory (recentf-menu-element-value e2))
             (file-name-nondirectory (recentf-menu-element-value e1))))))

(defsubst recentf-sort-directories-ascending (l)
  "Sort the list of menu elements L in ascending order.
Compares directories then filenames to order the list."
  (sort (copy-sequence l)
        #'(lambda (e1 e2)
            (recentf-directory-compare
             (recentf-menu-element-value e1)
             (recentf-menu-element-value e2)))))

(defsubst recentf-sort-directories-descending (l)
  "Sort the list of menu elements L in descending order.
Compares directories then filenames to order the list."
  (sort (copy-sequence l)
        #'(lambda (e1 e2)
            (recentf-directory-compare
             (recentf-menu-element-value e2)
             (recentf-menu-element-value e1)))))

(defun recentf-show-basenames (l &optional no-dir)
  "Filter the list of menu elements L to show filenames sans directory.
When a filename is duplicated, it is appended a sequence number if
optional argument NO-DIR is non-nil, or its directory otherwise."
  (let (filtered-names filtered-list full name counters sufx)
    (dolist (elt l (nreverse filtered-list))
      (setq full (recentf-menu-element-value elt)
            name (file-name-nondirectory full))
      (if (not (member name filtered-names))
          (push name filtered-names)
        (if no-dir
            (if (setq sufx (assoc name counters))
                (setcdr sufx (1+ (cdr sufx)))
              (setq sufx 1)
              (push (cons name sufx) counters))
          (setq sufx (file-name-directory full)))
        (setq name (format "%s(%s)" name sufx)))
      (push (recentf-make-menu-element name full) filtered-list))))

(defsubst recentf-show-basenames-ascending (l)
  "Filter the list of menu elements L to show filenames sans directory.
Filenames are sorted in ascending order.
This filter combines the `recentf-sort-basenames-ascending' and
`recentf-show-basenames' filters."
  (recentf-show-basenames (recentf-sort-basenames-ascending l)))

(defsubst recentf-show-basenames-descending (l)
  "Filter the list of menu elements L to show filenames sans directory.
Filenames are sorted in descending order.
This filter combines the `recentf-sort-basenames-descending' and
`recentf-show-basenames' filters."
  (recentf-show-basenames (recentf-sort-basenames-descending l)))

(defun recentf-relative-filter (l)
  "Filter the list of menu-elements L to show relative filenames.
Filenames are relative to the `default-directory'."
  (mapcar #'(lambda (menu-element)
              (let* ((ful (recentf-menu-element-value menu-element))
                     (rel (file-relative-name ful default-directory)))
                (if (string-match "^\\.\\." rel)
                    menu-element
                  (recentf-make-menu-element rel ful))))
          l))

;;; Rule based menu filters
;;
(defcustom recentf-arrange-rules
  '(
    ("Elisp files (%d)" ".\\.el$")
    ("Java files (%d)"  ".\\.java$")
    ("C/C++ files (%d)" "c\\(pp\\)?$")
    )
  "*List of rules used by `recentf-arrange-by-rule' to build sub-menus.
A rule is a pair (SUB-MENU-TITLE . MATCHER).  SUB-MENU-TITLE is the
displayed title of the sub-menu where a '%d' `format' pattern is
replaced by the number of items in the sub-menu.  MATCHER is a regexp
or a list of regexps.  Items matching one of the regular expressions in
MATCHER are added to the corresponding sub-menu."
  :group 'recentf-filters
  :type '(repeat (cons string (repeat regexp)))
  :set 'recentf-menu-customization-changed)

(defcustom recentf-arrange-by-rule-others "Other files (%d)"
  "*Title of the `recentf-arrange-by-rule' sub-menu.
This is for the menu where items that don't match any
`recentf-arrange-rules' are displayed.  If nil these items are
displayed in the main recent files menu.  A '%d' `format' pattern in
the title is replaced by the number of items in the sub-menu."
  :group 'recentf-filters
  :type '(choice (const  :tag "Main menu" nil)
                 (string :tag "Title"))
  :set 'recentf-menu-customization-changed)

(defcustom recentf-arrange-by-rules-min-items 0
  "*Minimum number of items in a `recentf-arrange-by-rule' sub-menu.
If the number of items in a sub-menu is less than this value the
corresponding sub-menu items are displayed in the main recent files
menu or in the `recentf-arrange-by-rule-others' sub-menu if
defined."
  :group 'recentf-filters
  :type 'number
  :set 'recentf-menu-customization-changed)

(defcustom recentf-arrange-by-rule-subfilter nil
  "*Function called by a rule based filter to filter sub-menu elements.
nil means no filter.  See also `recentf-menu-filter'.
You can't use another rule based filter here."
  :group 'recentf-filters
  :type '(choice (const nil) function)
  :set (lambda (variable value)
         (when (memq value '(recentf-arrange-by-rule
                             recentf-arrange-by-mode
                             recentf-arrange-by-dir))
           (error "Recursive use of a rule based filter"))
         (recentf-menu-customization-changed variable value)))

(defun recentf-match-rule-p (matcher filename)
  "Return non-nil if the rule specified by MATCHER match FILENAME.
See `recentf-arrange-rules' for details on MATCHER."
  (if (stringp matcher)
      (string-match matcher filename)
    (while (and (consp matcher)
                (not (string-match (car matcher) filename)))
      (setq matcher (cdr matcher)))
    matcher))

(defun recentf-arrange-by-rule (l)
  "Filter the list of menu-elements L.
Arrange them in sub-menus following rules in `recentf-arrange-rules'."
  (if (not recentf-arrange-rules)
      l
    (let ((menus (mapcar #'(lambda (r) (list (car r)))
                         recentf-arrange-rules))
          menu others min file rules elts count)
      (dolist (elt l)
        (setq file  (recentf-menu-element-value elt)
              rules recentf-arrange-rules
              elts  menus
              menu  nil)
        (while (and (not menu) rules)
          (when (recentf-match-rule-p (cdar rules) file)
            (setq menu (car elts))
            (recentf-set-menu-element-value
             menu (cons elt (recentf-menu-element-value menu))))
          (setq rules (cdr rules)
                elts  (cdr elts)))
        (unless menu
          (push elt others)))
      
      (setq l nil
            min (if (natnump recentf-arrange-by-rules-min-items)
                    recentf-arrange-by-rules-min-items 0))
      (dolist (menu menus)
        (when (setq elts (recentf-menu-element-value menu))
          (setq count (length elts))
          (if (< count min)
              (setq others (nconc elts others))
            (recentf-set-menu-element-item
             menu (format (recentf-menu-element-item menu) count))
            (recentf-set-menu-element-value
             menu (recentf-apply-menu-filter
                   recentf-arrange-by-rule-subfilter (nreverse elts)))
            (push menu l))))
      
      (if (and (stringp recentf-arrange-by-rule-others) others)
          (nreverse
           (cons
            (recentf-make-menu-element
             (format recentf-arrange-by-rule-others (length others))
             (recentf-apply-menu-filter
              recentf-arrange-by-rule-subfilter (nreverse others)))
            l))
        (nconc
         (nreverse l)
         (recentf-apply-menu-filter
          recentf-arrange-by-rule-subfilter (nreverse others)))))
    ))

;;; Predefined rule based menu filters
;;
(defun recentf-build-mode-rules ()
  "Convert `auto-mode-alist' to menu filter rules.
Rules obey `recentf-arrange-rules' format."
  (let ((case-fold-search recentf-case-fold-search)
        regexp rule-name rule rules)
    (dolist (mode auto-mode-alist)
      (setq regexp (car mode)
            mode   (cdr mode))
      (when (symbolp mode)
        (setq rule-name (symbol-name mode))
        (if (string-match "\\(.*\\)-mode$" rule-name)
            (setq rule-name (match-string 1 rule-name)))
        (setq rule-name (concat rule-name " (%d)")
              rule (assoc rule-name rules))
        (if rule
            (setcdr rule (cons regexp (cdr rule)))
          (push (list rule-name regexp) rules))))
    ;; It is important to preserve auto-mode-alist order
    ;; to ensure the right file <-> mode association
    (nreverse rules)))

(defun recentf-arrange-by-mode (l)
  "Split the list of menu-elements L into sub-menus by major mode."
  (let ((recentf-arrange-rules (recentf-build-mode-rules))
        (recentf-arrange-by-rule-others "others (%d)"))
    (recentf-arrange-by-rule l)))

(defun recentf-build-dir-rules (l)
  "Convert directories in menu-elements L to menu filter rules.
Rules obey `recentf-arrange-rules' format."
  (let (dirs)
    (mapcar #'(lambda (e)
                (let ((dir (file-name-directory
                            (recentf-menu-element-value e))))
                  (or (recentf-string-member dir dirs)
                      (push dir dirs))))
            l)
    (mapcar #'(lambda (d)
                (cons (concat d " (%d)")
                      (concat "\\`" d)))
            (nreverse (sort dirs 'recentf-string-lessp)))))

(defun recentf-file-name-nondir (l)
  "Filter the list of menu-elements L to show filenames sans directory.
This simplified version of `recentf-show-basenames' does not handle
duplicates.  It is used by `recentf-arrange-by-dir' as its
`recentf-arrange-by-rule-subfilter'."
  (mapcar #'(lambda (e)
              (recentf-make-menu-element
               (file-name-nondirectory (recentf-menu-element-value e))
               (recentf-menu-element-value e)))
          l))

(defun recentf-arrange-by-dir (l)
  "Split the list of menu-elements L into sub-menus by directory."
  (let ((recentf-arrange-rules (recentf-build-dir-rules l))
        (recentf-arrange-by-rule-subfilter 'recentf-file-name-nondir)
        recentf-arrange-by-rule-others)
    (nreverse (recentf-arrange-by-rule l))))

;;; Ring of menu filters
;;
(defvar recentf-filter-changer-state nil
  "Used by `recentf-filter-changer' to hold its state.")

(defcustom recentf-filter-changer-alist
  '(
    (recentf-arrange-by-mode . "*Files by Mode*")
    (recentf-arrange-by-dir  . "*Files by Directory*")
    (recentf-arrange-by-rule . "*Files by User Rule*")
    )
  "*List of filters managed by `recentf-filter-changer'.
Each filter is defined by a pair (FUNCTION . LABEL), where FUNCTION is
the filter function, and LABEL is the menu item displayed to select
that filter."
  :group 'recentf-filters
  :type '(repeat (cons function string))
  :set (lambda (variable value)
         (setq recentf-filter-changer-state nil)
         (recentf-menu-customization-changed variable value)))

(defun recentf-filter-changer-goto-next ()
  "Go to the next filter available.
See `recentf-filter-changer'."
  (setq recentf-filter-changer-state (cdr recentf-filter-changer-state))
  (recentf-clear-data))

(defsubst recentf-filter-changer-get-current ()
  "Get the current filter available.
See `recentf-filter-changer'."
  (unless recentf-filter-changer-state
    (setq recentf-filter-changer-state recentf-filter-changer-alist))
  (car recentf-filter-changer-state))

(defsubst recentf-filter-changer-get-next ()
  "Get the next filter available.
See `recentf-filter-changer'."
  ;; At this point the current filter is the first element of
  ;; `recentf-filter-changer-state'.
  (car (or (cdr recentf-filter-changer-state)
           ;; There is no next element in
           ;; `recentf-filter-changer-state', so loop back to the
           ;; first element of `recentf-filter-changer-alist'.
           recentf-filter-changer-alist)))

(defun recentf-filter-changer (l)
  "Manage a ring of menu filters.
`recentf-filter-changer-alist' defines the filters in the ring.
Filtering of L is delegated to the current filter in the ring.  A
filter menu item is displayed allowing to dynamically activate the
next filter in the ring.  If the filter ring is empty, L is left
unchanged."
  (let ((filter (recentf-filter-changer-get-current)))
    (when filter
      (setq l (recentf-apply-menu-filter (car filter) l)
            filter (recentf-filter-changer-get-next))
      (when filter
        (setq recentf-menu-filter-commands
              (list (vector (cdr filter)
                            '(recentf-filter-changer-goto-next)
                            t)))))
    l))

;;; Common dialog stuff
;;
(defun recentf-cancel-dialog (&rest ignore)
  "Cancel the current dialog.
Used internally by recentf dialogs.
IGNORE arguments."
  (interactive)
  (kill-buffer (current-buffer))
  (message "Dialog canceled"))

(defvar recentf-button-keymap
  (let (parent-keymap mouse-button1 keymap)
    (if (featurep 'xemacs)
        (setq parent-keymap widget-button-keymap
              mouse-button1 [button1])
      (setq parent-keymap widget-keymap
            mouse-button1 [down-mouse-1]))
    (setq keymap (copy-keymap parent-keymap))
    (define-key keymap mouse-button1 'widget-button-click)
    keymap)
  "Keymap used inside buttons.")

(defvar recentf-dialog-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "q" 'recentf-cancel-dialog)
    (define-key km [down-mouse-1] 'widget-button-click)
    (set-keymap-parent km widget-keymap)
    km)
  "Keymap used in recentf dialogs.")

(defun recentf-dialog-mode ()
  "Major mode of recentf dialogs.

\\{recentf-dialog-mode-map}"
  (interactive)
  (setq major-mode 'recentf-dialog-mode)
  (setq mode-name "recentf-dialog")
  (use-local-map recentf-dialog-mode-map))

;;; Hooks
;;
(defun recentf-track-opened-file ()
  "Insert the name of the file just opened or written into the recent list."
  (and buffer-file-name
       (recentf-add-file buffer-file-name))
  ;; Must return nil because it is run from `write-file-hooks'.
  nil)

(defun recentf-track-closed-file ()
  "Update the recent list when a buffer is killed.
That is, remove a non readable file from the recent list, if
`recentf-keep-non-readable-files-flag' is nil."
  (and buffer-file-name
       (not recentf-keep-non-readable-files-flag)
       (recentf-remove-if-non-readable buffer-file-name)))

(defun recentf-update-menu ()
  "Update the recentf menu from the current recent list."
  (let ((cache (cons default-directory recentf-list)))
    ;; Does nothing, if nothing has changed.
    (unless (equal recentf-data-cache cache)
      (setq recentf-data-cache cache)
      (condition-case err
          (easy-menu-change recentf-menu-path
                            recentf-menu-title
                            (recentf-make-menu-items)
                            recentf-menu-before)
        (error
         (message "recentf update menu failed: %s"
                  (error-message-string err)))))))

(defconst recentf-menu-hook (if (boundp 'activate-menubar-hook)
                                'activate-menubar-hook
                              'menu-bar-update-hook)
  "Hook run to update the recentf menu.")

(defconst recentf-used-hooks
  `(
    (find-file-hooks    recentf-track-opened-file)
    (write-file-hooks   recentf-track-opened-file)
    (kill-buffer-hook   recentf-track-closed-file)
    (,recentf-menu-hook recentf-update-menu)
    (kill-emacs-hook    recentf-save-list)
    )
  "Hooks used by recentf.")

(defsubst recentf-enabled-p ()
  "Return non-nil if recentf mode is currently enabled."
  (memq 'recentf-update-menu (symbol-value recentf-menu-hook)))

;;; Commands
;;
(defvar recentf-edit-selected-items nil
  "List of files to be deleted from the recent list.
Used internally by `recentf-edit-list'.")

(defun recentf-edit-list-action (widget &rest ignore)
  "Checkbox WIDGET action that toogles a file selection.
Used internally by `recentf-edit-list'.
IGNORE other arguments."
  (let ((value (widget-get widget ':tag)))
    ;; if value is already in the selected items
    (if (memq value recentf-edit-selected-items)
        ;; then remove it
        (progn
          (setq recentf-edit-selected-items
                (delq value recentf-edit-selected-items))
          (message "%s removed from selection" value))
      ;; else add it
      (push value recentf-edit-selected-items)
      (message "%s added to selection" value))))

(defun recentf-edit-list ()
  "Show a dialog buffer to edit the recent list.
That is to select files to be deleted from the recent list."
  (interactive)
  (with-current-buffer
      (get-buffer-create (format "*%s - Edit list*" recentf-menu-title))
    (switch-to-buffer (current-buffer))
    ;; Cleanup buffer
    (kill-all-local-variables)
    (let ((inhibit-read-only t)
          (ol (recentf-overlay-lists)))
      (erase-buffer)
      ;; Delete all the overlays.
      (mapc 'recentf-delete-overlay (car ol))
      (mapc 'recentf-delete-overlay (cdr ol)))
    (setq recentf-edit-selected-items nil)
    ;; Insert the dialog header
    (widget-insert
     "\
Select the files to be deleted from the recent list.\n\n\
Click on Ok to update the list. \
Click on Cancel or type \"q\" to quit.\n")
    ;; Insert the list of files as checkboxes
    (dolist (item recentf-list)
      (widget-create
       'checkbox
       :value nil                       ; unselected checkbox
       :format "\n %[%v%]  %t"
       :tag item
       :notify 'recentf-edit-list-action))
    (widget-insert "\n\n")
    ;; Insert the Ok button
    (widget-create
     'push-button
     :button-keymap recentf-button-keymap ; XEmacs
     :keymap        recentf-button-keymap ; Emacs
     :notify (lambda (&rest ignore)
               (if recentf-edit-selected-items
                   (let ((i 0))
                     (kill-buffer (current-buffer))
                     (dolist (e recentf-edit-selected-items)
                       (setq recentf-list (delq e recentf-list)
                             i (1+ i)))
                     (message "%S file(s) removed from the list" i))
                 (message "No file selected")))
     "Ok")
    (widget-insert " ")
    ;; Insert the Cancel button
    (widget-create
     'push-button
     :button-keymap recentf-button-keymap ; XEmacs
     :keymap        recentf-button-keymap ; Emacs
     :notify 'recentf-cancel-dialog
     "Cancel")
    (recentf-dialog-mode)
    (widget-setup)
    (goto-char (point-min))))

(defun recentf-open-files-action (widget &rest ignore)
  "Button WIDGET action that open a file.
Used internally by `recentf-open-files'.
IGNORE other arguments."
  (kill-buffer (current-buffer))
  (funcall recentf-menu-action (widget-value widget)))

(defvar recentf-open-files-item-shift ""
  "Amount of space to shift right sub-menu items.
Used internally by `recentf-open-files'.")

(defun recentf-open-files-item (menu-element)
  "Insert an item widget for MENU-ELEMENT in the current dialog buffer.
Used internally by `recentf-open-files'."
  (let ((item (car menu-element))
        (file (cdr menu-element)))
    (if (consp file)               ; This is a sub-menu
        (let* ((shift recentf-open-files-item-shift)
               (recentf-open-files-item-shift (concat shift "  ")))
          (widget-create
           'item
           :tag item
           :sample-face 'bold
           :format (concat shift "%{%t%}:\n"))
          (mapc 'recentf-open-files-item file)
          (widget-insert "\n"))
      (widget-create
       'push-button
       :button-keymap recentf-button-keymap ; XEmacs
       :keymap        recentf-button-keymap ; Emacs
       :button-face 'default
       :tag item
       :help-echo (concat "Open " file)
       :format (concat recentf-open-files-item-shift "%[%t%]")
       :notify 'recentf-open-files-action
       file)
      (widget-insert "\n"))))

(defun recentf-open-files (&optional files buffer-name)
  "Show a dialog buffer to open a recent file.
If optional argument FILES is non-nil, it specifies the list of
recently-opened files to choose from.  It is the whole recent list
otherwise.
If optional argument BUFFER-NAME is non-nil, it specifies which buffer
name to use for the interaction.  It is \"*`recentf-menu-title'*\" by
default."
  (interactive)
  (unless files
    (setq files recentf-list))
  (unless buffer-name
    (setq buffer-name (format "*%s*" recentf-menu-title)))
  (with-current-buffer (get-buffer-create buffer-name)
    (switch-to-buffer (current-buffer))
    ;; Cleanup buffer
    (kill-all-local-variables)
    (let ((inhibit-read-only t)
          (ol (recentf-overlay-lists)))
      (erase-buffer)
      ;; Delete all the overlays.
      (mapc 'recentf-delete-overlay (car ol))
      (mapc 'recentf-delete-overlay (cdr ol)))
    ;; Insert the dialog header
    (widget-insert "Click on a file to open it. ")
    (widget-insert "Click on Cancel or type \"q\" to quit.\n\n" )
    ;; Insert the list of files as buttons
    (let ((recentf-open-files-item-shift ""))
      (mapc 'recentf-open-files-item
            (recentf-apply-menu-filter
             recentf-menu-filter
             (mapcar 'recentf-make-default-menu-element files))))
    (widget-insert "\n")
    ;; Insert the Cancel button
    (widget-create
     'push-button
     :button-keymap recentf-button-keymap ; XEmacs
     :keymap        recentf-button-keymap ; Emacs
     :notify 'recentf-cancel-dialog
     "Cancel")
    (recentf-dialog-mode)
    (widget-setup)
    (goto-char (point-min))))

(defun recentf-open-more-files ()
  "Show a dialog buffer to open a recent file that is not in the menu."
  (interactive)
  (recentf-open-files (nthcdr recentf-max-menu-items recentf-list)
                      (format "*%s - More*" recentf-menu-title)))

(defconst recentf-save-file-header
  ";;; Automatically generated by `recentf' on %s.\n"
  "Header to be written into the `recentf-save-file'.")

(defun recentf-save-list ()
  "Save the recent list.
Write data into the file specified by `recentf-save-file'."
  (interactive)
  (with-temp-file (expand-file-name recentf-save-file)
    (erase-buffer)
    (insert (format recentf-save-file-header (current-time-string)))
    (recentf-dump-variable 'recentf-list recentf-max-saved-items)
    (recentf-dump-variable 'recentf-filter-changer-state)
    nil))

(defun recentf-load-list ()
  "Load a previously saved recent list.
Read data from the file specified by `recentf-save-file'."
  (interactive)
  (let ((file (expand-file-name recentf-save-file)))
    (when (file-readable-p file)
      (load-file file))))

(defun recentf-cleanup ()
  "Remove all non-readable and excluded files from the recent list."
  (interactive)
  (message "Cleaning up the recentf list...")
  (let (newlist)
    (dolist (f recentf-list)
      (if (and (file-readable-p f) (recentf-include-p f))
          (push f newlist)
        (message "File %s removed from the recentf list" f)))
    (setq recentf-list (nreverse newlist))
    (message "Cleaning up the recentf list...done")))

;;;###autoload
(defun recentf-mode (&optional arg)
  "Toggle recentf mode.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

When recentf mode is enabled, it maintains a menu for visiting files
that were operated on recently."
  (interactive "P")
  (setq recentf-mode (if arg
                         (> (prefix-numeric-value arg) 0)
                       (not recentf-mode)))
  (unless (and recentf-mode (recentf-enabled-p))
    (if recentf-mode
        (recentf-load-list)
      (recentf-save-list))
    (recentf-auto-cleanup)
    (recentf-clear-data)
    (let ((hook-setup (if recentf-mode 'add-hook 'remove-hook)))
      (dolist (hook recentf-used-hooks)
        (apply hook-setup hook)))
    (run-hooks 'recentf-mode-hook)
    (when (interactive-p)
      (message "Recentf mode %sabled" (if recentf-mode "en" "dis"))))
  recentf-mode)

(provide 'recentf)

(run-hooks 'recentf-load-hook)

;;; recentf.el ends here
