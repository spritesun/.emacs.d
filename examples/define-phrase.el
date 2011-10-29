;; Add the following lines to your init file.
;; (if (file-exists-p user-py-file)
;;     (load-file user-py-file))

(require 'mule)
(require 'quail)

;; �û������ļ�
(defvar user-py-file "~/emacs/USER_PY.el")
;; �û�������¼�������ļ�
(defvar user-people-names-file "~/emacs/USER_PEOPLE_NAMES.el")
;;ת������Ŀ�ʼ�ı�ǣ�һ���û������ļ�����֮��Ҫ�Ķ��������������
(defvar begin-translation-rulers-tag ";;Beginning of translation rulers")

;;�����û������ļ���ͷ
(defun new-quail-head ()
  (insert (format "(quail-define-rules\n((append . t))\n%s\n)"
		  begin-translation-rulers-tag)))

;;  ���������·��������뷨�Ĵʿ���ӹ���.
;;  ����� Emacs �� init file �����һ��:
;;
;;      (global-set-key "\C-cd" 'quail-define-new-ruler-from-line)
;;
;;  ��������������뷨���һ���û�����: sndsd -> ȫ�ܵ��ϵ�
;;  �������һ�� buffer (����: *scratch*) ���������µ�һ��
;;
;;      ȫ�ܵ��ϵ�  qndsd
;;
;;  Ȼ���ڴ��д����� C-c d
;;  ���� M-x quail-define-new-ruler-from-line
(defun quail-define-new-ruler-from-line ()
  "Add a new translation rule to the quail package named NAME.
The new rule is scaned from current line in the buffer.
A line looks like:
    ȫ�ܵ��ϵ�  qndsd
will define a rule with key \"qndsd\" and translation \"ȫ�ܵ��ϵ�\"."
  (interactive)
  (let (key translation)
    (save-excursion
      (end-of-line)
      (backward-word 1)
      (setq key (current-word))
      (backward-word 1)
      (setq translation (current-word)))
    (quail-define-new-ruler key translation)))


(defun quail-define-new-ruler (key translation &optional name)
  "Add a new translation rule to the quail package named NAME.
if NAME is not specified, add to the current package."
  ;;  (interactive (read-key-translation "Define the key"))
  (interactive (list (read-string "Key: ") (read-string "Translation: ")))
  (let ((vector (cdar (quail-lookup-key key)))
        (index 0)
        len)
    (if vector (setq len (length vector))
      (setq len 0))
    (while 
        (and (< index len)
             (not (string= translation (aref vector index))))
      (setq index (1+ index)))
    (if (= index len) ;; not redefined
        (progn
          ;;�����û�����
          (quail-defrule key (vector translation) name t)
          ;;����˴��鵽�û������ļ���
          (save-excursion
            (let ((buffer (get-file-buffer user-py-file)))
              (or buffer
                  (setq buffer (get-buffer-create user-py-file)))
              (set-buffer buffer)
              (goto-char (point-min))
              (if (file-exists-p user-py-file)
                  (insert-file-contents user-py-file nil nil nil t)
                (new-quail-head))
              (goto-char (point-min))
              (if (search-forward
                   begin-translation-rulers-tag nil t)
                  (progn
                    (if (search-forward-regexp
                         (concat "^" (regexp-quote (concat "(\"" key "\" ")))
                         nil t)
                        (progn
                          (search-forward-regexp "\\])$" nil t)
                          (goto-char (match-beginning 0))
                          (insert " \"" translation "\""))
                      (progn
                        (forward-line)
                        (beginning-of-line)
                        (insert (format "(\"%s\" [\"%s\"])\n" key translation))))
                    (write-file user-py-file)
                    (kill-buffer buffer))
                (message "No tag found!")))))
      (message "Rule: \"%s\" to \"%s\" already defined!"
               key translation))))

(defun quail-define-new-ruler-for-name-of-people-from-line ()
  "��¼����������."
  (interactive)
  (let ((user-py-file user-people-names-file))
    (quail-define-new-ruler-from-line)))