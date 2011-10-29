;;; Function keymap for linux console:
;;;Following lines taken from lk20.el
;;; Fix some keypad keys:
(global-unset-key "\e\e")
(define-key function-key-map "\e[1~" [find])
(define-key function-key-map "\e[2~" [insert])
(define-key function-key-map "\e[3~" [delete])
(define-key function-key-map "\e[4~" [select])
;;; Function key 11 -- 20
(define-key function-key-map "\e[23~" [f11])
(define-key function-key-map "\e[24~" [f12])
(define-key function-key-map "\e[25~" [f13])
(define-key function-key-map "\e[26~" [f14])
(define-key function-key-map "\e[28~" [help])
(define-key function-key-map "\e[29~" [menu])
(define-key function-key-map "\e[31~" [f17])
(define-key function-key-map "\e[32~" [f18])
(define-key function-key-map "\e[33~" [f19])
(define-key function-key-map "\e[34~" [f20])

;;; And finally handle the keypad keys:

(define-key function-key-map "\eOP" [kp-f1])
(define-key function-key-map "\eOQ" [kp-f2])
(define-key function-key-map "\eOR" [kp-f3])
(define-key function-key-map "\eOS" [kp-f4])

(define-key function-key-map "\eOI" [kp-tab])
(define-key function-key-map "\eOj" [kp-multiply])
(define-key function-key-map "\eOk" [kp-add])
(define-key function-key-map "\eOl" [kp-separator])
(define-key function-key-map "\eOM" [kp-enter])
(define-key function-key-map "\eOm" [kp-subtract])
(define-key function-key-map "\eOn" [kp-decimal])
(define-key function-key-map "\eOo" [kp-divide])
(define-key function-key-map "\eOp" [kp-0])
(define-key function-key-map "\eOq" [kp-1])
(define-key function-key-map "\eOr" [kp-2])
(define-key function-key-map "\eOs" [kp-3])
(define-key function-key-map "\eOt" [kp-4])
(define-key function-key-map "\eOu" [kp-5])
(define-key function-key-map "\eOv" [kp-6])
(define-key function-key-map "\eOw" [kp-7])
(define-key function-key-map "\eOx" [kp-8])
(define-key function-key-map "\eOy" [kp-9])
;;; shift arrows

(define-key function-key-map  "\e[E" '[S-up])
   (define-key function-key-map "\e[F" '[S-down])
   (define-key function-key-map "\e[G" '[S-right])
   (define-key function-key-map "\e[H" '[S-left])
;;; Get control arrows:

(define-key function-key-map "\e[I" '[C-up])
(define-key function-key-map  "\e[J" '[C-down])
(define-key function-key-map "\e[K" '[C-left])
(define-key function-key-map "\e[L" '[C-right])
;;; shift tab:
(define-key function-key-map "\033[T" '[S-tab])
