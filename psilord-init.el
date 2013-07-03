;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow F to visit all marked files in dired
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make it easier to see unique files in the mini-buffer header
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make buffer movement easier on the keyboard
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x C-,") 'previous-buffer)
(global-set-key (kbd "C-x ,") 'previous-buffer)
(global-set-key (kbd "C-x C-.") 'next-buffer)
(global-set-key (kbd "C-x .") 'next-buffer)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup org mode.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done 'time)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure lisp (or other functional languages) don't have tabs in them
;; when you save them.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; untabify some modes
(setq untabify-modes '(haskell-mode emacs-lisp-mode lisp-mode scheme-mode
                                    erlang-mode clojure-mode))
(defun untabify-hook ()
  (when (member major-mode untabify-modes)
     (delete-trailing-whitespace)
     (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'untabify-hook)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For the hyperspec lookup in SLIME, run the 'web-browser' script which
;; will use w3m.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-browse (url &rest ignore)
  "Browse URL using w3m."
  (interactive "sURL: ")
  (shell-command (concat "w3m " url))
  (pop-to-buffer "*Shell Command Output*")
  (setq truncate-lines t))

(if window-system
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "web-browser")
  (setq browse-url-browser-function 'my-browse))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Live Dangerously and don't make the stupid ~ backup files. They annoy me.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq make-backup-files nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shift arrows move between buffers in a frame
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I usually want to save sessions in individual directories...
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (desktop-save-mode 1)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I want to see the mark selections I do.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq transient-mark-mode t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I want to move by entire line, not by the visual line.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq line-move-visual nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make more accessable the bindings to shrink and enlarge buffers
;; The regular keybindings suck.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Choose a nicer default font, and make sure it is enabled quickly
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(let ((myfont "-misc-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*"))
;;(let ((myfont "DejaVu Sans Mono Book 9"))
(let ((myfont "7x13"))
  (set-default-font myfont)
  (set-face-attribute 'default nil :font myfont)
  ;; Also ensure that new frames get the font I want!
  (add-to-list 'default-frame-alist `(font . ,myfont))
  (modify-frame-parameters nil '((wait-for-wm . nil))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I like hippie expand a lot. Sadly this is borken and needs work.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq hippie-expand-try-functions-list
;;      '(try-expand-dabbrev
;;        try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
;;        try-complete-file-name-partially try-complete-file-name
;;        try-expand-all-abbrevs try-expand-list try-expand-line
;;        try-complete-lisp-symbol-partially try-complete-lisp-symbol))
;;(global-set-key "\M-TAB" 'hippie-expand)

;; this doesn't work yet, maybe add in hook after slime is loaded?
;;(setq slime-complete-symbol-function 'hippie-expand)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get rid of the scroll bars, I don't need them.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(scroll-bar-mode -1)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I don't like the spash screen
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-splash-screen t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I like more of a vim-like behavior when killing a whole line with C-k.
;; When the point is C-a, then C-k will act like dd.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq kill-whole-line t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto newline and indent when I hit return
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-m" 'newline-and-indent)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Change the annoying 'yes'/'no' query to just 'y'/'n'
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fset 'yes-or-no-p 'y-or-n-p)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When I come to the top/bottom of the screen with C-n/p, scroll one line only
;; This is different than "(setq scroll-step 1)" because the latter will have
;; bad boundary behavior at the end of the buffer.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scroll-conservatively most-positive-fixnum)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When [f12] is hit, indentify the whole buffer. Very useful for Common Lisp
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(global-set-key [f12] 'iwb)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is my attempt and making a reliable vim dd and binding it to M-K
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vim-dd ()
  "Act like Vim's dd command."
  (interactive)
  (let ((col (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (move-to-column col)))
(global-set-key (kbd "M-K") 'vim-dd)

;; If my point is on a ( or ), then bounce it like % in vim
;; This may be annoying if I actually wish to start or end a variable with
;; this character.
(defun bounce-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key "%" 'bounce-paren)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Increment decimal a number under the point
;; doesn't exactly work with negative numbers. :(
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
(global-set-key (kbd "C-c +") 'increment-number-at-point)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Decrement decimal a number under the point
;; doesn't exactly work with negative numbers. :(
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))
(global-set-key (kbd "C-c -") 'decrement-number-at-point)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up a color theme that is good enough for now.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'color-theme nil 'noerror)
  (if window-system
    (progn
          ;; comment in below line for emacs23
          ;;(color-theme-initialize)
          (color-theme-comidia)
          (message "Enabled comida theme"))
        (message "No color-theme available in tty!")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slime configuration
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new hotness quicklisp configuration
(when t
  (setq slime-lisp-implementations
        '((sbcl ("/home/psilord/bin/sbcl"))
          (alisp ("/home/psilord/bin/alisp"))))
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; (slime)
  )

;; old and busted personal slime configuration...
(when nil
  ;; /home/psilord/content/code/lisp/slime is a slime repo checked out with:
  ;; cvs -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime
  (set-language-environment "UTF-8")
  (setq slime-net-coding-system 'utf-8-unix)
  (setq load-path
        (cons "/home/psilord/content/code/lisp/slime" load-path))
  (setq load-path
        (cons "/home/psilord/content/code/lisp/slime/contrib" load-path))
  (setq slime-backend
        "/home/psilord/content/code/lisp/slime/swank-loader.lisp")
  (load "/home/psilord/content/code/lisp/slime/slime")

  ;; Set up which lisp I want to use as my inferior lisp. TODO, make it so 
  ;; I have a choice.
  ;; (setq inferior-lisp-program "/home/psilord/bin/sbcl --dynamic-space-size 2000")
  ;; (setq inferior-lisp-program "/home/psilord/bin/alisp")

  (slime-setup '(slime-fancy slime-tramp slime-asdf slime-xref-browser))
  (slime-require :swank-listener-hooks)
  (add-hook 'slime-mode-hook 'slime-redirect-inferior-output)
  (slime))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up some hilighting rules for all known ANSI common lisp functions and
;; macro names, special operators, etc. This assumes color-theme-comidia.
;; This is always tinkerable....
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface ansi-lisp-boolean
  '((t (:foreground "white")))
  "Color all of the ANSI Common Lisp Booleans this specific color")

(defface ansi-lisp-constant
  '((t (:foreground "orchid1")))
  "Color all of the ANSI Common Lisp Constants this specific color")

(defface ansi-lisp-declaration
  '((t (:foreground "indianred2")))
  "Color all of the ANSI Common Lisp Declarations this specific color")

(defface ansi-lisp-condition-type
  '((t (:foreground "indianred2")))
  "Color all of the ANSI Common Lisp Condition Types this specific color")

(defface ansi-lisp-function
  '((t (:foreground "pale green")))
  "Color all of the ANSI Common Lisp Functions this specific color")

(defface ansi-lisp-generic-function
  '((t (:foreground "cyan"))) ; same as the comida color-theme which I use.
  "Color all of the ANSI Common Lisp Generic Functions this specific color")

(defface ansi-lisp-macro
  '((t (:foreground "cyan"))) ; same as comida color-theme which I use.
  "Color all of the ANSI Common Lisp Macros this specific color")

(defface ansi-lisp-special-operator
  '((t (:foreground "cyan"))) ; same as macros and as the comida color-theme.
  "Color all of the ANSI Common Lisp Special Operators this specific color")

(defface ansi-lisp-type
  '((t (:foreground "red")))
  "Color all of the ANSI Common Lisp Types this specific color")

(defface ansi-lisp-unknown
  '((t (:foreground "red")))
  "Color all of the ANSI Common Lisp <mumble> this specific color")

(defface ansi-lisp-global-variable
  '((t (:foreground "yellow2")))
  "Color all of the ANSI Common Lisp Globals this specific color")

(defface ansi-lisp-expression
  '((t (:foreground "indianred1")))
  "Color all of the ANSI Common Lisp Expressions (like declare) this specific color")

(defface ansi-lisp-parenthesis
  '((t (:foreground "#4d4d3d")))  ; grey25, but with more yellow in it
  "Color all of the ANSI Common Lisp Parenthesis")

(defface ansi-lisp-numbers
  '((t (:foreground "orchid1")))
  "Color all of the ANSI Common Lisp Numbers")

;;; These are functions which produce the individual entries for each kind of
;;; symbol name according to what face they should have.

(defun ansi-boolean (x)
  (list
   (concatenate
    'string "\\<\\(" (symbol-name x) "\\)\\>") 0 ''ansi-lisp-boolean))

(defun ansi-constant (x)
  (list
   (concatenate
    'string "\\<\\(" (symbol-name x) "\\)\\>") 0 ''ansi-lisp-constant))

(defun ansi-declaration (x)
  (list
   (concatenate
    'string "\\<\\(" (symbol-name x) "\\)\\>") 0 ''ansi-lisp-declaration))

(defun ansi-condition-type (x)
  (list
   (concatenate
    'string "\\<\\(" (symbol-name x) "\\)\\>") 0 ''ansi-lisp-condition-type))

(defun ansi-function (x)
  (list
   (concatenate
    'string "\\<\\(" (symbol-name x) "\\)\\>") 0 ''ansi-lisp-function))

(defun ansi-generic-function (x)
  (list
   (concatenate
    'string "\\<\\(" (symbol-name x) "\\)\\>") 0 ''ansi-lisp-generic-function))

(defun ansi-macro (x)
  (list
   (concatenate
    'string "\\<\\(" (symbol-name x) "\\)\\>") 0 ''ansi-lisp-macro))

(defun ansi-special-operator (x)
  (list
   (concatenate
    'string "\\<\\(" (symbol-name x) "\\)\\>") 0 ''ansi-lisp-special-operator))

(defun ansi-type (x)
  (list
   (concatenate
    'string "\\<\\(" (symbol-name x) "\\)\\>") 0 ''ansi-lisp-type))

(defun ansi-unknown (x)
  (list
   (concatenate
    'string "\\<\\(" (symbol-name x) "\\)\\>") 0 ''ansi-lisp-unknown))

(defun ansi-global-variable (x)
  (list
   (concatenate
    'string "\\<\\(" (symbol-name x) "\\)\\>") 0 ''ansi-lisp-global-variable))

(defun ansi-expression (x)
                                        ; used for things like declare
  (list
   (concatenate
    'string "\\<\\(" (symbol-name x) "\\)\\>") 0 ''ansi-lisp-expression))

(when t ;; sometimes I need to disable this whole thing....
  (add-hook 'lisp-mode-hook
            (lambda ()
              (font-lock-add-keywords
               nil
               (append
                ;; Conventional Constant Variables
                '(("\\<\\([+][^ +]*[+]\\)\\>" 0 #'ansi-lisp-constant))

                ;; Conventional Global Variables, including ANSI ones
                '(("\\<\\([*][^ *]*[*]\\)\\>" 0 #'ansi-lisp-global-variable))

                ;; Lisp numbers, hexadecimal
                '(("\\([#][BbXx][0-9A-Fa-f]+\\)" 0 #'ansi-lisp-numbers))

                ;; Lisp Numbers, simple ones
                '(("\\<\\([+-]?[0-9]+?[.]?[0-9]+\\)\\>" 0 #'ansi-lisp-numbers))
                '(("\\<\\([+-]?[.]?[0-9]+\\)\\>" 0 #'ansi-lisp-numbers))

                ;; I'm a psycho and want my parentheis color to be controlled.
                '(("\\([()]\\)" 0 #'ansi-lisp-parenthesis))

                ;; These are often important to see, but I don't know how to
                ;; highlight the matching parenthesis with it
                ;;'(("\\([#][']\\)" 0 'ansi-lisp-boolean))

                (mapcar #'ansi-boolean
                        '(nil t NIL T))

                (mapcar #'ansi-constant
                        '(array-dimension-limit array-rank-limit
                                                array-total-size-limit boole-1 boole-2 boole-and
                                                boole-andc1 boole-andc2 boole-c1 boole-c2 boole-clr
                                                boole-eqv boole-ior boole-nand boole-nor boole-orc1
                                                boole-orc2 boole-set boole-xor call-arguments-limit
                                                char-code-limit double-float-epsilon
                                                double-float-negative-epsilon
                                                internal-time-units-per-second
                                                lambda-list-keywords lambda-parameters-limit
                                                least-negative-double-float least-negative-long-float
                                                least-negative-normalized-double-float
                                                least-negative-normalized-long-float
                                                least-negative-normalized-short-float
                                                least-negative-normalized-single-float
                                                least-negative-short-float
                                                least-negative-single-float
                                                least-positive-double-float
                                                least-positive-long-float
                                                least-positive-normalized-double-float
                                                least-positive-normalized-long-float
                                                least-positive-normalized-short-float
                                                least-positive-normalized-single-float
                                                least-positive-short-float
                                                least-positive-single-float
                                                long-float-epsilon
                                                long-float-negative-epsilon
                                                most-negative-double-float most-negative-fixnum
                                                most-negative-long-float most-negative-short-float
                                                most-negative-single-float most-positive-double-float
                                                most-positive-fixnum most-positive-long-float
                                                most-positive-short-float most-positive-single-float
                                                multiple-values-limit pi short-float-epsilon
                                                short-float-negative-epsilon single-float-epsilon
                                                single-float-negative-epsilon))

                (mapcar #'ansi-declaration
                        '(type compilation-speed debug declaration
                               dynamic-extent ftype ignorable ignore inline
                               notinline optimize safety space special speed))

                (mapcar #'ansi-condition-type
                        '(arithmetic-error cell-error condition control-error
                                           division-by-zero end-of-file file-error
                                           floating-point-inexact
                                           floating-point-invalid-operation
                                           floating-point-overflow floating-point-underflow
                                           package-error parse-error print-not-readable
                                           program-error reader-error serious-condition
                                           simple-condition simple-error simple-type-error
                                           simple-warning storage-condition stream-error
                                           style-warning type-error unbound-slot
                                           unbound-variable undefined-function warning))

                (mapcar #'ansi-function
                        '(- / \* \+ abort and atom bit
                            character complex cons continue eql error
                            float list logical-pathname member mod
                            muffle-warning not null pathname rational
                            store-value string use-value values vector /=
                            1- 1\+ < <= = > >= abs acons acos acosh adjoin
                            adjust-array adjustable-array-p alpha-char-p
                            alphanumericp append apply apropos
                            apropos-list aref arithmetic-error-operands
                            arithmetic-error-operation array-dimension
                            array-dimensions array-displacement
                            array-element-type array-has-fill-pointer-p
                            array-in-bounds-p array-rank
                            array-row-major-index array-total-size arrayp
                            ash asin asinh assoc assoc-if assoc-if-not
                            atan atanh bit-and bit-andc1 bit-andc2 bit-eqv
                            bit-ior bit-nand bit-nor bit-not bit-orc1
                            bit-orc2 bit-vector-p bit-xor boole
                            both-case-p boundp break
                            broadcast-stream-streams butlast byte
                            byte-position byte-size caaaar caaadr caaar
                            caadar caaddr caadr caar cadaar cadadr cadar
                            caddar cadddr caddr cadr call-next-method car
                            cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar
                            cddaar cddadr cddar cdddar cddddr cdddr cddr
                            cdr ceiling cell-error-name cerror char
                            char-code char-downcase char-equal
                            char-greaterp char-int char-lessp char-name
                            char-not-equal char-not-greaterp
                            char-not-lessp char-upcase char/= char< char<=
                            char= char> char>= characterp cis class-of
                            clear-input clear-output close clrhash
                            code-char coerce compile compile-file
                            compile-file-pathname compiled-function-p
                            compiler-macro compiler-macro-function
                            complement complexp compute-restarts
                            concatenate concatenated-stream-streams
                            conjugate consp constantly constantp
                            copy-alist copy-list copy-pprint-dispatch
                            copy-readtable copy-seq copy-structure
                            copy-symbol copy-tree cos cosh count count-if
                            count-if-not decode-float
                            decode-universal-time delete delete-duplicates
                            delete-file delete-if delete-if-not
                            delete-package denominator deposit-field
                            describe describe-object digit-char
                            digit-char-p directory directory-namestring
                            disassemble documentation dpb dribble
                            echo-stream-input-stream
                            echo-stream-output-stream ed eighth elt
                            encode-universal-time endp enough-namestring
                            ensure-directories-exist
                            ensure-generic-function eq equal equalp eval
                            evenp every exp export expt fboundp fceiling
                            fdefinition ffloor fifth file-author
                            file-error-pathname file-length
                            file-namestring file-position
                            file-string-length file-write-date fill
                            fill-pointer find find-all-symbols find-class
                            find-if find-if-not find-package find-restart
                            find-symbol finish-output first float-digits
                            float-precision float-radix float-sign floatp
                            floor fmakunbound force-output format fourth
                            fresh-line fround ftruncate funcall
                            function-lambda-expression functionp gcd
                            gensym gentemp get get-decoded-time
                            get-dispatch-macro-character
                            get-internal-real-time get-internal-run-time
                            get-macro-character get-output-stream-string
                            get-properties get-setf-expansion
                            get-universal-time getf gethash graphic-char-p
                            hash-table-count hash-table-p
                            hash-table-rehash-size
                            hash-table-rehash-threshold hash-table-size
                            hash-table-test host-namestring identity
                            imagpart import input-stream-p inspect
                            integer-decode-float integer-length integerp
                            interactive-stream-p intern intersection
                            invalid-method-error invoke-debugger
                            invoke-restart invoke-restart-interactively
                            isqrt keywordp last lcm ldb ldb-test ldiff
                            length lisp-implementation-type
                            lisp-implementation-version list-all-packages
                            list-length list\* listen listp load
                            load-logical-pathname-translations log logand
                            logandc1 logandc2 logbitp logcount logeqv
                            logical-pathname-translations logior lognand
                            lognor lognot logorc1 logorc2 logtest logxor
                            long-site-name lower-case-p machine-instance
                            machine-type machine-version macro-function
                            macroexpand macroexpand-1 make-array
                            make-broadcast-stream make-concatenated-stream
                            make-condition make-dispatch-macro-character
                            make-echo-stream make-hash-table make-list
                            make-load-form-saving-slots make-package
                            make-pathname make-random-state make-sequence
                            make-string make-string-input-stream
                            make-string-output-stream make-symbol
                            make-synonym-stream make-two-way-stream
                            makunbound map map-into mapc mapcan mapcar
                            mapcon maphash mapl maplist mask-field max
                            member-if member-if-not merge merge-pathnames
                            method-combination-error min minusp mismatch
                            name-char namestring nbutlast nconc
                            next-method-p nintersection ninth notany
                            notevery nreconc nreverse nset-difference
                            nset-exclusive-or nstring-capitalize
                            nstring-downcase nstring-upcase nsublis nsubst
                            nsubst-if nsubst-if-not nsubstitute
                            nsubstitute-if nsubstitute-if-not nth nthcdr
                            numberp numerator nunion oddp open
                            open-stream-p output-stream-p
                            package-error-package package-name
                            package-nicknames package-shadowing-symbols
                            package-use-list package-used-by-list packagep
                            pairlis parse-integer parse-namestring
                            pathname-device pathname-directory
                            pathname-host pathname-match-p pathname-name
                            pathname-type pathname-version pathnamep
                            peek-char phase plusp position position-if
                            position-if-not pprint pprint-dispatch
                            pprint-fill pprint-indent pprint-linear
                            pprint-newline pprint-tab pprint-tabular prin1
                            prin1-to-string princ princ-to-string print
                            print-not-readable-object print-object
                            probe-file proclaim provide random
                            random-state-p rassoc rassoc-if rassoc-if-not
                            rationalize rationalp read read-byte read-char
                            read-char-no-hang read-delimited-list
                            read-from-string read-line
                            read-preserving-whitespace read-sequence
                            readtable-case readtablep realp realpart
                            reduce rem remhash remove remove-duplicates
                            remove-if remove-if-not remprop rename-file
                            rename-package replace require rest
                            restart-name revappend reverse room round
                            row-major-aref rplaca rplacd sbit scale-float
                            schar search second set set-difference
                            set-dispatch-macro-character set-exclusive-or
                            set-macro-character set-pprint-dispatch
                            set-syntax-from-char seventh shadow
                            shadowing-import short-site-name signal signum
                            simple-bit-vector-p
                            simple-condition-format-arguments
                            simple-condition-format-control
                            simple-string-p simple-vector-p sin sinh sixth
                            sleep slot-boundp slot-exists-p
                            slot-makunbound slot-value software-type
                            software-version some sort special-operator-p
                            sqrt stable-sort standard-char-p
                            stream-element-type stream-error-stream
                            stream-external-format streamp
                            string-capitalize string-downcase string-equal
                            string-greaterp string-left-trim string-lessp
                            string-not-equal string-not-greaterp
                            string-not-lessp string-right-trim string-trim
                            string-upcase string/= string< string<=
                            string= string> string>= stringp structure
                            sublis subseq subsetp subst subst-if
                            subst-if-not substitute substitute-if
                            substitute-if-not subtypep svref sxhash
                            symbol-function symbol-name symbol-package
                            symbol-plist symbol-value symbolp
                            synonym-stream-symbol tailp tan tanh tenth
                            terpri third translate-logical-pathname
                            translate-pathname tree-equal truename
                            truncate two-way-stream-input-stream
                            two-way-stream-output-stream type-error-datum
                            type-error-expected-type type-of typep
                            unbound-slot-instance unexport unintern union
                            unread-char unuse-package
                            upgraded-array-element-type
                            upgraded-complex-part-type upper-case-p
                            use-package user-homedir-pathname values-list
                            variable vector-pop vector-push
                            vector-push-extend vectorp warn
                            wild-pathname-p write write-byte write-char
                            write-line write-sequence write-string
                            write-to-string y-or-n-p yes-or-no-p zerop))

                (mapcar #'ansi-generic-function
                        '(add-method
                          allocate-instance change-class class-name
                          compute-applicable-methods find-method
                          function-keywords initialize-instance make-instance
                          make-instances-obsolete make-load-form
                          method-qualifiers no-applicable-method no-next-method
                          reinitialize-instance remove-method shared-initialize
                          slot-missing slot-unbound
                          update-instance-for-different-class
                          update-instance-for-redefined-class))

                (mapcar #'ansi-macro
                        '(lambda or setf assert
                           call-method case ccase check-type cond
                           ctypecase decf declaim defclass defconstant
                           defgeneric define-compiler-macro
                           define-condition define-method-combination
                           define-modify-macro define-setf-expander
                           define-symbol-macro defmacro defmethod
                           defpackage defparameter defsetf defstruct
                           deftype defun defvar destructuring-bind do
                           do-all-symbols do-external-symbols do-symbols
                           do\* dolist dotimes ecase etypecase formatter
                           handler-bind handler-case ignore-errors
                           in-package incf loop loop-finish make-method
                           multiple-value-bind multiple-value-list
                           multiple-value-setq nth-value otherwise pop
                           pprint-exit-if-list-exhausted
                           pprint-logical-block pprint-pop
                           print-unreadable-object prog prog1 prog2
                           prog\* psetf psetq push pushnew remf
                           restart-bind restart-case return rotatef
                           shiftf step time trace typecase unless untrace
                           when with-accessors with-compilation-unit
                           with-condition-restarts
                           with-hash-table-iterator
                           with-input-from-string with-open-file
                           with-open-stream with-output-to-string
                           with-package-iterator with-simple-restart
                           with-slots with-standard-io-syntax))

                (mapcar #'ansi-special-operator
                        '(function block catch
                                   eval-when flet go if labels let let\*
                                   load-time-value locally macrolet
                                   multiple-value-call multiple-value-prog1 progn
                                   progv quote return-from setq symbol-macrolet
                                   tagbody the throw unwind-protect))

                (mapcar #'ansi-type
                        '(array base-char base-string
                                bignum bit-vector boolean broadcast-stream
                                built-in-class class compiled-function
                                concatenated-stream double-float echo-stream
                                extended-char file-stream fixnum
                                generic-function hash-table integer keyword
                                long-float method number package random-state
                                ratio readtable real restart satisfies
                                sequence short-float signed-byte simple-array
                                simple-base-string simple-bit-vector
                                simple-string simple-vector single-float
                                standard-char standard-class
                                standard-generic-function standard-method
                                standard-object stream string-stream
                                structure-class structure-object symbol
                                synonym-stream two-way-stream unsigned-byte))

                (mapcar #'ansi-unknown
                        '(method-combination))

                (mapcar #'ansi-global-variable
                        '(// /// \*\* \*\*\*
                             \*break-on-signals\* \*compile-file-pathname\*
                             \*compile-file-truename\* \*compile-print\*
                             \*compile-verbose\* \*debug-io\*
                             \*debugger-hook\*
                             \*default-pathname-defaults\* \*error-output\*
                             \*features\* \*gensym-counter\*
                             \*load-pathname\* \*load-print\*
                             \*load-truename\* \*load-verbose\*
                             \*macroexpand-hook\* \*modules\* \*package\*
                             \*print-array\* \*print-base\* \*print-case\*
                             \*print-circle\* \*print-escape\*
                             \*print-gensym\* \*print-length\*
                             \*print-level\* \*print-lines\*
                             \*print-miser-width\*
                             \*print-pprint-dispatch\* \*print-pretty\*
                             \*print-radix\* \*print-readably\*
                             \*print-right-margin\* \*query-io\*
                             \*random-state\* \*read-base\*
                             \*read-default-float-format\* \*read-eval\*
                             \*read-suppress\* \*readtable\*
                             \*standard-input\* \*standard-output\*
                             \*terminal-io\* \*trace-output\* \+\+ \+\+\+))

                (mapcar #'ansi-expression
                        '(declare))

                )))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code which sizes the emacs window according to my screen size
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make it so my emacs window is the right size and width.
;;(defun set-frame-size-according-to-resolution ()
  ;;(interactive)
  ;;(if window-system
      ;;(progn
        ;; use 80 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        ;;(if (> (x-display-pixel-width) 1280)
            ;;(add-to-list 'default-frame-alist (cons 'width 80))
          ;;(add-to-list 'default-frame-alist (cons 'width 80)))
        ;; for the height, subtract a couple hundred pixels
        ;; from the screen height (for panels, menubars and
        ;; whatnot), then divide by the height of a char to
        ;; get the height we want
        ;;(add-to-list 'default-frame-alist
                     ;;(cons 'height (/ (- (x-display-pixel-height) 50) (frame-char-height)))))))

;;(set-frame-size-according-to-resolution)

(defun toggle-fullscreen ()
  (interactive)
  (if window-system
    (progn
      (add-to-list 'default-frame-alist (cons 'width 80))

      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))

      ;;(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
      ;;              '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))

      (mapc #'(lambda (frame) (redraw-frame frame))
        (visible-frame-list))
      (force-window-update)
      (redisplay t)
      (redraw-display)))
)
(toggle-fullscreen)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the usual boiler plate junk that I customized in emacs via the
;; menu system.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(fringe-mode 0 nil (fringe))
 '(safe-local-variable-values (quote ((Syntax . Common-Lisp))))
 '(save-place t nil (saveplace))
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
