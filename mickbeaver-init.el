(require 'cl)
(require 'cc-mode)


;; FUNCTIONS ----------------------------------------------------------
(defun font-exists-p (font)
  "See if a font exists, returning the name if it does"
  (if window-system
      (if (null (x-list-fonts font))
          nil
        font)
    nil))

(defun switch-speedbar-between-files-and-buffers ()
  "Switch between files and buffers mode in the speedbar"
  (interactive)
  (when (boundp 'speedbar-initial-expansion-list-name)
    (if (string= "buffers" speedbar-initial-expansion-list-name)
        (speedbar-change-initial-expansion-list "files")
      (speedbar-change-initial-expansion-list "buffers"))
    (speedbar-update-contents)))

(defun run-python-on-current-buffer-through-compile ()
  "Run the current buffer using Python and using the compile system"
  (interactive)
  (compile (concat "python " (buffer-name))))

(defun exe-in-path-p (exe)
  "Check if a given executable is in the PATH"
  (interactive)
  (some #'file-exists-p (mapcar (lambda (x) (concat x exe))
                                (parse-colon-path (getenv "PATH")))))

(defun get-arch ()
  (cond ((or (eql 0 (string-match "i386" system-configuration))
             (eql 0 (string-match "i686" system-configuration))) 'x86)
        ((or (eql 0 (string-match "x86_64" system-configuration))
             (eql 0 (string-match "amd64" system-configuration))) 'x86_64)
        ((eql 0 (string-match "arm" system-configuration)) 'arm)))

(defun get-operating-system-type ()
  (cond ((string-match "freebsd" system-configuration) 'freebsd)
        (t system-type)))

(defun get-clozure-exe-name ()
  "Get the Clozure Common Lisp executable name"
  (interactive)
  (case (get-arch)
    ('x86
     (case system-type
       ('freebsd "fx86cl")
       ('gnu/linux "lx86cl")
       ('darwin "dx86cl")
       ('windows-nt "wx86cl64.exe")))
    ('x86_64
     (case (get-operating-system-type)
       ('freebsd "fx86cl64")
       ('gnu/linux "lx86cl64")
       ('darwin "dx86cl64")
       ('windows-nt "wx86cl64.exe")))
    ('arm
     (case system-type
       ('gnu/linux "armcl")))))


;; PACKAGES -----------------------------------------------------------
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;; LOAD ---------------------------------------------------------------
;; Specify where to load additional Emacs Lisp files.
(add-to-list 'load-path (directory-file-name (file-name-directory load-file-name)))

;; Have the Emacs Custom settings go to a separate file
(setq custom-file (concat (file-name-directory load-file-name) "custom-settings.el"))
(load custom-file 'noerror)


;; GENERAL ------------------------------------------------------------
;; Turns on syntax highlighting.
(global-font-lock-mode t)

;; Sets the syntax highlighting to the maximum amount.
(setq font-lock-maximum-decoration t)

;; Do not make ~ backup files
(setq make-backup-files nil)

;; Don't ask to follow a symlink to a version-controlled file.
(setq vc-follow-symlinks t)

;; When the cursor goes off screen, scroll only by 1 line
(setq scroll-step 1)

;; Don't speed up the scrolling based on how fast the mouse is scrolling
(setq mouse-wheel-progressive-speed nil)

;; Scroll the buffer by 3 lines at a time
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control))))

;; Use Shift + the arrow keys to move between windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Make directory upon save if it doesn't exist
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))


;; IDO ----------------------------------------------------------------
(require 'ido-hacks)
(require 'ido)
(ido-mode t)
(setq ido-everwhere t)
(setq ido-enable-flex-matching t)
(ido-everywhere)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(ido-hacks-mode)


;; SPELL CHECKING -----------------------------------------------------
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))


;; VISUAL -------------------------------------------------------------
;; Global colors and default font
(add-to-list 'default-frame-alist '(width . 80))
(add-to-list 'default-frame-alist '(height . 38))
(add-to-list 'default-frame-alist '(foreground-color . "cyan"))
(add-to-list 'default-frame-alist '(background-color . "black"))
(let ((programming-font (or (font-exists-p "-adobe-Source Code Pro-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
                            (font-exists-p "-outline-Source Code Pro-normal-normal-normal-mono-12-*-*-*-c-*-iso8859-1")
                            (font-exists-p "-outline-Lucida Console-normal-normal-normal-mono-12-*-*-*-c-*-iso8859-1")
                            (font-exists-p "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1"))))
  (when programming-font
    (add-to-list 'default-frame-alist
                 `(font           .      ,programming-font))))

;; Disable startup screens/messages
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Use a visible bell
(setq visible-bell t)

;; Line numbers on the left
(when (and window-system
           (fboundp 'global-linum-mode))
  (global-linum-mode))


;; SEMANTIC -----------------------------------------------------------
(when (or (> emacs-major-version 23)
          (and (= emacs-major-version 23)
               (>= emacs-minor-version 2)))
  (semantic-mode 1)
  (global-semantic-idle-completions-mode 1)
  (setq semantic-complete-inline-analyzer-displayor-class 'semantic-displayor-tooltip)
  (setq semantic-complete-inline-analyzer-idle-displayor-class 'semantic-displayor-ghost)
  (add-hook 'speedbar-load-hook (lambda () (require 'semantic/sb))))


;; Compilation --------------------------------------------------------
(setq compilation-ask-about-save nil)
(setq compilation-read-command nil)


;; PYTHON -------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)SConstruct\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)SConscript\\'" . python-mode))


;; LUA ----------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)


;; Batch files --------------------------------------------------------
(autoload 'dos-mode "dos" "DOS batch file editing mode." t)
(add-to-list 'auto-mode-alist '("\\.bat\\'" . dos-mode))


;; C/C++ --------------------------------------------------------------
(c-add-style "allman"
	     '("bsd"
	       (c-basic-offset . 4)))
(setq c-default-style "allman")
(add-hook 'c-mode-common-hook '(lambda () (setq tab-width 4)))
(add-to-list 'auto-mode-alist '("\\.mks\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.mkh\\'" . c-mode))


;; Google C/C++ style
;;(require 'google-c-style nil t)
;;(when (fboundp 'google-set-c-style)
;;  (add-hook 'c-mode-common-hook 'google-set-c-style))
;;(when (fboundp 'google-make-newline-indent)
;;  (add-hook 'c-mode-common-hook 'google-make-newline-indent))


;; GLSL ---------------------------------------------------------------
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))


;; HLSL --------------------------------------------------------------
(autoload 'hlsl-mode "hlsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.fx\\'"   . hlsl-mode))
(add-to-list 'auto-mode-alist '("\\.hlsl\\'" . hlsl-mode))
(add-to-list 'auto-mode-alist '("\\.usf\\'"  . hlsl-mode))


;; Lisp ---------------------------------------------------------------
(setq inferior-lisp-program (or "sbcl" (get-clozure-exe-name) inferior-lisp-program))
(let ((slime-helper-file-name (expand-file-name "~/.quicklisp/slime-helper.el")))
  (when (file-exists-p slime-helper-file-name)
    (load slime-helper-file-name)))


;; MICROSOFT WINDOWS --------------------------------------------------
(when (equal system-type 'windows-nt)
  (autoload 'csharp-mode "csharp-mode" "C Sharp editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))
  (autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.vbs\\'" . visual-basic-mode))
  ;; Don't insert a default directory into the minibuffer for C-x C-f
  (setq insert-default-directory nil))


;; KEYBINDINGS --------------------------------------------------------
(global-set-key [f8] 'speedbar-get-focus)
(global-set-key [f7] 'switch-speedbar-between-files-and-buffers)


;; EMACS SERVER --------------------------------------------------------
(require 'server)
(when (equal system-type 'windows-nt)
  (let ((server-directory (expand-file-name "~/.emacs.d/server")))
    (unless (file-exists-p server-directory)
      (display-message-or-buffer (format "Creating Emacs server directory at %S" server-directory))
      (make-directory server-directory t)))
  ;; Suppress error "directory ~/.emacs.d/server is unsafe on MS Windows
  (defun server-ensure-safe-dir (dir) "Noop" t))
(server-start)

