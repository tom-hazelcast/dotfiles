

;;; init.el --- Andrew Kensler's init.el File For GNU Emacs

;; Copyright (C) 1997-2014  Andrew Kensler

;; Author: Andrew Kensler
;; Version: 20140329
;; Time-stamp: <2014-03-29 15:15:19 aek>
;; Keywords: local, convenience

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This is my personal startup file for GNU Emacs.  It has only recently
;; been tested on GNU Emacs 24.3.1, though it may run on other versions.

;;; Code:

;; ===========================================================================
;; Simple Settings and Internal Packages
;; ===========================================================================

;; Miscellaneous settings
;; ----------------------
;;

(global-set-key (kbd "\C-x b") 'eval-buffer)
(global-set-key (kbd "\C-c t") 'beginning-of-buffer)
(global-set-key (kbd "\C-c e") 'end-of-buffer)
;;(global-set-key (kbd "\C-x s") 'shell)

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
;;(key-chord-define-global "JJ" 'switch-to-previous-buffer)
(global-set-key (kbd "C-x p") 'switch-to-previous-buffer)

(setq-default inhibit-startup-screen t                        ; Skip the startup screens
              initial-scratch-message nil
              confirm-kill-emacs 'y-or-n-p                    ; Disallow accidental exits
              blink-cursor-alist '((t . hollow))              ; Cursor blinks solid and hollow
              frame-title-format '(buffer-file-name "%f" "%b") ; I know it's Emacs; show something useful
              mode-line-format '(" %+ "                       ; Simplify the mode line
                                 (:propertize "%b" face mode-line-buffer-id)
                                 ":%l:%c %[" mode-name "%]"
                                 (-2 "%n")
                                 (visual-line-mode " W")
                                 (auto-fill-function " F")
                                 (overwrite-mode " O"))
              max-mini-window-height 1                        ; Bouncy minibuffer/echo gets obnoxious
              truncate-lines t                                ; Truncate lines, don't wrap
              default-truncate-lines t
              vc-make-backup-files t                          ; Keep numbered backups in ~/.saves
              version-control t
              delete-old-versions t
              kept-new-versions 4
              kept-old-versions 0
              backup-directory-alist '((".*" . "~/.saves/"))
              auto-save-list-file-prefix nil
              auto-save-file-name-transforms '((".*" "~/.saves/" t))
              font-lock-use-fonts '(or (mono) (grayscale))    ; Maximal syntax hilighting
              font-lock-use-colors '(color)
              font-lock-maximum-decoration t
              font-lock-maximum-size nil
              font-lock-auto-fontify t
              show-paren-style 'expression                    ; Highlight parenthesis
              comment-empty-lines t                           ; Prefix empty lines too
              show-trailing-whitespace t                      ; Show trailing whitespace
              use-dialog-box nil                              ; Always use the minibuffer for prompts
              user-full-name "Andrew Kensler"
              user-mail-address "***@*****.***"
              query-user-mail-address nil
              display-warning-minimum-level 'error            ; Turn off annoying warning messages
              disabled-command-function nil                   ; Don't second-guess advanced commands
              delete-key-deletes-forward t                    ; Make delete key work normally
              kill-read-only-ok t                             ; Silently copy in read-only buffers
              column-number-mode t                            ; Display line and column numbers
              line-number-mode t
              tab-width 4                                     ; Set tab stops
              tab-stop-list (number-sequence 4 120 4)
              indent-tabs-mode nil                            ; Use spaces only, no tabs
              tabify-regexp "^\t* [ \t]+"                     ; Tabify only initial whitespace
              page-delimiter "^\\s *\n"                       ; Page delim is one or more blank lines
              minibuffer-max-depth nil                        ; Mini-buffer settings
              toolbar-print-function 'ps-print-buffer-with-faces ; Set the print button to print nice PS
              ps-line-number t
              ps-n-up-printing 2
              ps-print-color-p nil
              fill-column 75                                  ; Wrap lines at 75th column
              initial-major-mode 'text-mode                   ; Text mode, not Elisp
              case-fold-search t                              ; Fold case on searches
              buffers-menu-sort-function 'sort-buffers-menu-by-mode-then-alphabetically ; Buffers menu settings
              buffers-menu-grouping-function 'group-buffers-menu-by-mode-then-alphabetically
              buffers-menu-submenus-for-groups-p t
              ibuffer-default-sorting-mode 'filename/process  ; Group buffers primarily by directory
              uniquify-buffer-name-style 'forward             ; Prepend unique dirs when names clash
              uniquify-after-kill-buffer-p t
              uniquify-ignore-buffers-re "^\\*"
              major-mode 'major-mode-from-name                ; Set mode for empty buffers by name
              ispell-program-name "aspell"                    ; Use aspell to spell check
              gdb-many-windows t                              ; Show GUD in all its glory
              ediff-split-window-function 'split-window-horizontally ; Show diffs side-by-side
              diff-switches "-u"                              ; Prefer unified over context diffs
              org-support-shift-select t                      ; Don't nuke shift selection in org-mode
              calc-display-sci-low -5                         ; More zeros before scientific notation
              )
(defun startup-echo-area-message ()                           ; Use a more interesting startup message
  "By your command...")
(set-frame-parameter nil 'alpha 99)                           ; Make the window 90% opaque
(set-scroll-bar-mode 'right)                                  ; Scrollbars should always be on the right
(set-fringe-mode '(1 . 0))                                    ; Turn off the left fringe
(defun major-mode-from-name ()
  "Choose proper mode for buffers created by switch-to-buffer."
  (let ((buffer-file-name (or buffer-file-name (buffer-name))))
    (set-auto-mode)))
(show-paren-mode t)                                           ; Highlight whole parenthetic expressions
(delete-selection-mode t)                                     ; Typed text replaces selection
(global-subword-mode t)                                       ; Treat CamelCase as multiple words
(auto-fill-mode t)                                            ; Automatically wrap lines
(fset 'yes-or-no-p 'y-or-n-p)                                 ; Yes or no prompts accept short y or n
(require 'uniquify)                                           ; Actually do buffer renaming on clashes
(if (featurep 'mswindows)                                     ; If we're on Windows, write files with CR/LF
    (set-default-buffer-file-coding-system 'raw-text-dos))
(require 'server)                                             ; Allow clients to attach to the first session
(if (not (server-running-p))
    (server-start))

;; Font lock customization
;; -----------------------
;; This is an attempt to provide a pleasing, sane and reasonably consistent
;; color scheme across GNU Emacs, XEmacs, Windows, X, and TTY systems.  It
;; also highlights fixme type tags.
;;
(require 'font-lock)
(global-font-lock-mode t)
(add-hook 'font-lock-mode-hook 'font-lock-mode-setup)
(defun font-lock-mode-setup ()
  "Make fixme tags standout."
  (font-lock-add-keywords nil
   '(("\\<\\(AEK\\|NOTE\\|FIXME\\|TODO\\|XXX\\)\\>:?" 0 'font-lock-warning-face prepend))))
(defun set-colour-theme (theme)
  "Helper function to set a bunch of faces and ignore potential errors from missing faces."
  (mapc (lambda (setting)
          (condition-case nil
              (face-spec-set (car setting) (cdr setting))
            (error t)))
        theme))
(defun light-on-dark-theme ()
  "Setup the colors for a light-on-dark theme."
  (interactive)
  (set-colour-theme
   '((default . ((((type x w32)) (:background "white" :foreground "darkgrey")) (t (:background "unspecified-bg" :foreground "grey"))))
     (cursor . ((t (:background "plum"))))
     (mode-line . ((t (:foreground "grey5" :background "grey75" :box (:style released-button)))))
     (mode-line-inactive . ((t (:foreground "grey80" :background "grey30" :box (:color "grey50" :line-width 1)))))
     (minibuffer-prompt . ((t (:foreground "deepskyblue" :bold t))))
     (font-lock-keyword-face . ((t (:foreground "darkgreen" :bold t))))
     (font-lock-comment-face . ((t (:foreground "steelblue" :italic t))))
     (font-lock-string-face . ((t (:foreground "lightgreen"))))
     (font-lock-doc-face . ((t (:foreground "deepskyblue" :italic t))))
     (font-lock-function-name-face . ((t (:foreground "coral" :bold t))))
     (font-lock-type-face . ((t (:foreground "steelblue" :bold t))))
     (font-lock-variable-name-face . ((t (:foreground "orchid"))))
     (font-lock-reference-face . ((t (:foreground "coral"))))
     (font-lock-builtin-face . ((t (:foreground "coral"))))
     (font-lock-constant-face . ((t (:foreground "coral"))))
     (font-lock-warning-face . ((t (:foreground "darkorange" :bold t))))
     (error . ((t (:foreground "red"))))
     (paren-match . ((t (:background "midnightblue"))))
     (show-paren-match-face . ((t (:background "midnightblue"))))
     (zmacs-region . ((t (:foreground "grey5" :background "lightcoral"))))
     (region . ((t (:foreground "grey5" :background "lightcoral"))))
     (isearch . ((t (:foreground "white" :background "red"))))
     (isearch-secondary . ((t (:foreground "red3" :background "grey"))))
     (isearch-lazy-highlight-face . ((t (:foreground "red3" :background "grey"))))
     (trailing-whitespace . ((t (:background "grey15"))))
     (shadow . ((t (:foreground "grey40"))))
     (link . ((t (:foreground "deepskyblue"))))
     (visited-link . ((t (:foreground "orchid"))))
     (highlight . ((t (:foreground "white" :background "gray15" :underline t))))
     (flymake-errline . ((t (:background nil :underline "red"))))
     (flymake-warnline . ((t (:background nil :underline "orange"))))
     (diff-removed . ((t (:foreground "coral"))))
     (diff-added . ((t (:foreground "lightgreen"))))
     (diff-refine-removed . ((t (:inherit diff-removed :foreground "sienna" :background nil :strike-through t))))
     (diff-refine-added . ((t (:inherit diff-added :foreground "palegreen" :background nil :bold t))))
     (ediff-even-diff-A . ((t (:foreground nil :background "grey15"))))
     (ediff-odd-diff-A . ((t (:foreground nil :background "grey15"))))
     (ediff-even-diff-B . ((t (:foreground nil :background "grey15"))))
     (ediff-odd-diff-B . ((t (:foreground nil :background "grey15"))))
     (ediff-current-diff-A . ((t (:foreground "coral" :bold nil :italic nil))))
     (ediff-current-diff-B . ((t (:foreground "lightgreen" :bold nil :italic nil))))
     (ediff-fine-diff-A . ((t (:foreground "sienna" :background nil :strike-through t))))
     (ediff-fine-diff-B . ((t (:foreground "palegreen" :background nil :bold t))))
     (outline-1 . ((t (:foreground "coral" :bold t))))
     (outline-2 . ((t (:foreground "orchid" :bold t))))
     (outline-3 . ((t (:foreground "steelblue" :bold t))))
     (outline-4 . ((t (:foreground "steelblue" :italic t))))
     (rst-level-1 . ((t (:foreground "coral" :background nil :bold t))))
     (rst-level-2 . ((t (:foreground "orchid" :background nil :bold t))))
     (rst-level-3 . ((t (:foreground "steelblue" :background nil :bold t))))
     (rst-level-4 . ((t (:foreground "steelblue" :background nil :italic t))))
     (rst-adornment-face . ((t (:foreground "grey40"))))
     (org-code . ((t (:foreground "lightgreen"))))
     (org-verbatim . ((t (:foreground "lightgreen")))))))
(defun dark-on-light-theme ()
  "Setup the colors for a dark-on-light theme."
  (interactive)
  (set-colour-theme
   '((default . ((t (:background "grey100" :foreground "grey5"))))
     (cursor . ((t (:background "red"))))
     (mode-line . ((t (:foreground "grey5" :background "grey70" :box (:style released-button)))))
     (mode-line-inactive . ((t (:foreground "grey45" :background "grey85" :box (:color "grey75" :line-width 1)))))
     (minibuffer-prompt . ((t (:foreground "deepskyblue4" :bold t))))
     (font-lock-keyword-face . ((t (:foreground "grey5" :bold t))))
     (font-lock-comment-face . ((t (:foreground "blue" :italic t :underline nil))))
     (font-lock-string-face . ((t (:foreground "green4"))))
     (font-lock-doc-face . ((t (:foreground "deepskyblue4" :italic t))))
     (font-lock-function-name-face . ((t (:foreground "red" :bold t))))
     (font-lock-type-face . ((t (:foreground "steelblue" :bold t))))
     (font-lock-variable-name-face . ((t (:foreground "magenta"))))
     (font-lock-reference-face . ((t (:foreground "red3"))))
     (font-lock-builtin-face . ((t (:foreground "red3"))))
     (font-lock-constant-face . ((t (:foreground "red3"))))
     (font-lock-warning-face . ((t (:foreground "darkorange" :bold t))))
     (error . ((t (:foreground "red"))))
     (paren-match . ((t (:background "lightsteelblue"))))
     (show-paren-match-face . ((t (:background "lightsteelblue"))))
     (zmacs-region . ((t (:background "lightcoral"))))
     (region . ((t (:background "lightcoral"))))
     (isearch . ((t (:foreground "white" :background "red"))))
     (isearch-secondary . ((t (:foreground "red3" :background "grey"))))
     (isearch-lazy-highlight-face . ((t (:foreground "red3" :background "grey"))))
     (trailing-whitespace . ((t (:background "mistyrose"))))
     (shadow . ((t (:foreground "grey60"))))
     (link . ((t (:foreground "deepskyblue4"))))
     (visited-link . ((t (:foreground "magenta"))))
     (highlight . ((t (:foreground "black" :background "mistyrose" :underline t))))
     (flymake-errline . ((t (:background nil :underline "red"))))
     (flymake-warnline . ((t (:background nil :underline "orange"))))
     (diff-removed . ((t (:foreground "red3"))))
     (diff-added . ((t (:foreground "green4"))))
     (diff-refine-removed . ((t (:inherit diff-removed :foreground "lightcoral" :background nil :strike-through t))))
     (diff-refine-added . ((t (:inherit diff-added :foreground "darkgreen" :background nil :bold t))))
     (ediff-even-diff-A . ((t (:foreground nil :background "grey85"))))
     (ediff-odd-diff-A . ((t (:foreground nil :background "grey85"))))
     (ediff-even-diff-B . ((t (:foreground nil :background "grey85"))))
     (ediff-odd-diff-B . ((t (:foreground nil :background "grey85"))))
     (ediff-current-diff-A . ((t (:foreground "red3" :bold nil :italic nil))))
     (ediff-current-diff-B . ((t (:foreground "green4" :bold nil :italic nil))))
     (ediff-fine-diff-A . ((t (:foreground "lightcoral" :background nil :strike-through t))))
     (ediff-fine-diff-B . ((t (:foreground "darkgreen" :background nil :bold t))))
     (outline-1 . ((t (:foreground "red" :bold t))))
     (outline-2 . ((t (:foreground "magenta" :bold t))))
     (outline-3 . ((t (:foreground "steelblue" :bold t))))
     (outline-4 . ((t (:foreground "steelblue" :italic t))))
     (rst-level-1 . ((t (:foreground "red" :background nil :bold t))))
     (rst-level-2 . ((t (:foreground "magenta" :background nil :bold t))))
     (rst-level-3 . ((t (:foreground "steelblue" :background nil :bold t))))
     (rst-level-4 . ((t (:foreground "steelblue" :background nil :italic t))))
     (rst-adornment-face . ((t (:foreground "grey60"))))
     (org-code . ((t (:foreground "green4"))))
     (org-verbatim . ((t (:foreground "green4")))))))
(light-on-dark-theme)

;; Interactively do things
;; -----------------------
;; I love using ido-mode in Emacs.  The first part here automatically adds
;; newly opened files to the ido-mode cache and history.  This way, if we
;; open a file from the prompt via emacsclient or from a tag lookup we can
;; easily get back to it later via ido-mode.  The second part here is based
;; on bits from Emacswiki that extend ido-mode to most other places that
;; take input from the minibuffer.  This is awesome with tags and help!
;;
(require 'ido)
(ido-mode 1)
(add-hook 'find-file-hook 'ido-remember-buffer-file)
(defun ido-remember-buffer-file ()
  "Add this buffers file to the ido cache and history."
  (interactive)
  (let ((file (expand-file-name (buffer-file-name))))
    (if file
        (let ((dir (file-name-directory file))
              (name (file-name-nondirectory file)))
          (ido-record-work-file name)
          (ido-record-work-directory dir)
          (ido-file-name-all-completions dir)))))
(ido-everywhere 1)
(defadvice completing-read
  (around use-ido-when-possible activate)
  (if (not (boundp 'ido-cur-list))
      (let ((completions (all-completions "" collection predicate)))
        (if completions
            (setq ad-return-value
                  (ido-completing-read prompt completions nil require-match
                                       initial-input hist def)))))
  (unless ad-return-value
    ad-do-it))
(define-key global-map [(meta ?x)] 'ido-meta-x)
(defun ido-meta-x ()
  "Replacement for standard M-x that use ido."
  (interactive)
  (call-interactively
   (intern
    (or (completing-read "M-x " (all-completions "" obarray 'commandp))))))

;; Isearch
;; -------
;; Temporarily turn on which-function-mode during an interactive search and
;; make sure it shows up in our custom modeline.
;;
(defvar saved-which-function)
(add-hook 'isearch-mode-hook 'isearch-mode-start-which-func)
(defun isearch-mode-start-which-func ()
  "Start which-func mode and add it to the mode line."
  (setq saved-which-function which-function-mode)
  (nconc mode-line-format '((which-func-mode
                             (:propertize (" " which-func-current "()")
                                          face mode-line-emphasis))))
  (which-function-mode))
(add-hook 'isearch-mode-end-hook 'isearch-mode-end-which-func)
(defun isearch-mode-end-which-func ()
  "Stop which-func mode and clear it from the mode line."
  (which-function-mode (if saved-which-function 1 -1))
  (nbutlast mode-line-format))

;; Common settings
;; ---------------
;; Basic, fundamental text mode settings.
;;
(add-hook 'text-mode-hook 'common-mode-setup)
(add-hook 'prog-mode-hook 'common-mode-setup)
(defun common-mode-setup ()
  "Automatically fill text by default."
  (auto-fill-mode t))

;; C-style modes
;; -------------
;; C mode is nice, but I have a particular style that I like.  This tries
;; to be a bit smarter about automatically inserting newlines around braces
;; for enums and single- and multi-line initializer lists.  It also sets up
;; highlighting for Doxygen style comments.
;;
(defun c-brace-open (syntax pos)
  (save-excursion
    (let ((start (c-point 'bol))
          langelem)
      (if (and (eq syntax 'brace-list-open)
               (setq langelem (assq 'brace-list-open c-syntactic-context))
               (progn (goto-char (c-langelem-pos langelem))
                      (if (eq (char-after) ?{)
                          (c-safe (c-forward-sexp -1)))
                      (looking-at "\\<enum\\>[^_]")))
          '(before after)
        (if (< (point) start)
            '(after))))))
(defun c-brace-close (syntax pos)
  (save-excursion
    (goto-char pos)
    (if (> (c-point 'bol)
           (progn (up-list -1) (point)))
        '(before))))
(defconst doxygen-font-lock-doc-comments
  `(("\\s-\\([\\@].*?\\)\\s-"
     1 font-lock-constant-face prepend nil)                     ; ,c-doc-markup-face-name
    ("\\[in\\]\\|\\[out\\]\\|\\[in,out\\]"
     0 font-lock-constant-face prepend nil)
    ("\\<\\(?:[a-zA-Z_][a-zA-Z0-9_]*::\\)*[a-zA-Z_][a-zA-Z0-9_]*()"
     0 font-lock-constant-face prepend nil)))
(defconst doxygen-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "/\\*[*!]<?" limit
          doxygen-font-lock-doc-comments)
        (c-font-lock-doc-comments "//[/!]<?" limit
          doxygen-font-lock-doc-comments)))))
(c-add-style "aek"
             '((c-doc-comment-style . doxygen)
               (c-basic-offset . 4)
               (c-comment-only-line-offset . 0)
               (c-hanging-braces-alist . ((substatement-open before after)
                                          (brace-list-open . c-brace-open)
                                          (brace-list-close . c-brace-close)
                                          (class-close before))) ; Semicolon, not newline after brace
               (c-hanging-semi&comma-criteria . (c-semi&comma-no-newlines-before-nonblanks
                                                 c-semi&comma-inside-parenlist))
               (c-offsets-alist . ((topmost-intro     . 0)
                                   (substatement      . +)
                                   (substatement-open . 0)
                                   (case-label        . +)
                                   (access-label      . -)
                                   (inclass           . +)
                                   (inline-open       . 0)
                                   (brace-list-open   . 0)
                                   (brace-list-close  . 0)))))
(add-to-list 'c-default-style '(c-mode . "aek"))              ; Use new style in C and C++ modes
(add-to-list 'c-default-style '(c++-mode . "aek"))
(add-hook 'c-mode-common-hook 'c-mode-common-setup)
(defun c-mode-common-setup ()
  "Turn on auto-newline and hungry-delete."
  (c-toggle-auto-hungry-state 1))

;; ReST mode
;; ---------
;; Use a toned-down face for the section adornments so we can color code
;; the headings without them being overwhelmed.
;;
(add-hook 'rst-mode-hook 'rst-mode-setup)
(defun rst-mode-setup ()
  "Tweak the faces for editing ReST documents."
  (setq rst-adornment-faces-alist (assq-delete-all nil rst-adornment-faces-alist))
  (setq rst-adornment-faces-alist (assq-delete-all t rst-adornment-faces-alist))
  (add-to-list 'rst-adornment-faces-alist '(nil . rst-adornment-face))
  (add-to-list 'rst-adornment-faces-alist '(t . rst-adornment-face)))

;; ============================================================================
;; External Packages
;; ============================================================================

(add-to-list 'load-path "~/.emacs.d/elisp")                        ; Append the .emacs.d dir to the path

;; Notes-mode
;; ----------
;; I wrote this mode to make it easier to write up notes in a heirarchical
;; outline format.  Basically, it's to make it easier to edit my style of
;; notes.
;;
(autoload 'notes-mode "notes-mode" "Major mode for editing outline structured notes." t)

;; Rib-mode
;; --------
;; My version of a mode for editing RenderMan Interface Bytestream (RIB)
;; files in ASCII form.
;;
(autoload 'rib-mode "rib-mode" "Major mode for ASCII RenderMan Interface Bytestream files." t)
(add-to-list 'auto-mode-alist '("\\.rib\\'" . rib-mode))

;; Show-word
;; ---------
;; I saw another editor that would highlight matching instances of the word
;; under the cursor whenever it came to rest and thought that was kind of
;; neat and useful.  This is my reimplementation of that for Emacs.
;;
;(require 'show-word)
;(show-word-mode 1)

;; File-tree
;; ---------
;; I wrote this to be a handy light-weight display of the file system tree,
;; a bit like the Speedbar but better behaved on a TTY.
;;
(autoload 'file-tree "file-tree" "Tree display of files for easy navigation." t)

;; Lua-mode
;; --------
;; Lua is a neat little language.  Unfortunately, not all Emacs
;; distributions include lua-mode.
;;
(autoload 'lua-mode "lua-mode" "Major mode for editing Lua code." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

;; YASsnippet
;; ---------
;; Yasnippet is a fairly slick new template mechanism.  It supports
;; intelligent mirrors, execution of arbitrary Emacs lisp code, reasonable
;; indenting and such.  The following is a mix of brand new snippets with
;; customized versions of a subset of the snippets that it comes bundled
;; with.  Unfortunately, it currently only works on GNU Emacs.
;;
(when (require 'yasnippet nil t)
  (yas/load-directory "~/.emacs.d/snippets")
  (yas/global-mode 1))

;; Gtags-mode
;; ----------
;; GNU GLOBAL is a bit of an upgrade over the basic etags and ctags
;; functionality.  It's smarter about handling multiple definitions and
;; lets you do need things like middle click to jump immediately.
;;
(autoload 'gtags-mode "gtags" "Toggle Gtags mode, a minor mode for browsing source code using GLOBAL." t)
(add-hook 'gtags-mode-hook 'gtags-mode-setup)
(defun gtags-mode-setup ()
  "Adjust the mouse bindings for Gtags."
  (define-key gtags-mode-map [(shift mouse-2)] 'gtags-find-tag-by-event)
  (define-key gtags-select-mode-map [(shift mouse-2)] 'gtags-select-tag-by-event)
  (define-key gtags-select-mode-map [mouse-2] 'gtags-select-tag-by-event))

;; HTMLize
;; -------
;; XEmacs includes this by default, but not GNU Emacs.
;;
(autoload 'htmlize-buffer "htmlize" "Convert buffer to HTML, preserving colors and decorations." t)

;; ============================================================================
;; Bindings and Interactive Functions
;; ============================================================================

;; Extra key bindings
;; ------------------
;;
(define-key global-map [(control ?z)] nil)                    ; Appropriate C-z for a personal prefix key
(define-key global-map [(control ?z) ?a] 'align-current)      ; Note: try space before first entry then C-u C-z a
(define-key global-map [(control ?z) ?l] 'sort-lines)
(define-key global-map [(control ?z) ?t] 'delete-trailing-whitespace)
(define-key global-map [(control ?z) tab] 'bury-buffer)
(define-key global-map [(control tab)] 'bury-buffer)
(define-key global-map [(meta ?g)] 'goto-line)                ; Use XEmacs' M-g for goto-line in GNU too
(define-key global-map [(control ?h) ?a] 'apropos)            ; Apropos *all* symbols, not just commands
(define-key global-map [(control ?x) (control ?b)] 'ibuffer)  ; Why use buffer-menu when there's IBuffer?
(define-key global-map [(help)] 'overwrite-mode)              ; Some Macs have <help> where <insert> should be
(define-key global-map [(shift help)] 'yank)
(define-key global-map [(control help)] 'copy-region-as-kill)
(define-key global-map [(f13)] 'overwrite-mode)               ; Newer Macs use <fn> which can't be bound. Ugh!!
(define-key global-map [(shift f13)] 'yank)
(define-key global-map [(control f13)] 'copy-region-as-kill)

;; Move-beginning-of-line-dwim
;; ---------------------------
;;
(define-key global-map [remap move-beginning-of-line] 'move-beginning-of-line-dwim)
(require 'newcomment)
(defun move-beginning-of-line-dwim ()
  "Cycles between the beginning of text on the line, the actual
beginning of the line, and the beginning of the body of a comment
on the line.  If not in one of those three places, it starts at
the beginning of the text.  Basically, it's a smarter HOME
command."
  (interactive "^")
  (unless (and (bolp) (comment-search-forward (point-at-eol) t))
    (let ((cur (point)))
      (beginning-of-line-text)
      (if (= cur (point))
          (beginning-of-line)))))

;; Delete-forward-char-dwim
;; ------------------------
;;
(define-key global-map [remap delete-forward-char] 'delete-forward-char-dwim)
(define-key global-map [remap delete-char] 'delete-forward-char-dwim) ; Emacs 23
(defun delete-forward-char-dwim (n &optional killflag)
  "Acts like normal DELETE except when at the end of a line, in
which case it tries to intelligently joins the next line to the
end of the current one."
  (interactive "p\nP")
  (if (or (not (eolp))
          (bolp)
          (and (use-region-p) delete-active-region (= n 1)))
      (if (fboundp 'delete-forward-char)
          (delete-forward-char n killflag)
        (delete-char n killflag))
    (let ((start (point)))
      (forward-char)
      (skip-chars-forward " \t")
      (if (looking-at (or (and (listp c-comment-prefix-regexp)
                               (or (assoc-default major-mode c-comment-prefix-regexp)
                                   (assoc-default 'other c-comment-prefix-regexp)))
                          comment-start-skip))
          (goto-char (match-end 0)))
      (skip-chars-forward " \t")
      (if (and (/= 0 (logand (car (syntax-after (point))) (lsh 1 19)))
               (/= 0 (logand (car (syntax-after (1- (point)))) (lsh 1 18))))
          (backward-char))
      (delete-region start (point))
      (when (and (not (eolp)) (/= (char-before) ? ) (/= (char-before) ?\t))
        (insert-char ?  1)
        (backward-char)))))
(put 'delete-forward-char-dwim 'delete-selection 'supersede) ; Make delete-selection-mode work with it

;; Kill-region-dwim
;; ----------------
;;
(define-key global-map [remap kill-region] 'kill-region-dwim)
(defun kill-region-dwim ()
  "Behaves much like the standard kill-region, but if there's
no region active then it simply kills the current line."
  (interactive)
  (let ((x-select-enable-clipboard t))
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (kill-region (line-beginning-position) (line-beginning-position 2)))))

;; Newline-dwim
;; ------------
;;
(define-key global-map [remap newline] 'newline-dwim)
(defun newline-dwim ()
  "Like the standard newline function, but if the newline is
added after a line that is empty except for whitespace the
whitespace will be cleared.  This is useful for clearing the
space that automatic indentation typically inserts in
anticipation of a non-blank line."
  (interactive)
  (let ((start (point-at-bol)))
    (newline)
    (save-excursion
      (goto-char start)
      (if (looking-at "[ \t]+$")
          (delete-region start (point-at-eol))))))

;; Case-dwim
;; ---------
;; Often I just want to change the case throughout an entire region rather
;; than a word at a time.
;;
(define-key global-map [remap upcase-word] 'upcase-dwim)
(define-key global-map [remap downcase-word] 'downcase-dwim)
(define-key global-map [remap capitalize-word] 'capitalize-dwim)
(define-key subword-mode-map [remap upcase-word] nil)       ; Unbind subword remaps
(define-key subword-mode-map [remap downcase-word] nil)
(define-key subword-mode-map [remap capitalize-word] nil)
(defun upcase-dwim (arg)
  "Upcase the region if active, otherwise the current subword or word."
  (interactive "p")
  (cond ((use-region-p) (upcase-region (region-beginning) (region-end)))
         (subword-mode (subword-upcase arg))
         (t (upcase-word arg))))
(defun downcase-dwim (arg)
  "Downcase the region if active, otherwise the current subword or word."
  (interactive "p")
  (cond ((use-region-p) (downcase-region (region-beginning) (region-end)))
        (subword-mode (subword-downcase arg))
        (t (downcase-word arg))))
(defun capitalize-dwim (arg)
  "Capitalize the region if active, otherwise the current subword or word."
  (interactive "p")
  (cond ((use-region-p) (capitalize-region (region-beginning) (region-end)))
        (subword-mode (subword-capitalize arg))
        (t (capitalize-word arg))))

;; Subword-left/right
;; ------------------
;; I'm suprised these aren't already part of subword-mode!
;;
(define-key global-map [remap left-word] 'subword-left)
(define-key global-map [remap right-word] 'subword-right)
(defun subword-left (&optional n)
  "Move point N subwords to the left (to the right if N is
negative).  This behaves just like left-word, but uses subword
motion instead of regular word motion."
  (interactive "^p")
  (if (eq (current-bidi-paragraph-direction) 'left-to-right)
      (subword-backward n)
    (subword-forward n)))
(defun subword-right (&optional n)
  "Move point N subwords to the right (to the left if N is
negative).  This behaves just like right-word, but uses subword
motion instead of regular word motion."
  (interactive "^p")
  (if (eq (current-bidi-paragraph-direction) 'left-to-right)
      (subword-forward n)
    (subword-backward n)))

;; Shift-lines-vertically
;; ----------------------
;;
(define-key global-map [(meta up)] 'shift-lines-up)         ; Bind to M-up
(define-key global-map [(meta down)] 'shift-lines-down)     ; Bind to M-down
(defun shift-lines-vertically (n)
  "Moves the lines containing the current region n lines
forward (backward if n is negative).  Mark, point and activation
status are preserved so that this may be called repeatedly.  If
the region is inactive, moves the line that point is currently
on.  This like a more intuitive transpose-lines or a visual
kill/yank for short distances."
  (interactive "p")
  (when (= n 0)
    (error "Nonzero line offset needed"))
  (let ((deactivate-mark nil)
        (original (point))
        (begin1 (if (region-active-p) (region-beginning) (point)))
        (end1 (if (region-active-p) (region-end) (point)))
        (begin2 nil)
        (end2 nil))
    (goto-char begin1)
    (forward-line 0)
    (setq begin1 (point))
    (when (< n 0)
      (setq end2 (point))
      (forward-line n)
      (setq begin2 (point)))
    (goto-char end1)
    (unless (and (bolp) (/= end1 begin1))
      (forward-line 1))
    (when (> n 0)
      (setq end1 (point))
      (setq begin2 (point))
      (forward-line n))
    (unless (= (char-before) ?\n)
      (insert-char ?\n 1))
    (cond ((> n 0)
           (setq end2 (point))
           (if (< original end1) (goto-char original)))
          ((< n 0)
           (setq end1 (point))
           (goto-char (if (= original end1) begin2 original))))
    (transpose-regions begin1 end1 begin2 end2)))
(defun shift-lines-up () (interactive) (shift-lines-vertically -1))
(defun shift-lines-down () (interactive) (shift-lines-vertically 1))

;; Shift-lines-horizontally
;; ------------------------
;;
(define-key global-map [(meta left)] 'shift-lines-left)     ; Bind to M-left
(define-key global-map [(meta right)] 'shift-lines-right)   ; Bind to M-right
(defun shift-lines-horizontally (n)
  "Move the text in the current region to the right or left by
changing indentation.  This adjusts everything to the right of
the point or mark (whichever is leftmost).  With a positive
argument it adds indentation.  With a negative argument it
removes it.  Mark, point and activation status are preserved so
that this may be called repeatedly."
  (interactive "p")
  (save-excursion
    (let* ((begin (if (use-region-p) (region-beginning) (point)))
           (end (if (use-region-p) (region-end) (point)))
           (column (min (progn (goto-char begin) (current-column))
                        (progn (goto-char end) (current-column)))))
      (goto-char begin)
      (setq begin (line-beginning-position))
      (goto-char end)
      (if (and (bolp) (not (bobp)) (/= (point) begin)) (backward-char))
      (while (and (prog1 t (if (= (move-to-column column 'coerce) column)
                               (if (> n 0)
                                   (unless (eolp) (insert-char ?\  n))
                                 (skip-chars-forward " \t" (line-end-position))
                                 (if (>= (current-column) (- column n))
                                     (move-to-column (- column n) 'coerce))
                                 (delete-region
                                  (point)
                                  (progn (move-to-column column) (point))))))
                  (= (forward-line -1) 0)
                  (>= (point) begin)))))
  (setq deactivate-mark nil))
(defun shift-lines-left () (interactive) (shift-lines-horizontally -1))
(defun shift-lines-right () (interactive) (shift-lines-horizontally 1))

;; Duplicate-as-comment
;; --------------------
;;
(define-key global-map [(control ?z) ?d] 'duplicate-as-comment)
(defun duplicate-as-comment ()
  "Creates a commented-out copy of the current region (or line
if no region is active). This is handy for testing out temporary
changes to a section of code while keeping the original version
as a comment for reference."
  (interactive)
  (let* ((begin (if (use-region-p) (region-beginning) (line-beginning-position)))
         (end (if (use-region-p) (region-end) (line-beginning-position 2)))
         (text (buffer-substring begin end)))
    (save-excursion
      (goto-char begin)
      (insert text))
    (if (equal (point) begin)
        (goto-char end))
    (comment-region begin end)))

;; Zap-up-to-char
;; --------------
;;
(define-key global-map [remap zap-to-char] 'zap-up-to-char)
(defun zap-up-to-char (arg char)
  "Like standard zap-to-char, but stops just before the given
character. Useful for killing to a delimeter that should be left
alone."
  (interactive "p\ncZap up to char: ")
  (kill-region (point)
               (progn
                 (search-forward (char-to-string char) nil nil arg)
                 (forward-char (if (>= arg 0) -1 1))
                 (point))))

;; Highlight-sloppy-grammar
;; ------------------------
;;
(defun highlight-sloppy-grammar ()
  "This uses the font lock mechanism to highlight some potential
grammatical trouble spots.  It checks against a small list of
common problems such as duplicate words and instances of the
passive voice.  It's not fool-proof but it does help when taking
a pass over a paper."
  (interactive)
  (make-face 'grammar-warning-face "Face to display grammar warnings in.")
  (face-spec-set 'grammar-warning-face
                 '((t (:bold t :foreground "orange" :underline t))))
  (font-lock-add-keywords nil
   '(("\\<\\(?:were\\|was\\|is\\|are\\|has been\\|be\\)\\(?:[ \t\r\n]+[a-zA-Z]+\\)?[ \t\r\n]+[a-zA-Z]+ed\\>"
      0 'grammar-warning-face t)
     ("\\<\\([a-zA-Z]+\\)[ \t\r\n]+\\1\\>" 0 'grammar-warning-face t)
     ("[,-][ \t\r\n]+that\\>" 0 'grammar-warning-face t)
     ("[a-zA-Z]+[ \t\r\n]+which\\>" 0 'grammar-warning-face t)
     ("\\<[a-z]+\\(?:n't\\|d've\\)\\>" 0 'grammar-warning-face t)
     ("\\<by[ \t\r\n]+[a-z]+ing\\>" 0 'grammar-warning-face t)
     ("\\<which[ \t\r\n]+was\\>" 0 'grammar-warning-face t)
     ("\\<the[ \t\r\n]+[a-zA-Z]+[ \t\r\n]+of[ \t\r\n]+the\\>" 0 'grammar-warning-face t)))
  (font-lock-fontify-buffer))

;; Goto-longest-line
;; -----------------
;;
(define-key global-map [(control ?z) ?g] 'goto-longest-line)
(defun goto-longest-line ()
  "Finds the longest line and puts the point there.  Sometimes
for code, it's nice to find lines that are pushed out too far.
This function moves point to the end of the longest line.  Also
handy for lining up columns of text when used in a narrowed
buffer."
  (interactive)
  (let ((width 0)
        (pos 0))
    (goto-char (point-min))
    (while (= (forward-line 1) 0)
      (end-of-line)
      (let ((curwid (current-column)))
        (unless (<= curwid width)
          (setq width curwid)
          (setq pos (point)))))
    (goto-char pos)))

;; Navigate-parens
;; ---------------
;;
(define-key global-map [(control ?z) ?p] 'navigate-parens)
(defun navigate-parens ()
  "This is a DWIM style function for navigating parenthesis. If
point is sitting on a parenthetic character, jump to its match.
If the region is active stop go right up to the closing
parenthesis.  Otherwise, go to the beginning of the enclosing
expression."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1))
        ((save-excursion (backward-char 1) (looking-at "\\s\)"))
         (backward-list 1))
        ((region-active-p) (up-list) (backward-char 1))
        (t (backward-up-list))))

;; Kill-other-buffers
;; ------------------
;; The original version of this was my first code in Emacs Lisp!  Back
;; then, I liked to minimize the number of open buffers.  These days, I'm
;; comfortable with hundreds of buffers thanks to ido-mode and ibuffers.
;;
(define-key global-map [(control ?z) ?k] 'kill-other-buffers)
(defun kill-other-buffers ()
  "Kill all buffers except the current and unsplit the window."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  (delete-other-windows)
  (delete-other-frames))

;; Uniq-lines
;; ----------
;;
(defun uniq-lines (start end)
  "Removes duplicate lines from the active region. This is
something of a companion to the built-in sort-lines function.
Like the uniq utility, it is best used after sorting a region."
  (interactive "*r")
  (goto-char start)
  (beginning-of-line)
  (let ((last ""))
    (while (< (point) end)
      (let* ((bol (point))
             (eol (progn (end-of-line) (point)))
             (text (buffer-substring bol eol)))
        (forward-char)
        (if (string= last text)
            (delete-region bol (point))
          (setq last text))))))

;; For-each
;; --------
;;
(define-key global-map [(control ?z) ?e] 'for-each)
(defun for-each (begin end place start stop step)
  "Copy the active region multiple times, each time replacing
all occurences of a placeholder matching a regexp with an
incremented number.  Useful for creating numbered list.  Use this
multiple times with different place holders for multi-dimensional
expansions."
  (interactive "r\nsPlaceholder: \nn Start: \nn Stop: \nn Step: ")
  (let ((template (buffer-substring begin end))
        (count (1+ (/ (- stop start) step))))
    (delete-region begin end)
    (dotimes (time count)
      (let ((value (number-to-string (+ start (* time step))))
            (expansion template))
        (while (string-match place expansion)
          (setq expansion (replace-match value nil t expansion)))
        (insert expansion)))))

;; Query-simultaneous-replace
;; --------------------------
;;
(define-key global-map [(control ?z) ?r] 'query-simultaneous-replace)
(defun query-simultaneous-replace (from-string to-string &optional delimited start end)
  "This behaves like the standard query-replace except that it
replaces strings from one space-delimited set with the
corresponding string from another.  For example, given the
replacement \"x y z -> y z x\" it will change x to y, y to z, and
z to x, etc.  Useful for swapping and rotating strings."
  (interactive
   (let ((common
          (query-replace-read-args
           (concat "Query simultaneous replace"
                   (if current-prefix-arg " words" "")
                   (if (use-region-p) " in region" ""))
           nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
           (if (use-region-p) (region-beginning))
           (if (use-region-p) (region-end)))))
  (let* ((from-list (split-string from-string))
         (to-list (split-string to-string))
         (regexp (concat "\\(" (mapconcat 'regexp-quote from-list "\\|") "\\)"))
         (alist (pairlis from-list to-list))
         (to-expr (lambda (alist count)
                    (cdr (assoc (match-string 0) alist)))))
    (perform-replace regexp (cons to-expr alist)
                     t 'literal delimited nil nil start end)))

;; Space-to-column
;; ---------------
;;
(define-key global-map [(control ?z) ? ] 'space-to-column)
(defun space-to-column (col)
  "Insert spaces until the point reaches a particular column,
specified via the universal argument.  If no column is given then
it uses the current goal column.  This does nothing if the point
is already past the goal column.  Useful for quickly lining up
columns of text via key macros."
  (interactive "P")
  (insert (make-string (max 0 (- (if col
                                     col
                                   goal-column)
                                 (current-column))) ? )))

;; Yank-on-right
;; -------------
;;
(defun yank-on-right (start end &optional margin)
    "Yank the current kill, inserting it to the right of the
current region.  Rectangle editing can be used to place blocks of
text in columns alongside each other.  But that usually requires
finding the longest lines and then padding top or bottom lines to
match.  This function produces the same effect without the
hassle."
    (interactive "r\np")
    (goto-char start)
    (end-of-line)
    (let ((lines (split-string (current-kill 0) "\n"))
          (width (current-column)))
      (while (< (point) end)
        (end-of-line 2)
        (setq width (max width (current-column))))
      (setq width (+ margin width))
      (goto-char start)
      (push-mark end)
      (while (and (< (point) (mark)) lines)
        (move-to-column width t)
        (insert (car lines))
        (setq lines (cdr lines))
        (forward-line))
      (pop-mark)))

;; Toggle-mini-emacs
;; ------------------
;;
(define-key global-map [(control ?z) ?m] 'toggle-mini-emacs)
  (defun toggle-mini-emacs ()
    "Switches back and forth between a minimalist look by
toggling the toolbar and menubar."
    (interactive)
    (let ((setting (if menu-bar-mode -1 1)))
      (menu-bar-mode setting)
      (tool-bar-mode setting)))

;; Toggle-zoom
;; -----------
;;
(define-key global-map [(control ?z) ?z] 'toggle-zoom)
(defun toggle-zoom ()
  "Toggles a zoom to fullscreen on some platforms.  Good for
hiding clutter and getting things done without distractions."
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (unless (frame-parameter nil 'fullscreen)
                         'fullboth)))

;; Rotate-windows
;; --------------
;;
(define-key global-map [(control ?z) ?w] 'rotate-windows)
(defun rotate-windows ()
  "Rotate the buffers between windows in the current frame."
  (interactive)
  (let* ((windows (window-list))
         (buffers (mapcar 'window-buffer windows))
         (points (mapcar 'window-point windows)))
    (setq buffers (nconc (cdr buffers) (list (car buffers)))
          points (nconc (cdr points) (list (car points))))
    (while windows
      (set-window-buffer (car windows) (car buffers))
      (set-window-point (car windows) (car points))
      (setq windows (cdr windows)
            buffers (cdr buffers)
            points (cdr points)))))

;; Show-ascii-chart
;; ----------------
;;
(defun show-ascii-chart ()
  "Display a helpful ASCII reference chart.  Useful for quickly
double checking or looking up character codes.  Usually the
what-cursor-position (C-x =) is faster for spot lookups of the
number for a character here and there.  It's terrible, however,
for finding the character given a number."
  (interactive)
  (with-output-to-temp-buffer "ASCII Chart"
    (set-buffer "ASCII Chart")
    (insert-file-contents "~/.emacs.d/ascii.txt")))

;; Inline-calc
;; -----------
;;
(define-key global-map [(control ?z) ?c] 'inline-calc)
(defun inline-calc (arg)
  "Evaluate a region as an algebraic expression via Calc.  If a
region is active, it will evaluate it and either replace it with
or append to it (if given a prefix argument) the result.  If the
is no active region, it prompts for an expression via the
minibuffer."
  (interactive "P")
  (require 'calc)
  (let ((calc-multiplication-has-precedence nil))
    (if (use-region-p)
        (let ((result (calc-eval (buffer-substring (region-beginning)
                                                   (region-end)))))
          (if (not arg)
              (delete-region (region-beginning) (region-end))
            (goto-char (region-end))
            (insert " = "))
          (insert result))
      (message "Result: %s" (calc-eval (read-string "Expression: "))))))

;; Font-size
;; ---------
;;
(define-key global-map [(control ?z) ?-] 'font-size-decrease)
(define-key global-map [(control ?z) ?=] 'font-size-increase)
(defun font-size (n)
  "Make the current display font size a step larger or smaller.
Sometimes you just feel like a larger, bolder font.  Other times
you want something a little smaller to get the big picture."
  (interactive "n")
  (set-face-attribute 'default nil
                      :height (+ (face-attribute 'default :height) n))
  (message "Font size: %s" (face-attribute 'default :height)))
(defun font-size-decrease () (interactive) (font-size -10))
(defun font-size-increase () (interactive) (font-size 10))

;; External-compile-check
;; ----------------------
;;
(defun external-compile-check (&optional tree)
  "Check for unsaved files and flash the frame the first time one
is found.  When compiling from a standalone shell outside of
Emacs, one can sometimes forget to save buffers before compiling
which can lead to confusion.  Defining a shell alias to invoke
something like:
  emacsclient -e '(external-compile-check \"^/sourcetree/\")' && make
causes Emacs to flash and prompt to save any unsaved buffers first."
  (let ((flashed nil)
        (original (face-attribute 'default :background)))
    (save-some-buffers nil
     (lambda ()
       (when (string-match (or tree "") (or (buffer-file-name) ""))
         (unless flashed
           (set-face-attribute 'default nil :background "white") (sit-for 0.02)
           (set-face-attribute 'default nil :background "black") (sit-for 0.02)
           (set-face-attribute 'default nil :background "white") (sit-for 0.02)
           (set-face-attribute 'default nil :background "black") (sit-for 0.02)
           (set-face-attribute 'default nil :background original)
           (setq flashed t))
         t)))
    nil))

;; Revert-some-buffers
;; -------------------
;;
(defun revert-some-buffers ()
  "Revert buffers whose files have changed on disk.  Prompts on
edited buffers. Version control and other things can change large
numbers of files behind Emacs' back.  While Emacs is usually good
about detecting this when you try to edit a buffer, read-only and
merely viewed buffers may fall out of date.  This takes care of
updating everything at once."
  (interactive)
  (let ((count 0)
        (continue t))
    (mapc (lambda (buf)
            (unless (or (verify-visited-file-modtime buf)
                        (not (file-exists-p (buffer-file-name buf))))
              (with-current-buffer buf
                (when (or (not (buffer-modified-p))
                          (yes-or-no-p
                           (format "Buffer %s is modified; revert anyway? "
                                   (buffer-name))))
                  (message "Reverting buffer %s..." (buffer-name))
                  (setq continue t)               ; Revert sometimes fails.
                  (while continue
                    (condition-case nil
                        (progn (revert-buffer t t)
                               (setq count (1+ count)
                                     continue nil))
                      (error t)))))))
        (buffer-list))
  (message "Reverted %d buffer%s."
           count (if (= count 1) "" "s"))))

;; Unfill-paragraph
;; ----------------
;;
(define-key global-map [(control ?z) ?u] 'unfill-paragraph)
(defun unfill-paragraph ()
  "Removes internal line breaks from paragraph at point, or from
region if the mark is active."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph)))

;; Fmt-fill
;; --------
;; Normally, I prefer to have everything in pure Elisp so that it's self
;; contained and portable to all platforms.  But fmt is well-tested and
;; convenient for now.
;;
(define-key global-map [(control ?z) ?q] 'fmt-fill)
(defun fmt-fill (begin end)
  "Attempt to fill the region using the standard fmt utility.
This utility normally uses a Knuth-Plass-inspired algorithm that
considers entire paragraphs at a time and attempts to reduce
raggedness and to avoid orphans and widows, etc.  This may or may
not give better results than the standard fill-paragraph."
  (interactive "r")
  (goto-char end)
  (unless (bolp)
    (forward-line 1)
    (setq end (point)))
  (goto-char begin)
  (unless (bolp)
    (forward-line 0)
    (setq begin (point)))
  (let ((command (format "fmt -c -u -w%d" (current-fill-column)))
        (prefix (fill-match-adaptive-prefix)))
    (when (> (count-lines begin end) 1)
      (forward-line 1)
      (setq prefix (fill-common-string-prefix
                     prefix
                     (fill-match-adaptive-prefix))))
    (when (and prefix (not (string-equal prefix "")))
      (setq command (concat command " -p " (shell-quote-argument prefix))))
    (shell-command-on-region begin end command nil t)))

;; Wrap-modes-dwim
;; ---------------
;;
(define-key global-map [(control ?z) ?f] 'wrap-modes-dwim)
(defun wrap-modes-dwim ()
  "Cycles between auto-fill-mode, visual-line-mode and neither active."
  (interactive)
  (cond (auto-fill-function (auto-fill-mode 0) (visual-line-mode t))
        (visual-line-mode (auto-fill-mode 0) (visual-line-mode 0))
        (t (auto-fill-mode t) (visual-line-mode 0))))

;; Ido-really-wash-history
;; -----------------------
;;
(defun ido-really-wash-history ()
  "Remove non-local or non-existent entries from ido-mode's history and cache."
  (interactive)
  (ido-wash-history)
  (setq ido-last-directory-list
        (delq nil (mapcar
                   (lambda (entry)
                     (if (and (ido-local-file-exists-p (car entry))
                              (file-directory-p (concat (car entry) (cdr entry))))
                         entry))
                   ido-last-directory-list)))
  (setq ido-work-directory-list
        (delq nil (mapcar
                   (lambda (entry)
                     (if (and (ido-local-file-exists-p entry)
                              (file-directory-p entry))
                         entry))
                   ido-work-directory-list)))
  (mapc 'ido-file-name-all-completions
        (mapcar 'car ido-dir-file-cache)))

;; Local Variables:
;; mode: emacs-lisp
;; comment-column: 64
;; End:
;;; init.el ends here

(dark-on-light-theme)
(message "colors set from 'dark-on-light'")
