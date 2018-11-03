;;; notes-mode.el --- major mode for editing outline structured notes

;; Copyright (C) 2003-2009  Andrew Kensler

;; Author: Andrew Kensler
;; Version: 20090520
;; Last-Updated: Wed May 20 13:42:32 2009 (-0600)
;; Keywords: local, outlines

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.
;;
;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this file; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; Major mode for editing outline structured notes.  The general premise is
;; that you want to take notes online in heirarchical, bulleted format or
;; wish to write an outline in such a way.

;;; Code:

(provide 'notes-mode)
(require 'easymenu)

;; ======================================================================
;; Utilities
;; ======================================================================

(defun notes-match-column (match)
  "Return the column that the given match ends on."
  (save-excursion
    (goto-char (match-end match))
    (current-column)))

(defun notes-replace-all (regexp replacement start stop)
  "Globally replace a regular expression."
  (goto-char start)
  (while (re-search-forward regexp stop 'stop)
    (replace-match replacement)))

;; ======================================================================
;; Finding
;; ======================================================================

(defun notes-find-branch ()
  "Looks for the outline node around point and determines the extent of the
entire matching node including its children.  The start and end points, and
level of indentation are returned together as a list.  Alternatively,
returns nil if the current point does not seem to be in an outline node."
  (save-excursion
    (end-of-line)
    (if (or (re-search-backward "^\\([ \t]*\\)- " nil 'stop)
            (looking-at "^\\([ \t]*\\)- "))
        (let ((start (point))
              (indent (notes-match-column 1)))
          (while (and (= (forward-line) 0)
                      (re-search-forward "^\\([ \t]*\\)- " nil 'stop)
                      (progn
                        (goto-char (match-beginning 0))
                        (> (notes-match-column 1) indent))))
          (list start (point) indent)))))

(defun notes-find-node ()
  "Looks for the outline node around point and determines the extent of
it. The start and end points, and level of indentation are returned
together as a list.  Alternatively, returns nil if the current point does
not seem to be in an outline node."
  (save-excursion
    (end-of-line)
    (if (or (re-search-backward "^\\([ \t]*\\)- " nil 'stop)
            (looking-at "^\\([ \t]*\\)- "))
        (let ((start (point))
              (indent (notes-match-column 1)))
          (forward-line)
          (if (re-search-forward "^\\([ \t]*\\)- " nil 'stop)
              (goto-char (match-beginning 0)))
          (list start (point) indent)))))

(defun notes-find-prev-branch ()
  "Looks for the outline branch before the one containing point that is at
the same level as the one containing point and doesn't cross parents.  Will
return either the standard list of start, stop and depth on success or nil
if there is no previous branch."
  (save-excursion
    (end-of-line)
    (if (or (re-search-backward "^\\([ \t]*\\)- " nil 'stop)
            (looking-at "^\\([ \t]*\\)- "))
        (let ((start (point))
              (indent (notes-match-column 1)))
          (while (and (re-search-backward "^\\([ \t]*\\)- " nil 'stop)
                      (> (notes-match-column 1) indent)))
          (if (and (= (notes-match-column 1) indent)
                   (not (= (point) start)))
              (list (point) start indent))))))

(defun notes-find-prev-node ()
  "Looks for the outline node before the one containing point that is at
the same level as the one containing point and doesn't cross parents.  Will
return either the standard list of start, stop and depth on success or nil
if there is no previous node."
  (save-excursion
    (end-of-line)
    (if (or (re-search-backward "^\\([ \t]*\\)- " nil 'stop)
            (looking-at "^\\([ \t]*\\)- "))
        (let ((start (point))
              (indent (notes-match-column 1)))
          (while (and (re-search-backward "^\\([ \t]*\\)- " nil 'stop)
                      (> (notes-match-column 1) indent))
            (setq start (point)))
          (if (and (= (notes-match-column 1) indent)
                   (not (= (point) start)))
              (list (point) start indent))))))

(defun notes-find-next-branch ()
  "Looks for the outline branch after the one containing point that is at
the same level as the one containing point and doesn't cross parents.  Will
return either the standard list of start, stop and depth on success or nil
if there is no next branch."
  (save-excursion
    (end-of-line)
    (if (or (re-search-backward "^\\([ \t]*\\)- " nil 'stop)
            (looking-at "^\\([ \t]*\\)- "))
        (let ((indent (notes-match-column 1)))
          (forward-line)
          (while (and (re-search-forward "^\\([ \t]*\\)- " nil 'stop)
                      (> (notes-match-column 1) indent)))
          (if (= (notes-match-column 1) indent)
              (notes-find-branch))))))

(defun notes-find-next-node ()
  "Looks for the outline node after the one containing point that is at
the same level as the one containing point and doesn't cross parents.  Will
return either the standard list of start, stop and depth on success or nil
if there is no next node."
  (save-excursion
    (end-of-line)
    (if (or (re-search-backward "^\\([ \t]*\\)- " nil 'stop)
            (looking-at "^\\([ \t]*\\)- "))
        (let ((indent (notes-match-column 1)))
          (forward-line)
          (while (and (re-search-forward "^\\([ \t]*\\)- " nil 'stop)
                      (> (notes-match-column 1) indent)))
          (if (= (notes-match-column 1) indent)
              (notes-find-node))))))

(defun notes-find-parent-node ()
  "Looks for the outline node that is the parent of the one containing
point.  Will return either the standard list of start, stop and depth on
success or nil if there is no parent node."
  (save-excursion
    (end-of-line)
    (if (or (re-search-backward "^\\([ \t]*\\)- " nil 'stop)
            (looking-at "^\\([ \t]*\\)- "))
        (let ((start (point))
              (indent (notes-match-column 1)))
          (while (and (re-search-backward "^\\([ \t]*\\)- " nil 'stop)
                      (>= (notes-match-column 1) indent))
            (setq start (point)))
          (if (and (< (notes-match-column 1) indent)
                   (not (= (point) start)))
              (list (point) start indent))))))

(defun notes-find-child-node ()
  "Looks for the outline node that is the first child of the one containing
point.  Will return either the standard list of start, stop and depth on
success or nil if there is no parent node."
  (save-excursion
    (end-of-line)
    (if (or (re-search-backward "^\\([ \t]*\\)- " nil 'stop)
            (looking-at "^\\([ \t]*\\)- "))
        (let ((indent (notes-match-column 1)))
          (forward-line)
          (if (re-search-forward "^\\([ \t]*\\)- " nil 'stop)
              (let ((start (match-beginning 0))
                    (cindent (notes-match-column 1)))
                (if (> cindent indent)
                    (progn (if (re-search-forward "^\\([ \t]*\\)- " nil 'stop)
                               (beginning-of-line))
                           (list start (point) cindent)))))))))

;; ======================================================================
;; Movement
;; ======================================================================

(defun notes-move-branch-up ()
  "Moves the entire current branch around point upwards through the
heirarchy.  Any sub-nodes are moved with it, and the position of point is
preserved."
  (interactive)
  (let ((prev (notes-find-prev-branch))
        (branch (notes-find-branch)))
    (if (and prev branch)
        (save-excursion
          (goto-char (cadr branch))
          (let ((txt (buffer-substring (car prev) (cadr prev))))
            (delete-region (car prev) (cadr prev))
            (insert txt))))))

(defun notes-move-branch-down ()
  "Moves the entire current branch around point downwards through the
heirarchy.  Any sub-nodes are moved with it, and the position of point is
preserved."
  (interactive)
  (let ((next (notes-find-next-branch))
        (branch (notes-find-branch)))
    (if (and next branch)
        (save-excursion
          (goto-char (car branch))
          (let ((txt (buffer-substring (car next) (cadr next))))
            (delete-region (car next) (cadr next))
            (insert txt))))))

(defun notes-demote-branch ()
  "Demotes the entire current branch around point, including any sub-nodes,
by indenting them all by `tab-width' spaces.  The position of point is
preserved."
  (interactive)
  (let ((branch (notes-find-branch)))
    (if branch
        (save-excursion
          (indent-rigidly (car branch) (cadr branch) tab-width)))))

(defun notes-demote-node ()
  "Demotes the current node at point by indenting it by `tab-width' spaces.
The position of point is preserved."
  (interactive)
  (let ((node (notes-find-node)))
    (if node
        (save-excursion
          (indent-rigidly (car node) (cadr node) tab-width)))))

(defun notes-promote-branch ()
  "Promotes the entire current branch around point, including any
sub-nodes, by unindenting them all by `tab-width' spaces.  The position of
point is preserved."
  (interactive)
  (let ((branch (notes-find-branch)))
    (if (and branch (> (car (cddr branch)) 0))
        (save-excursion
          (indent-rigidly (car branch) (cadr branch) (- tab-width))))))

(defun notes-promote-node ()
  "Promotes the current node around point by unindenting it by `tab-width'
space.  The position of point is preserved."
  (interactive)
  (let ((node (notes-find-node)))
    (if (and node (> (car (cddr node)) 0))
        (save-excursion
          (indent-rigidly (car node) (cadr node) (- tab-width))))))

;; ======================================================================
;; Navigation
;; ======================================================================

(defun notes-navigate-prev ()
  "Try to move point to the start of the node before the one containing point that
is at the same level as the one containing point.  This will not move
across the boundaries of the parent node."
  (interactive)
  (let ((prev (notes-find-prev-node)))
    (if prev
        (progn
          (goto-char (car prev))
          (re-search-forward "^\\([ \t]*\\)- " nil 'stop)))))

(defun notes-navigate-next ()
  "Try to move point to the start of the node after the one containing
point that is at the same level as the one containing point.  This will not
move across the boundaries of the parent node."
  (interactive)
  (let ((next (notes-find-next-node)))
    (if next
        (progn
          (goto-char (car next))
          (re-search-forward "^\\([ \t]*\\)- " nil 'stop)))))

(defun notes-navigate-parent ()
  "Try to move point to the start of the node that is the parent to the
node containing point."
  (interactive)
  (let ((parent (notes-find-parent-node)))
    (if parent
        (progn
          (goto-char (car parent))
          (re-search-forward "^\\([ \t]*\\)- " nil 'stop)))))

(defun notes-navigate-child ()
  "Try to move point to the start of the node that is the first child to
the node containing point."
  (interactive)
  (let ((child (notes-find-child-node)))
    (if child
        (progn
          (goto-char (car child))
          (re-search-forward "^\\([ \t]*\\)- " nil 'stop)))))

;; ======================================================================
;; Marking
;; ======================================================================

(defun notes-mark-branch ()
  "Mark the current branch containing point."
  (interactive)
  (let ((branch (notes-find-branch)))
    (if branch
        (progn (push-mark (car branch) t t)
               (goto-char (cadr branch))))))

(defun notes-mark-node ()
  "Mark the current node containing point."
  (interactive)
  (let ((node (notes-find-node)))
    (if node
        (progn (push-mark (car node) t t)
               (goto-char (cadr node))))))

;; ======================================================================
;; Hiding and Showing
;; ======================================================================

(defun notes-hide-branch ()
  "Hide the current branch containing point."
  (interactive)
  (let ((branch (notes-find-branch)))
    (if branch
        (subst-char-in-region (car branch)
                              (if (char-equal (char-before (cadr branch)) ?\n)
                                  (1- (cadr branch))
                                (cadr branch))
                              ?\n ?\r))))

(defun notes-show-branch ()
  "Show the current branch containing point."
  (interactive)
  (let ((branch (notes-find-branch)))
    (if branch
        (subst-char-in-region (car branch)
                              (if (char-equal (char-before (cadr branch)) ?\n)
                                  (1- (cadr branch))
                                (cadr branch))
                              ?\r ?\n))))

;; ======================================================================
;; Electric
;; ======================================================================

(defun notes-electric-return ()
  "Electric return for Notes mode.  If at the end of a line, inserts a new
line and then inserts the lead for a new node at the same level as the one
above.  If return is pressed while point is in the middle of a line, it
simply inserts return and indents to the appropriate column."
  (interactive)
  (let ((node (notes-find-node))
        (end (eolp)))
    (skip-chars-backward " ")
    (insert "\n")
    (skip-chars-forward " ")
    (cond ((< (current-column) (car (cddr node)))
           (insert (make-string (- (car (cddr node)) (current-column)) ?\ )))
          ((> (current-column) (car (cddr node)))
           (delete-char (- (car (cddr node)) (current-column)))))
    (cond (end (insert "- "))
          ((not (looking-at "- ")) (insert "  ")))))

(defun notes-electric-backspace ()
  "Electric backspace for Notes mode.  If point is at the end of a blank
notes entry, then backspace should delete the entire line back to the
beginning, deleting the note prefix as well."
  (interactive)
  (cond ((if (fboundp 'region-exists-p)
             (region-exists-p)
           (and transient-mark-mode mark-active))
         (delete-region (mark) (point)))
        ((and (eolp)
              (equal (save-excursion
                       (beginning-of-line)
                       (re-search-forward "^\\([ \t]*\\)- " nil t))
                     (point)))
         (delete-region (save-excursion (beginning-of-line) (point))
                        (point)))
        (t (delete-backward-char 1))))

;; ======================================================================
;; Auto Fill
;; ======================================================================

(defun notes-do-auto-fill ()
  "Custom `auto-fill-function'.  Determines the correct amount of
indentation being used at this node and locally sets `fill-prefix' to it
before calling the standard `do-auto-fill' to wrap the line."
  (if (> (save-excursion (end-of-line) (current-column))
         fill-column)
      (let ((node (notes-find-node)))
        (if node
            (let ((fill-prefix (make-string (+ (car (cddr node)) 2) ?\ )))
              (do-auto-fill))))))

(defun notes-adaptive-fill-function ()
  "Custom `adaptive-fill-function' for notes mode.  Looks at the indentation
of the beginning of the node to determine the approriate `fill-prefix' to
return."
  (let ((node (notes-find-node)))
    (if node
        (make-string (+ (car (cddr node)) 2) ?\ ))))

;; ======================================================================
;; Conversion
;; ======================================================================

(defun notes-to-xhtml-bullets (start stop)
  "Replace the standard indented outline style of Notes mode with
XHTML-style markup for nested bulleted lists."
  (interactive "r")
  (let* ((stop-marker (copy-marker stop))
         (stack (list -1))
         (close-to (lambda (level)
                     (if (>= level (car stack))
                         ""
                       (concat (make-string (car stack) ?\ ) "</li>\n"
                               (make-string (car stack) ?\ ) "</ul>\n"
                               (progn (setq stack (cdr stack)) (funcall close-to level)))))))
    (goto-char start)
    (while (re-search-forward "^\\([ \t]*\\)- " stop-marker 'stop)
      (let ((indent (notes-match-column 1)))
        (cond ((> indent (car stack))
               (replace-match "\\1<ul>\n\\1<li>")
               (setq stack (cons indent stack)))
              ((= indent (car stack))
               (replace-match "\\1</li>\n\\1<li>"))
              (t
               (replace-match (concat (funcall close-to indent) "\\1</li>\n\\1<li>"))))))
    (insert-before-markers (funcall close-to -1))
    (notes-replace-all "\\([^> \t\n]\\)[ \t\n]+</li>" "\\1</li>" start stop-marker)
    (notes-replace-all "^\\([ \t]*\\)\\(</?li>\\)" "\\1  \\2" start stop-marker)
    (notes-replace-all "<li>\\([^\000]*?\\)\\([ \t\n]*</li>\\|[ \t\n]*<ul>\\)" "<li><p>\\1</p>\\2" start stop-marker)
    (notes-replace-all "[ \t]*\n\\(?:[ \t]*\n\\)+\\([ \t]+\\)" "</p>\n\\1<p>" start stop-marker)))

(defun notes-to-latex-bullets (start stop)
  "Replace the standard indented outline style of Notes mode with
LaTeX-style markup for nested bulleted lists."
  (interactive "r")
  (let* ((stop-marker (copy-marker stop))
         (stack (list -1))
         (close-to (lambda (level)
                     (if (>= level (car stack))
                         ""
                       (concat (make-string (car stack) ?\ ) "\\\\end{itemize}\n"
                               (progn (setq stack (cdr stack)) (funcall close-to level)))))))
    (goto-char start)
    (while (re-search-forward "^\\([ \t]*\\)- " stop-marker 'stop)
      (let ((indent (notes-match-column 1)))
        (cond ((> indent (car stack))
               (replace-match "\\1\\\\begin{itemize}\n\\1\\\\item  ")
               (setq stack (cons indent stack)))
              ((= indent (car stack))
               (replace-match "\\1\\\\item  "))
              (t
               (replace-match (concat (funcall close-to indent) "\\1\\\\item  "))))))
    (insert-before-markers (funcall close-to -1))
    (notes-replace-all "\\\\\\\\end{itemize}" "\\\\end{itemize}" start stop-marker)
    (notes-replace-all "^\\([ \t]*\\)\\\\item" "\\1  \\\\item" start stop-marker)))

(defun notes-to-xhtml-headers (start stop)
  "Replace the standard indented outline style of Notes mode with
XHTML-style markup for headers."
  (interactive "r")
  (let ((stop-marker (copy-marker stop))
        (stack-indent (list -1))
        (stack-number nil)
        (list-to-string (lambda (ls)
                          (if (null ls)
                              ""
                            (concat (funcall list-to-string (cdr ls))
                                    (number-to-string (car ls))
                                    ".")))))
    (while (progn (goto-char start)
                  (re-search-forward "^\\([ \t]*\\)- " stop-marker 'stop))
      (let ((indent (notes-match-column 1)))
        (cond ((> indent (car stack-indent))
                  (setq stack-indent (cons indent stack-indent))
                  (setq stack-number (cons 1 stack-number)))
              ((= indent (car stack-indent))
               (setq stack-number (cons (1+ (car stack-number)) (cdr stack-number))))
              (t
               (while (< indent (car stack-indent))
                 (setq stack-indent (cdr stack-indent))
                 (setq stack-number (cdr stack-number)))
               (setq stack-number (cons (1+ (car stack-number)) (cdr stack-number)))))
        (replace-match (concat "<h" (number-to-string (length stack-number)) ">"
                               (funcall list-to-string stack-number) " "))
        (re-search-forward "^\\([ \t]*\\)- " stop-marker 'stop)
        (beginning-of-line)
        (insert-before-markers (concat "</h" (number-to-string (length stack-number)) ">\n"))))
    (goto-char start)
    (while (re-search-forward "[ \t\n]*\\(</h[0-9]+>\\)" stop-marker 'stop)
      (replace-match "\\1"))))

(defun notes-to-latex-headers (start stop)
  "Replace the standard indented outline style of Notes mode with
LaTeX-style markup for headers."
  (interactive "r")
  (let ((stop-marker (copy-marker stop))
        (stack (list -1)))
    (while (progn (goto-char start)
                  (re-search-forward "^\\([ \t]*\\)- " stop-marker 'stop))
      (let ((indent (notes-match-column 1)))
        (if (> indent (car stack))
            (setq stack (cons indent stack))
          (while (< indent (car stack))
            (setq stack (cdr stack))))
        (replace-match (nth (- (length stack) 2)
                            '("\\\\section{" "\\\\subsection{" "\\\\subsubsection{"
                              "\\\\paragraph{" "\\\\subparagraph{")))
        (re-search-forward "^\\([ \t]*\\)- " stop-marker 'stop)
        (beginning-of-line)
        (insert-before-markers "}\n")))
    (goto-char start)
    (while (re-search-forward "[ \t\n]*}" stop-marker 'stop)
      (replace-match "}"))))

;; ======================================================================
;; Mode Definition
;; ======================================================================

(defvar notes-mode-map nil
  "Key map used while in Notes mode.")
(unless notes-mode-map
  (setq notes-mode-map (make-sparse-keymap))
  (define-key notes-mode-map [return] 'notes-electric-return)
  (define-key notes-mode-map [?\r] 'notes-electric-return)
  (define-key notes-mode-map [backspace] 'notes-electric-backspace)
  (define-key notes-mode-map [(meta up)] 'notes-navigate-prev)
  (define-key notes-mode-map [(meta down)] 'notes-navigate-next)
  (define-key notes-mode-map [(meta left)] 'notes-navigate-parent)
  (define-key notes-mode-map [(meta right)] 'notes-navigate-child)
  (define-key notes-mode-map [(shift tab)] 'notes-promote-node)
  (define-key notes-mode-map [tab] 'notes-demote-node)
  (define-key notes-mode-map [(control ?c) up] 'notes-move-branch-up)
  (define-key notes-mode-map [(control ?c) down] 'notes-move-branch-down)
  (define-key notes-mode-map [(control ?c) left] 'notes-promote-branch)
  (define-key notes-mode-map [(control ?c) right] 'notes-demote-branch)
  (define-key notes-mode-map [(control ?c) ?b] 'notes-mark-branch)
  (define-key notes-mode-map [(control ?c) ?n] 'notes-mark-node)
  (define-key notes-mode-map [(control ?c) ?h] 'notes-hide-branch)
  (define-key notes-mode-map [(control ?c) ?s] 'notes-show-branch)
  (define-key notes-mode-map [(control ?c) ?x] 'notes-to-xhtml-bullets)
  (define-key notes-mode-map [(control ?c) ?l] 'notes-to-latex-bullets))

(easy-menu-define notes-mode-menu notes-mode-map "Notes Mode Menu"
                  '("Notes"
                    ["Go to previous" notes-navigate-prev]
                    ["Go to next" notes-navigate-next]
                    ["Go to parent" notes-navigate-parent]
                    ["Go to child" notes-navigate-child]
                    "---"
                    ["Promote node" notes-promote-node]
                    ["Demote node" notes-demote-node]
                    "---"
                    ["Move branch up" notes-move-branch-up]
                    ["Move branch down" notes-move-branch-down]
                    ["Promote branch" notes-promote-branch]
                    ["Demote branch" notes-demote-branch]
                    "---"
                    ["Mark branch" notes-mark-branch]
                    ["Mark node" notes-mark-node]
                    "---"
                    ["Hide branch" notes-hide-branch]
                    ["Show branch" notes-show-branch]
                    "---"
                    ["Convert to XHTML bullets" notes-to-xhtml-bullets]
                    ["Convert to LaTeX bullets" notes-to-latex-bullets]
                    ["Convert to XHTML headers" notes-to-xhtml-headers]
                    ["Convert to LaTeX headers" notes-to-latex-headers]))

;;;###autoload
(defun notes-mode ()
  "Major mode for editing outline structured notes.  The general premise is
that you want to take notes online in heirarchical, bulleted format or wish
to write an outline in such a way.  A 'node' in the outline starts at a line
that begins with any number of spaces, followed by a hyphen, then another
space and then the text of the item.  Thus the general structure is:

- Item I
    - Item A
        - Item 1
    - Item B
        - Item 2
        - Item 3
- Item II
    - Item C
        - Item 3
        - Item 4
    - Item D
        - Item 5
- Item III
    - Item E
    - Item F

  A 'branch' in the outline consist of a node plus all of its children
(nodes immediately following and indented).  The mode provides facilities
for rapidly navigating, moving branches, and formatting nodes.  In general,
the M-arrow keys are used for navigation and the C-c arrow keys for moving
the current branch.  Tab and Sh-Tab promote or demote the current node, C-c
n marks the current node and C-c b marks the current branch.  The mode also
attempts to handle the return key intelligently and make the paragraph
filling system behave correctly.  The idea is to be as simple, rapid and
intuitive as possible."

  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Notes"
        major-mode 'notes-mode
        normal-auto-fill-function 'notes-do-auto-fill
        paragraph-start "[ \t\n]*$\\|[ \t]*- "
        adaptive-fill-regexp nil
        adaptive-fill-function 'notes-adaptive-fill-function
        selective-display t
        selective-display-ellipses t)
  (use-local-map notes-mode-map)
  (easy-menu-add notes-mode-menu notes-mode-map)
  (if (and (featurep 'menubar)
           current-menubar)
      (progn (set-buffer-menubar current-menubar)
             (add-submenu nil notes-mode-menu)))
  (run-hooks 'text-mode-hook))

;; Local Variables:
;; mode: emacs-lisp
;; comment-column: 64
;; End:
;;; notes-mode.el ends here

