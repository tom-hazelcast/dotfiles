

;;; file-tree.el --- handy tree display of filesystem for navigating files

;; Copyright (C) 2005-2009  Andrew Kensler

;; Author: Andrew Kensler
;; Version: 20090520
;; Last-Updated: Wed May 20 13:42:38 2009 (-0600)
;; Keywords: local, files

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

;; Creates a persistent view of the files below a certain directory.  If
;; the user presses f on any of the files, they will be loaded in the other
;; window.  This is meant to allow quick navigation much like the speedbar
;; file list but friendlier to use on a TTY.

;;; Code:

(provide 'file-tree)

(defvar file-tree-indent
  2
  "Amount to indent for each directory.")

(defvar file-tree-ignore
  '("^\.$" "^\.\.$" "^\.svn$" "^_MTN$" "^CVS$" "^#.*?#$" "^\.#" "~$")
  "List of regular expressions for filenames to ignore.")

(defun file-tree-make-tree (path)
  "Erase the current buffer and replace its contents with a recursively
generated display the list of files from a given root path."
  (interactive "DFile tree from: ")
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "Directory: " (file-truename path) "\n")
  (let* ((matches (lambda (name lst)
                    (if lst
                        (if (string-match (car lst) name)
                            t
                          (funcall matches name (cdr lst))))))
         (recurse (lambda (dir depth)
                    (let ((files (directory-files dir))
                          (indent (make-string depth ?\ )))
                      (while files
                        (if (not (funcall matches (car files) file-tree-ignore))
                            (let ((fullpath (concat dir "/" (car files)))
                                  (line (concat indent (car files) "\n")))
                              (put-text-property 0 (length line) 'path fullpath line)
                              (insert line)
                              (if (file-directory-p fullpath)
                                  (funcall recurse fullpath (+ depth file-tree-indent)))))
                        (setq files (cdr files)))))))
    (funcall recurse (file-truename path) 0))
  (goto-char (point-min)))

(defun file-tree-find ()
  "Load the file in the list named under point."
  (interactive)
  (let ((path (get-text-property (point) 'path)))
    (if path
        (find-file-other-window path))))

(defun file-tree-move-and-find (event)
  "Move point in the buffer to clicked character and find the file."
  (interactive "e")
  (mouse-set-point event)
  (file-tree-find))

(defvar file-tree-mode-map nil
  "Key map used for the file-tree window.")
(unless file-tree-mode-map
  (setq file-tree-mode-map (make-sparse-keymap))
  (define-key file-tree-mode-map [return] 'file-tree-find)
  (define-key file-tree-mode-map [?\r] 'file-tree-find)
  (define-key file-tree-mode-map [mouse-1] 'file-tree-move-and-find)
  (define-key file-tree-mode-map [?f] 'file-tree-find)
  (define-key file-tree-mode-map [?q] 'delete-window)
  (define-key file-tree-mode-map [?r] 'file-tree-make-tree))

;;;###autoload
(defun file-tree (path)
  "Creates a buffer and window with a persistant display of the tree of the
files under a given path.  Switching to the buffer and pressing enter or f
will find the file.  Press r to refresh the tree, possibly with a new
top-level directory.  Press q to close the window.  This is meant to allow
quick navigation much like the speedbar file list but friendlier to use on
a TTY."
  (interactive "DFile tree from: ")
  (let* ((buf (get-buffer-create "*File Tree*"))
         (win (get-buffer-window buf)))
    (if win
        (select-window win)
      (split-window-horizontally 25)
      (set-window-buffer (selected-window) buf))
    (kill-all-local-variables)
    (file-tree-make-tree path)
    (setq buffer-read-only t)
    (setq mode-name "File-Tree"
          major-mode 'file-tree-mode)
    (use-local-map file-tree-mode-map)))

;; Local Variables:
;; mode: emacs-lisp
;; comment-column: 64
;; End:
;;; file-tree.el ends here

