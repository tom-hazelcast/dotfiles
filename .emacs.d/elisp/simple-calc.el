

;;; simple-calc.el --- simple, in-place calculator for Emacs

;; Copyright (C) 2004-2009  Andrew Kensler

;; Author: Andrew Kensler
;; Version: 20090519
;; Last-Updated: Tue May 19 16:24:35 2009 (-0600)
;; Keywords: local, tools

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

;; Simple calc is a simple, lightweight expression evaluator with a vaguely
;; C-like syntax.  If the region is active it evaluates it and either
;; replaces or appends the region with the result depending on the prefix
;; argument.  Otherwise it prompts through the minibuffer for an expression
;; to evaluate and displays the result there.  See the `simple-calc' doc
;; comment for more details.

;;; Code:

(provide 'simple-calc)

(defvar simple-calc-last-result
  0
  "Result of the last succesful evaluation.")

(defun simple-calc-lex-string (str)
  "Split a string into its constituent tokens and return them as a list."
  (let ((lexer (concat "[ \t\n\r]+\\|"
                       "0b[01]+\\|0o[0-7]+\\|0[hx][0-9a-fA-F]+\\|"
                       "[0-9]+\\(\\.[0-9]*\\)?\\(e[+-]?[0-9]+\\)?\\|"
                       "\\.[0-9]+\\(e[+-]?[0-9]+\\)?\\|"
                       "[a-z]+\\|"
                       "!=?\\|%\\|&&?\\|(\\|)\\|\\*\\*?\\|+\\|-\\|"
                       "/\\|:\\|<\\(<\\|=\\)?\\|==\\|>\\(=\\|>\\)?\\|"
                       "\\?\\|@\\|\\^\\^?\\|||?\\|~"))
        (bases '(("b" . 2) ("o" . 8) ("h" . 16) ("x" . 16)))
        (tokens '())
        (start 0))
    (while (< start (length str))
      (string-match lexer str start)
      (if (or (not (match-beginning 0))
              (< start (match-beginning 0)))
          (error "Unexpected input in expression: %s"
                 (substring str start (match-beginning 0))))
      (setq start (match-end 0))
      (let ((token (match-string 0 str)))
        (if (not (string-match "^[ \t\n\r]+$" token))
            (setq tokens (cons
                          (cond ((string-match "^0\\([bohx]\\)\\(.*\\)" token)
                                 (string-to-number (match-string 2 token)
                                                   (cdr (assoc (match-string 1 token) bases))))
                                ((string-match "^[0-9.]" token)
                                 (string-to-number token))
                                (t token))
                          tokens)))))
    (nreverse tokens)))

(defun simple-calc-parse-to-sexp (tokens)
  "Parse a list of tokens as returned by `simple-calc-lex-string' and
return a list containing a sexp to evaluate to calculate the expression.
Why a sexp instead of direct evaluation?  Because we want to be able to
short circuit some computations, though those parts need to be parsed in
either case.  A flag could be carried around indicating whether to evaluate
a sub-expression, but it's easier just to build up a sexp and let the Emacs
LISP interpreter deal with it."
  (let ((level-1 nil) (level-14 nil) (level-13 nil)) ; Ugly hack to supress compiler warnings.  Oh! for a letrec!
    (let* ((read-token (lambda ()
                         (prog1
                             (car tokens)
                           (setq tokens (cdr tokens)))))
           (read-if-match (lambda (wanted)
                            (let ((token (car tokens)))
                              (if (and (stringp token) (string= wanted token))
                                  (prog1 t (setq tokens (cdr tokens)))))))
           (level-15 (lambda ()
                       (let* ((token (funcall read-token))
                              (const (assoc token '(("pi" . 3.1415926535897932385)
                                                    ("e" . 2.7182818284590452354)
                                                    ("phi" . 1.6180339887498948482))))
                              (func (assoc token '(("sin" . sin) ("cos" . cos) ("tan" . tan)
                                                   ("asin" . asin) ("acos" . acos) ("atan" . atan)
                                                   ("sinh" . sinh) ("cosh" . cosh) ("tanh" . tanh)
                                                   ("asinh" . asinh) ("acosh" . acosh) ("atanh" . atanh)
                                                   ("exp" . exp) ("ln" . log) ("log" . log10)
                                                   ("sqrt" . sqrt) ("abs" . abs)
                                                   ("floor" . floor) ("ceil" . ceiling)
                                                   ("trunc" . truncate) ("round" . round)))))
                         (cond ((numberp token) token)
                               ((string= "@" token) simple-calc-last-result)
                               ((string= "(" token)
                                (prog1 (funcall level-1)
                                  (if (not (funcall read-if-match ")"))
                                      (error "Missing closing parenthesis: %s" (car tokens)))))
                               (const (cdr const))
                               (func
                                  (if (not (funcall read-if-match "("))
                                      (error "Expected opening parenthesis after function: %s" (car tokens)))
                                  (prog1 (list (cdr func) (funcall level-1))
                                    (if (not (funcall read-if-match ")"))
                                        (error "Missing closing parenthesis after function: %s" (car tokens)))))
                               (t
                                (error "Expected a number, function or parenthetic expression: %s" (car tokens)))))))
           (level-14 (lambda ()
                       (cond ((funcall read-if-match "+")
                              (funcall level-14))
                             ((funcall read-if-match "-")
                              `(- ,(funcall level-14)))
                             ((funcall read-if-match "!")
                              `(if (= ,(funcall level-14) 0) 1 0))
                             ((funcall read-if-match "~")
                              `(lognot ,(funcall level-14)))
                             (t
                              (funcall level-15)))))
           (level-13 (lambda ()
                       (let ((val1 (funcall level-14)))
                         (if (funcall read-if-match "**")
                             (let ((val2 (funcall level-13)))
                               `(expt ,val1 ,val2))
                           val1))))
           (level-12 (lambda ()
                       (let ((val1 (funcall level-13)))
                         (while (cond ((funcall read-if-match "*")
                                       (let ((val2 (funcall level-13)))
                                         (setq val1 `(* ,val1 ,val2))
                                         t))
                                      ((funcall read-if-match "/")
                                       (let ((val2 (funcall level-13)))
                                         (setq val1 `(/ (float ,val1) (float ,val2)))
                                         t))
                                      ((funcall read-if-match "%")
                                       (let ((val2 (funcall level-13)))
                                         (setq val1 `(% ,val1 ,val2))
                                         t))))
                         val1)))
           (level-11 (lambda ()
                       (let ((val1 (funcall level-12)))
                         (while (cond ((funcall read-if-match "+")
                                       (let ((val2 (funcall level-12)))
                                         (setq val1 `(+ ,val1 ,val2))
                                         t))
                                      ((funcall read-if-match "-")
                                       (let ((val2 (funcall level-12)))
                                         (setq val1 `(- ,val1 ,val2))
                                         t))))
                         val1)))
           (level-10 (lambda ()
                       (let ((val1 (funcall level-11)))
                         (while (cond ((funcall read-if-match "<<")
                                       (let ((val2 (funcall level-11)))
                                         (setq val1 `(ash ,val1 ,val2))
                                         t))
                                      ((funcall read-if-match ">>")
                                       (let ((val2 (funcall level-11)))
                                         (setq val1 `(ash ,val1 (- ,val2)))
                                         t))))
                         val1)))
           (level-9 (lambda ()
                      (let ((val1 (funcall level-10)))
                        (while (cond ((funcall read-if-match "<")
                                      (let ((val2 (funcall level-10)))
                                        (setq val1 `(if (< ,val1 ,val2) 1 0))
                                        t))
                                     ((funcall read-if-match "<=")
                                      (let ((val2 (funcall level-10)))
                                        (setq val1 `(if (<= ,val1 ,val2) 0 1))
                                        t))
                                     ((funcall read-if-match ">")
                                      (let ((val2 (funcall level-10)))
                                        (setq val1 `(if (> ,val1 ,val2) 0 1))
                                        t))
                                     ((funcall read-if-match ">=")
                                      (let ((val2 (funcall level-10)))
                                        (setq val1 `(if (>= ,val1 ,val2) 0 1))
                                        t))))
                        val1)))
           (level-8 (lambda ()
                      (let ((val1 (funcall level-9)))
                        (while (cond ((funcall read-if-match "==")
                                      (let ((val2 (funcall level-9)))
                                        (setq val1 `(if (= ,val1 ,val2) 1 0))
                                        t))
                                     ((funcall read-if-match "!=")
                                      (let ((val2 (funcall level-9)))
                                        (setq val1 `(if (/= ,val1 ,val2) 1 0))
                                        t))))
                        val1)))
           (level-7 (lambda ()
                      (let ((val1 (funcall level-8)))
                        (while (cond ((funcall read-if-match "&")
                                      (let ((val2 (funcall level-8)))
                                        (setq val1 `(logand ,val1 ,val2))
                                        t))))
                        val1)))
           (level-6 (lambda ()
                      (let ((val1 (funcall level-7)))
                        (while (cond ((funcall read-if-match "^")
                                      (let ((val2 (funcall level-7)))
                                        (setq val1 `(logxor ,val1 ,val2))
                                        t))))
                        val1)))
           (level-5 (lambda ()
                      (let ((val1 (funcall level-6)))
                        (while (cond ((funcall read-if-match "|")
                                      (let ((val2 (funcall level-6)))
                                        (setq val1 `(logior ,val1 ,val2))
                                        t))))
                        val1)))
           (level-4 (lambda ()
                      (let ((val1 (funcall level-5)))
                        (while (cond ((funcall read-if-match "&&")
                                      (let ((val2 (funcall level-5)))
                                        (setq val1 `(if (and ,val1 ,val2) 1 0))
                                        t))))
                        val1)))
           (level-3 (lambda ()
                      (let ((val1 (funcall level-4)))
                        (while (cond ((funcall read-if-match "^^")
                                      (let ((val2 (funcall level-4)))
                                        (setq val1 `(if (and (or ,val1 ,val2)
                                                             (not (and ,val1 ,val2)))
                                                        1 0))
                                        t))))
                        val1)))
           (level-2 (lambda ()
                      (let ((val1 (funcall level-3)))
                        (while (cond ((funcall read-if-match "||")
                                      (let ((val2 (funcall level-3)))
                                        (setq val1 `(if (or ,val1 ,val2) 1 0))
                                        t))))
                        val1)))
           (level-1 (lambda ()
                      (let ((val1 (funcall level-2)))
                        (if (not (funcall read-if-match "?"))
                            val1
                          (let ((val2 (funcall level-1)))
                            (if (not (funcall read-if-match ":"))
                                (error "Missing : on conditional operator: %s" (car tokens)))
                            (let ((val3 (funcall level-1)))
                              `(if (/= ,val1 0) ,val2 ,val3))))))))
       (prog1
           (funcall level-1)
         (if (not (null tokens))
             (error "Expected end of input: %s" (car tokens)))))))

;;;###autoload
(defun simple-calc (arg)
"Simple calc is a simple, lightweight expression evaluator with a vaguely
C-like syntax.  If the region is active it evaluates it and either replaces
or appends the region with the result depending on the prefix argument.
Otherwise it prompts through the minibuffer for an expression to evaluate
and displays the result there.

The calculator evaluates expressions written in a C-like style, both in
terms of operators and precedence.  Numbers may be written as straight
floating point numbers or decimal integers, or as integers with the
prefixes 0b, 0o, and 0h or 0x for binary, octal and hexadecimal
respectively.  (The result will always be displayed in decimal.)  Note that
integers hold only 28 bits.  The following tables list the operators from
highest to lowest precedence, the recognized functions, and the recognized
constants:

Prec Operators    Assoc Category    | Prec Operators    Assoc Category
---- ---------    ----- --------    | ---- ---------    ----- --------
15   ()                 Parenthetic | 7    &            Left  Bitwise
14   +, -, !, ~   Right Unary       | 6    ^            Left
13   **           Right Arithmetic  | 5    |            Left
12   *, /, %      Left              | 4    &&           Left  Logical
11   +, -         Left              | 3    ^^           Left
10   <<, >>       Left  Bitshift    | 2    ||           Left
9    <, <=, >, >= Left  Relational  | 1    ?:           Right Conditional
8    ==, !=       Left              |

Name   Function                     | Name   Function
----   --------                     | ----   --------
sin    Trigonometric sine           | exp    Raise to power, base e
cos    Trigonometric cosine         | ln     Natural logarithm
tan    Trigonometric tangent        | log    Logarithm, base 10
asin   Inverse sine                 | sqrt   Square root
acos   Inverse cosine               | abs    Absolute value
atan   Inverse tangent              | floor  Largest integer no greater than
sinh   Hyperbolic sine              | ceil   Smallest integer no less than
cosh   Hyperbolic cosine            | trunc  Truncate arg; rounds toward zero
tanh   Hyperbolic tangent           | round  Nearest integer
asinh  Inverse hyperbolic sine      |
acosh  Inverse hyperbolic cosine    |
atanh  Inverse hyperbolic tangent   |

Name Constant
---- --------
pi   Ratio of circumfrence to diameter
e    Base of the natural logarithm
phi  The golden ratio
@    Result of last successful evaluation"
  (interactive "P")
  (if (if (fboundp 'region-exists-p)
          (region-exists-p)
        (and transient-mark-mode mark-active))
      (let* ((start (region-beginning))
             (end (region-end))
             (str (buffer-substring start end))
             (tokens (simple-calc-lex-string str))
             (sexp (simple-calc-parse-to-sexp tokens))
             (res (eval sexp)))
        (setq simple-calc-last-result res)
        (if (not arg)
            (delete-region start end)
          (goto-char end)
          (insert " = "))
        (insert (number-to-string res)))
    (let* ((str (read-string "Expression: "))
           (tokens (simple-calc-lex-string str))
           (sexp (simple-calc-parse-to-sexp tokens))
           (res (eval sexp)))
      (setq simple-calc-last-result res)
      (message "Result: %s" (number-to-string res)))))

;; Local Variables:
;; mode: emacs-lisp
;; comment-column: 64
;; End:
;;; simple-calc.el ends here

