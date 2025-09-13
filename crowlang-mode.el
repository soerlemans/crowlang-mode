;;; crowlang-mode.el --- Major mode for the Crow programming language.
;; -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2025  soerlemans

;; Author: soerlemans <https://github.com/soerlemans>
;; Keywords: languages crow
;; URL: https://github.com/soerlemans/crowlang-mode
;;
;; This file is not part of GNU Emacs.

;; MIT License
;;
;; Copyright (c) 2025 soerlemans
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; TODO: Write Commentary

;;; Code:
(require 'generic-x)

;;; Variables:
(setq crowlang--comments
      (list
       "//"
       '("/*" . "*/" )
       )
      ;; "List of character sequences for detecting comments."
      )

(setq crowlang--keywords
      (list
       ;; Variables:
       "let"
       "var"

       ;; Package:
       ;; TODO: Implement when stable.
       "module"
       "import"
       "export"

       ;; Control Statements:
       "func"
       "match"
       "if"
       "else"
       "elif"
       "loop"

			 ;; Typing:
			 "struct"
			 "self"
			 "enum"
			 "interface"

			 ;; Meta:
       "declare"
       "macro"

       ;; Jump:
       "break"
       "continue"
       "defer"
       "return"
       )
      ;; "List of keywords for crowlang."
      )

(setq crowlang--font-lock-defaults
      '(
				;; Types:
				("\\<void\\>" . font-lock-type-face)

				("\\<f32\\>" . font-lock-type-face)
				("\\<f64\\>" . font-lock-type-face)

				("\\<int\\>" . font-lock-type-face)
				("\\<i8\\>" . font-lock-type-face)
				("\\<i16\\>" . font-lock-type-face)
				("\\<i32\\>" . font-lock-type-face)
				("\\<i64\\>" . font-lock-type-face)
				("\\<isize\\>" . font-lock-type-face)

				("\\<uint\\>" . font-lock-type-face)
				("\\<u8\\>" . font-lock-type-face)
				("\\<u16\\>" . font-lock-type-face)
				("\\<u32\\>" . font-lock-type-face)
				("\\<u64\\>" . font-lock-type-face)
				("\\<usize\\>" . font-lock-type-face)

				("\\<string\\>" . font-lock-type-face)
				("\\<bool\\>" . font-lock-type-face)

				;;
				("\\<\\(TODO\\|FIXME\\|IMPORTANT\\)" . font-lock-warning-face)

				;; Builin functions:
				("\\<println\\|print\\>" . font-lock-builtin-face)

				("^#[a-zA-Z_][a-zA-Z0-9_]*". font-lock-preprocessor-face)

				;; TODO: Figure these out:
				;; ("=" . font-lock-operator)

				;; Attributes
				;; TODO: We still need to highlight all the attribute names.
				;; Instead of just the first one.
				("^\\[\\[[ \t]*\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*\\(?:([^)]*)\\)?[ \t]*\\(?:, ?\\|\\]\\]\\)" 1 font-lock-preprocessor-face)
				;; ("^\\[\\[" 1 font-lock-preprocessor-face)

				;;("\\[\\[\\(.*?\\)\\]\\]" 1 font-lock-preprocessor-face)
				;; ("\\[\\[[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\(?:\\s-*(.*?)\\)?\\s-*\\(?:,\\|\\]\\]\\)" 1 font-lock-preprocessor-face)

				;; Constants
				("\\<\\(True\\|False\\)\\>" . font-lock-constant-face)

				("\"\\.\\*\\?" . font-lock-string-face)

				;; (,(regexp-opt '("void"
				;; 								"f32" "f64"
				;; 								"int"  "i8" "i16" "i32" "i64" "isize"
				;; 								"uint" "u8" "u16" "u32" "u64" "usize"
				;; 								"string"
				;; 								"bool") 'words) . font-lock-type-face)

				;; TODO: Simplify
				;; ("\\([+\\-*/%^&|<>!=]=?\\|==\\|!=\\|&&\\|\\|\\|\\?<\\|>\\|<<\\|>>\\)" . font-lock-operator-face)

				;; Typing:
				("enum[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-type-face)
				("struct[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-type-face)
				("interface[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-type-face)

				;; Functions:
				("func[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-function-name-face)

				;; Variable names
				("\\<[a-zA-Z_][a-zA-Z0-9_]*\\>" . font-lock-variable-name-face)
				)
      ;;  "List of font lock settings for crowlang."
      )

;;; Functions:
(defun crowlang--calculate-indentation ()
  "Return the column to which the current line should be indented."
  (* tab-width (min (car (syntax-ppss (line-beginning-position)))
                    (car (syntax-ppss (line-end-position))))))

(defun crowlang--indent-line ()
  "Indent current line of Crowlang code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (crowlang--calculate-indentation) 0)
                  (error 0))))
    (if savep
				(save-excursion (indent-line-to indent))
      (indent-line-to indent))
    ))

;;; Define generic mode:
(define-generic-mode 'crowlang-mode
  crowlang--comments           ; Comments.
  crowlang--keywords           ; Keywords.
  crowlang--font-lock-defaults ; Font-lock settings.
  '("\\.cw$")                  ; Activate the mode only for crowlang source files.
  (list		               ; function list, for indentation.
   (lambda ()
     (setq-local tab-width 2)
     (setq-local indent-tabs-mode nil)
     (setq-local indent-line-function 'crowlang--indent-line)))
  "Major mode for the Crow programming language."
  )

(provide 'crowlang-mode)
;;; crowlang-mode.el ends here
