;;; goby-mode.el --- Major mode for the Goby programming language  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Goby contributors
;; Author: Goby contributors
;; URL: https://gitlab.com/yoshitsugu/goby
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages goby

;;; Commentary:

;; Provides syntax highlighting for Goby (.gb) source files.
;; Indentation and electric features are out of scope for this MVP release;
;; the focus is on stable font-lock coverage matching the canonical
;; TextMate grammar (tooling/syntax/textmate/goby.tmLanguage.json).
;;
;; Installation:
;;
;;   (add-to-list 'load-path "/path/to/goby/tooling/emacs")
;;   (require 'goby-mode)
;;
;; Or with use-package:
;;
;;   (use-package goby-mode
;;     :load-path "path/to/goby/tooling/emacs")

;;; Code:

(defconst goby-mode-keywords-control
  '("if" "else" "case" "with" "in" "resume" "can")
  "Goby control-flow and effect-application keywords.")

(defconst goby-mode-keywords-other
  '("type" "effect" "handler" "import" "as" "mut" "fn")
  "Goby declaration and module keywords.")

(defconst goby-mode-builtin-types
  '("Int" "String" "Bool" "Unit" "List")
  "Goby built-in type names.")

(defconst goby-mode-constants
  '("True" "False")
  "Goby boolean constants.")

(defconst goby-mode-font-lock-keywords
  (let ((kw-control-re (regexp-opt goby-mode-keywords-control 'words))
        (kw-other-re   (regexp-opt goby-mode-keywords-other   'words))
        (embed-re      (rx (or line-start (not (any "[:alnum:]_"))) "@embed" word-end))
        (builtin-re    (regexp-opt goby-mode-builtin-types    'words))
        (constants-re  (regexp-opt goby-mode-constants        'words))
        ;; UpperCamelCase: user-defined type / constructor names.
        ;; Placed after built-in types in the list so Int/String/etc. get
        ;; the more specific face first (font-lock applies rules in order
        ;; and by default does not re-fontify already-fontified text).
        (type-name-re  "\\b[A-Z][A-Za-z0-9_]*\\b")
        ;; Integer literals.
        (number-re     "\\b[0-9]+\\b")
        ;; Operators — longer alternatives first to avoid partial matches.
        ;; Multi-character operators must precede their single-character forms.
        (operator-re   (rx (or "->" "|>" "||" "&&" "==" "<=" ">="
                               "=" "<" ">" "+" "-" "*" "/" "%" "!" "|" ":"))))
    `(
      ;; Strings and comments are handled automatically via the syntax table.

      ;; Integer literals
      (,number-re      . font-lock-constant-face)
      ;; Boolean constants (before type-name-re to win the UpperCamelCase match)
      (,constants-re   . font-lock-constant-face)
      ;; Control keywords
      (,kw-control-re  . font-lock-keyword-face)
      ;; Stdlib-only embed declaration token
      (,embed-re       . font-lock-keyword-face)
      ;; Declaration / module keywords
      (,kw-other-re    . font-lock-keyword-face)
      ;; Built-in type names (before the catch-all UpperCamelCase rule)
      (,builtin-re     . font-lock-type-face)
      ;; User-defined type / constructor names (UpperCamelCase)
      (,type-name-re   . font-lock-type-face)
      ;; Operators — use font-lock-builtin-face (available since Emacs 22)
      ;; as a stand-in for operator face (font-lock-operator-face is Emacs 28+).
      (,operator-re    . font-lock-builtin-face)))
  "Font-lock keywords for `goby-mode'.")

(defvar goby-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; `#' starts a line comment; newline ends it.
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Double-quote delimits strings.
    (modify-syntax-entry ?\" "\"" table)
    ;; Backslash is an escape character inside strings.
    (modify-syntax-entry ?\\ "\\" table)
    ;; Parentheses and brackets.
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    ;; Underscore is a word constituent (identifiers and the `_' wildcard).
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table for `goby-mode'.")

;;;###autoload
(define-derived-mode goby-mode prog-mode "Goby"
  "Major mode for editing Goby (.gb) source files.

Provides syntax highlighting for keywords, types, operators,
string literals, and comments.  Indentation is not yet implemented
in this MVP release."
  :syntax-table goby-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(goby-mode-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gb\\'" . goby-mode))

(provide 'goby-mode)

;;; goby-mode.el ends here
