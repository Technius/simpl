;;; simpl-mode.el --- Provides utilities for SimPL

;; Copyright (C)
;;   2019 Bryan Tan
;; Author: Bryan Tan <techniux@gmail.com>
;; Version: 0.0.1

;; To use this, add the following to your init.el:
;;     (add-to-list 'auto-mode-alist '("\\.spl" . simpl-mode))

(defvar simpl-mode-hook nil)
(add-to-list 'auto-mode-alist '("\\.spl\\'" . simpl-mode))

(defconst simpl-font-lock-keywords
  (let ((keywords '("fun" "data" "if" "then" "else" "case" "of" "let" "in" "cast" "as"))
        (builtins '("true" "false" "println")))
    `(("\\<fun[ \t]+\\([[:lower:]_][[:alnum:]_]*\\)\\>" . (1 font-lock-function-name-face))
      (,(regexp-opt keywords 'words) . font-lock-keyword-face)
      (,(regexp-opt builtins 'words) . font-lock-builtin-face)
      ("\\<\\([[:upper:]][[:alnum:]]*\\)\\>" . font-lock-type-face)))
  "Keyword highlighting for simpl-mode")

(defun simpl-indent-line ()
  "Indent line of SimPL code"
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*}")
          (setq cur-indent 0)
        ())
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

(defconst simpl-mode-syntax-table
    (let ((table (make-syntax-table)))
      ;; _ is part of a word
      (modify-syntax-entry ?_ "w" table)
      ;; " is a string delimiter
      (modify-syntax-entry ?\" "\"" table)
      ;; # is a comment
      (modify-syntax-entry ?# "<" table)
      table)
  "Syntax table for simpl-mode")

(define-derived-mode simpl-mode prog-mode "SimPL Mode"
  :syntax-table simpl-mode-syntax-table
  (setq font-lock-defaults '((simpl-font-lock-keywords)))
  (setq font-lock-keywords-case-fold-search nil))

(provide 'simpl-mode)
