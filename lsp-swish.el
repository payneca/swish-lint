;;; lsp-swish.el --- Swish-Lint configuration -*- lexical-binding: t; -*-
;;;
;;; Copyright 2020 Beckman Coulter, Inc.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;;; URL: https://github.com/becls/swish-lint
;;; Keywords: languages, tools
;;; Package-Requires: ((emacs "31.1") (lsp-mode "10.0.0"))

;;; Commentary:

;;; Registers Swish-Lint with lsp-mode as the language server for Swish
;;; and Chez Scheme code.  See README.md for setup.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-swish nil
  "LSP support for Swish and Chez Scheme, using Swish-Lint."
  :group 'lsp-mode
  :link '(url-link "https://github.com/becls/swish-lint")
  :prefix "lsp-swish-")

(defcustom lsp-swish-server-command '("swish-lint" "--lsp")
  "Command and arguments that start the Swish-Lint language server."
  :type '(repeat string)
  :risky t
  :group 'lsp-swish)

(defun lsp-swish-indent-sexp ()
  "Indent each line of the sexp starting just after point."
  (interactive)
  (save-excursion
    (let ((start (point)))
      (forward-sexp 1)
      (lsp-format-region start (point)))))

(add-to-list 'lsp-language-id-configuration '(scheme-mode . "scheme"))

(defgroup lsp-swish-semantic-tokens nil
  "Semantic tokens from Swish-Lint."
  :group 'lsp-swish
  :prefix "lsp-swish-")

(defface lsp-swish-internal-modifier-face
  '((t :inherit default))
  "Face for the `internal' modifier, which marks a #% primitive."
  :group 'lsp-swish-semantic-tokens)

(defface lsp-swish-optimize2-modifier-face
  '((t :inherit default))
  "Face for the `optimize2' modifier, which marks a #2% primitive."
  :group 'lsp-swish-semantic-tokens)

(defface lsp-swish-optimize3-modifier-face
  '((t :inherit default))
  "Face for the `optimize3' modifier, which marks a #3% primitive."
  :group 'lsp-swish-semantic-tokens)

(defface lsp-swish-side-effect-modifier-face
  '((t :inherit default))
  "Face for the `side-effect' modifier, which marks a name ending in !."
  :group 'lsp-swish-semantic-tokens)

(defcustom lsp-swish-semantic-token-faces
  '(("comment" . lsp-face-semhl-comment)
    ("function" . lsp-face-semhl-function)
    ("keyword" . lsp-face-semhl-keyword)
    ("macro" . lsp-face-semhl-macro)
    ("number" . lsp-face-semhl-number)
    ("regexp" . lsp-face-semhl-regexp)
    ("string" . lsp-face-semhl-string)
    ("type" . lsp-face-semhl-type)
    ("variable" . lsp-face-semhl-variable))
  "Faces for the semantic token types that Swish-Lint reports.
Set this before loading `lsp-swish'."
  :type '(alist :key-type string :value-type face)
  :group 'lsp-swish-semantic-tokens)

(defcustom lsp-swish-semantic-token-modifier-faces
  '(("internal" . lsp-swish-internal-modifier-face)
    ("optimize2" . lsp-swish-optimize2-modifier-face)
    ("optimize3" . lsp-swish-optimize3-modifier-face)
    ("side-effect" . lsp-swish-side-effect-modifier-face))
  "Faces for the semantic token modifiers that Swish-Lint reports.
Set this before loading `lsp-swish'."
  :type '(alist :key-type string :value-type face)
  :group 'lsp-swish-semantic-tokens)

(lsp-defcustom lsp-swish-semtok-mode "full"
  "Which semantic tokens Swish-Lint reports.
\"full\" reports every token, and \"no-modifiers\" leaves out the tokens
that have modifiers."
  :type '(choice
          (const "no-modifiers")
          (const "full"))
  :group 'lsp-swish-semantic-tokens
  :lsp-path "swish.semtok-mode")

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-swish-server-command))
  :activation-fn (lsp-activate-on "scheme")
  :server-id 'swish-ls
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "swish"))))
  :synchronize-sections '("swish")
  :semantic-tokens-faces-overrides
  `(:discard-default-modifiers t
    :discard-default-types t
    :modifiers ,lsp-swish-semantic-token-modifier-faces
    :types ,lsp-swish-semantic-token-faces)))

(lsp-consistency-check lsp-swish)

(provide 'lsp-swish)
;;; lsp-swish.el ends here
