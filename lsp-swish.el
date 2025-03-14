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

(require 'lsp-mode)

(defun swish-indent-sexp ()
  "Indent each line of the sexp starting just after point."
  (interactive)
  (save-excursion
    (let ((start (point)))
      (forward-sexp 1)
      (lsp-format-region start (point)))))

(defun swish-indent-line ()
  "Indent current line as Scheme code."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (end-of-line)
      (lsp-format-region start (point))))
  (skip-chars-forward " \t"))

(add-to-list 'lsp-language-id-configuration '(scheme-mode . "scheme"))

(defgroup lsp-swish-semantic-tokens nil
  "LSP semantic tokens support for swish-lint."
  :group 'lsp-mode
  :link '(url-link "https://github.com/becls/swish-lint")
  :package-version '(lsp-mode . "9.0.0"))

(defface swish-internal-modifier-face
  '((t :inherit default))
  "Face for internal system procedure modifier"
  :group 'lsp-swish-semantic-tokens
  )

(defface swish-optimize2-modifier-face
  '((t :inherit default))
  "Face for optimize level 2 modifier"
  :group 'lsp-swish-semantic-tokens
  )

(defface swish-optimize3-modifier-face
  '((t :inherit default))
  "Face for optimize level 3 modifier"
  :group 'lsp-swish-semantic-tokens
  )

(defface swish-side-effect-modifier-face
  '((t :inherit default))
  "Face for side-effect(!) procedure modifier"
  :group 'lsp-swish-semantic-tokens
  )

(defcustom swish-internal-modifier 'swish-internal-modifier-face
  "Face for semantic token modifier for `internal` code."
  :type 'face
  :group 'lsp-swish-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom swish-optimize2-modifier 'swish-optimize2-modifier-face
  "Face for semantic token modifier for `optimize-level 2` code."
  :type 'face
  :group 'lsp-swish-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom swish-optimize3-modifier 'swish-optimize3-modifier-face
  "Face for semantic token modifier for `optimize-level 3` code."
  :type 'face
  :group 'lsp-swish-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom swish-side-effect-modifier 'swish-side-effect-modifier-face
  "Face for semantic token modifier for side-effect(!) code."
  :type 'face
  :group 'lsp-swish-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

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
  "Mapping between swish-lint tokens and fonts to apply."
  :group 'lsp-swish
  :type '(alist :key-type string :value-type face)
  :package-version '(lsp-mode . "8.1"))

(defcustom lsp-swish-semantic-token-modifier-faces
  `(("internal" . ,swish-internal-modifier)
    ("optimize2" . ,swish-optimize2-modifier)
    ("optimize3" . ,swish-optimize3-modifier)
    ("side-effect" . ,swish-side-effect-modifier))
  "Mapping between swish-lint modifiers and fonts to apply."
  :group 'lsp-swish
  :type '(alist :key-type string :value-type face)
  :package-version '(lsp-mode . "8.1"))

(lsp-defcustom lsp-swish-semtok-mode "full"
  "The way swish-lint processes semantic tokens."
  :type '(choice
          (const "no-modifiers")
          (const "full"))
  :group 'lsp-swish
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "swish.semtok-mode")

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   '("swish-lint" "--lsp"))
  :major-modes '(scheme-mode)
  :server-id 'swish-ls
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace
                        (lsp--set-configuration (lsp-configuration-section "swish"))))
  :synchronize-sections '("swish")
  :semantic-tokens-faces-overrides
  `(:discard-default-modifiers t
    :discard-default-types t
    :modifiers ,lsp-swish-semantic-token-modifier-faces
    :types ,lsp-swish-semantic-token-faces)
  ))

(add-hook 'scheme-mode-hook #'lsp)

(provide 'lsp-swish)
