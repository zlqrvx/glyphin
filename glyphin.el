;;; glyphin --- Unicode input mode -*- coding: utf-8; lexical-binding: t; -*-

;; Package-Requires: (cl-lib)

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defvar gl-completion-table
  (make-hash-table :test 'equal)
  "The hash table containing all the glyphin completions.")

(cl-defstruct gl-hash-entry
  (str :type 'string)
  (replacement :type (or string nil))
  (n-continuations :type 'integer))

(defun gl-compile--collect-prefixes (str)
  (cl-loop for i below (1- (length str))
           collect (reverse (substring (reverse str) 0 (1+ i)))))

(defun gl-compile--put-prefix-hash (prefix hash)
  (if (gethash prefix hash)
      (cl-incf (slot-value (gethash prefix hash) 'n-continuations))
    (puthash prefix
             (make-glyphin-hash-entry
              :str prefix
              :replacement nil
              :n-continuations 1)
             hash)))

(defun gl-compile--put-keyword-hash (kw rep hash)
  (if (gethash kw hash)
      (progn
        (setf (slot-value (gethash kw hash) 'replacement) rep)
        (cl-incf (slot-value (gethash kw hash) 'n-continuations)))
    (puthash kw
             (make-glyphin-hash-entry
              :str kw
              :replacement rep
              :n-continuations 0)
             hash)))

(defun gl-compile--compile-keyword (kw rep hash)
  (gl-compile--put-keyword-hash kw rep hash)
  (dolist (p (gl-compile--collect-prefixes kw))
    (gl-compile--put-prefix-hash p hash)))

(defun gl--defkeyword (kw-val-pair)
  (gl-compile--compile-keyword (car kw-val-pair) (cadr kw-val-pair) gl-completion-table))

(defmacro gl-defkeywords (&rest kwds)
  `(mapc 'gl--defkeyword ',(apply #'list kwds)))


;; Greek
(gl-defkeywords
 ("a" "α")
 ("b" "β")
 ("c" "χ")
 ("d" "δ")
 ("e" "ε")
 ("f" "φ")
 ("g" "γ")
 ("h" "η")
 ("i" "ι")
 ;; j
 ("k" "k")
 ("l" "λ")
 ("m" "μ")
 ("n" "ν")
 ;; o
 ("p" "π")
 ("q" "θ")
 ("r" "ρ")
 ("s" "σ")
 ("t" "τ")
 ;; u
 ("v" "υ")
 ("w" "ω")
 ("x" "ξ")
 ("y" "ψ")
 ("z" "ζ")
 )

;; Blackboard bold
(gl-defkeywords
 ("bbZ" "ℤ")
 ("bbR" "ℝ")
 ("bbC" "ℂ")
 ("bbQ" "ℚ")
 ("bbH" "ℍ")
 )

;; Math
(gl-defkeywords
 ("inf" "∞")
 ("prop" "∝")
 ("in" "∈")
 ("nin" "∉")
 ("fa" "∀")
 ("ex" "∃")
 ("partial" "∂")
 ("pd" "∂")
 ("del" "∇")
 ("nabla" "∇")
 ("sqrt" "√")
 ("sum" "∑")
 ("prod" "∏")
 ("int" "∫")
 ("times" "×")
 ("divide" "÷")
 ("sub" "⊂")
 ("sup" "⊃")
 ("sube" "⊆")
 ("supe" "⊇")
 ("le" "≤")
 ("ge" "≥")
 ("..." "…")
 ("otimes" "⊗")
 ("oplus" "⊕")
 ("bot" "⊥")
 ("top" "⊤")
 ("|-" "⊢")
 ("-|" "⊣")
 ("and" "∧")
 ("or" "∨")
 ("not" "¬")
 ("+-" "±")
 ("." "⋅")
 ("circ" "∘")
 ("union" "∪")
 ("intersection" "∩")
 )



(defun gl--get-completion (kw hash)
  "Returns pair of values: car being the replacement, cdr being bool indicating whether it is a final completion."
  (let ((comp (gethash kw hash)))
    (if comp
        (cl-values 
         (slot-value comp 'replacement)
         (slot-value comp 'n-continuations))
      (cl-values nil 0))))

(cl-defun gl-complete ()
  "Find the current completion at point."
  (interactive)
  (cl-block top
    (save-excursion
      (let ((start (point))
            (end (point))
            (str "")
            (completion (list nil (point))))
        (while t
          (setq start (- start 1))
          (setq str (buffer-substring-no-properties start end))
          (cl-multiple-value-bind
              (rep n-cont) (gl--get-completion str gl-completion-table)
            (cond
             ((and rep (= n-cont 0))
              (progn
                (kill-region start end)
                (insert-before-markers rep)
                (cl-return-from top t)))
             ((and rep (> n-cont 0))
              (setq completion (list rep start)))
             ((and (not rep) (= n-cont 0) (car completion))
              (let ((rep (car completion))
                    (start (cadr completion)))
                (kill-region start end)
                (insert-before-markers rep)
                (cl-return-from top t)))
             ;; Exit condition
             ((and (= n-cont 0) (not (car completion)))
              (cl-return-from top nil)))))))))



(defvar gl-keymap nil "Glyphin keymap.")

(progn
  (setq gl-keymap (make-sparse-keymap))
  (define-key gl-keymap (kbd "S-SPC") 'gl-complete))


;;;###autoload
(define-globalized-minor-mode global-glyphin-mode gl-mode gl-mode-on)

;;;###autoload
(defun gl-mode-on ()
  "Turn on `glyphin-mode' in current buffer."
  (interactive)
  (gl-mode 1))

;;;###autoload
(defun gl-mode-off ()
  "Turn off `glyphin-mode' in current buffer."
  (interactive)
  (gl-mode 0))

;;;###autoload
(define-minor-mode gl-mode
  "Toggle glyphin mode."
  :init-value nil
  :global nil
  :lighter " gl"
  :keymap gl-keymap)


(provide 'glyphin)
;;; glyphin.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("gl-" . "glyphin-"))
;; End:
