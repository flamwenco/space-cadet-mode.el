;;; space-cadet-mode.el --- Adds Space Cadet inspired Greek and Math layer mappings to Emacs as a minor mode

;; Copyright (C) 2015-2016  Daniel Muckerman

;; Author: Daniel Muckerman <danielmuckerman@me.com>
;; Maintainer: Daniel Muckerman <danielmuckerman@me.com>
;; Version: 1.0
;; Homepage: https://github.com/DMuckerman/space-cadet-mode.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defgroup space-cadet nil
  "Make typing Greek and Math characters easier."
  :group 'convenience)

(make-variable-buffer-local
 (defvar greek-enabled 0
   "If greek typing is enabled/disabled in the current buffer."))

(make-variable-buffer-local
 (defvar math-enabled 0
   "If math typing is enabled/disabled in the current buffer."))

(defcustom space-cadet-org-latex-chars nil
  "Use LaTeX-style Greek/Math chars while in Org/LaTeX modes."
  :group 'space-cadet)

(defcustom space-cadet-latex-modes-alist
  '((org-mode . "org")
    (LaTeX-mode . "LaTeX")
    (latex-mode . "latex"))
  "List which contains the major modes to print LaTeX symbols in when space-cadet-org-latex-chars is set to t."
  :type '(repeat (cons (symbol :tag "Major mode name")
                       (string :tag "This currently does nothing")))
  :group 'space-cadet)

(defun space-cadet-guess-major-mode ()
  (interactive)
  (car (assoc major-mode
	      space-cadet-latex-modes-alist)))

;; Toggle the Greek layer when F17 is pressed
(defun toggle-greek ()
  (interactive)
  (setq greek-enabled (if (= greek-enabled 1) 0 1)))

;; Toggle the Math layer when F18 is pressed
(defun toggle-math ()
  (interactive)
  (setq math-enabled (if (= math-enabled 1) 0 1)))

;; Print a character based on the current keyboard layer
(defun space-cadet-insert-char (greek math old)
  (interactive)
  (if (= greek-enabled 1)
      (progn
	(insert greek)
	(setq greek-enabled (if (= greek-enabled 1) 0 1)))
    (if (= math-enabled 1)
	(progn
	  (insert math)
	  (setq math-enabled (if (= math-enabled 1) 0 1)))
      (insert old)))
  (if selectric-mode
      (selectric-type-sound)))

(defun insert-lower-alpha ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\alpha" "\\land" "a")
    (space-cadet-insert-char "α" "∧" "a")))

(defun insert-upper-alpha ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Alpha" "\\aleph" "A")
    (space-cadet-insert-char "Α" "ℵ" "A")))

(defun insert-lower-beta ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\beta" "" "b")
    (space-cadet-insert-char "β" "" "b")))

(defun insert-upper-beta ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Beta" "" "B")
    (space-cadet-insert-char "Β" "" "B")))

(defun insert-lower-chi ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\chi" "\\circ" "c")
    (space-cadet-insert-char "χ" "∘" "c")))

(defun insert-upper-chi ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Chi" "\\C" "C")
    (space-cadet-insert-char "Χ" "ℂ" "C")))

(defun insert-lower-delta ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\delta" "" "d")
    (space-cadet-insert-char "δ" "" "d")))

(defun insert-upper-delta ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Delta" "" "D")
    (space-cadet-insert-char "Δ" "" "D")))

(defun insert-lower-epsilon ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\epsilon" "\\in" "e")
    (space-cadet-insert-char "ε" "∈" "e")))
  
(defun insert-upper-epsilon ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Epsilon" "\\notin" "E")
    (space-cadet-insert-char "Ε" "∉" "E")))

(defun insert-lower-phi ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\phi" "\\int" "f")
    (space-cadet-insert-char "φ" "∫" "f")))
  
(defun insert-upper-phi ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Phi" "" "F")
    (space-cadet-insert-char "Φ" "" "F")))

(defun insert-lower-gamma ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\gamma" "" "g")
    (space-cadet-insert-char "γ" "" "g")))
  
(defun insert-upper-gamma ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Gamma" "" "G")
    (space-cadet-insert-char "Γ" "" "G")))

(defun insert-lower-eta ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\eta" "" "h")
    (space-cadet-insert-char "η" "" "h")))
  
(defun insert-upper-eta ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Eta" "" "H")
    (space-cadet-insert-char "Η" "" "H")))

(defun insert-lower-iota ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\iota" "\\cap" "i")
    (space-cadet-insert-char "ι" "∩" "i")))
  
(defun insert-upper-iota ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Iota" "\\infty" "I")
    (space-cadet-insert-char "Ι" "∞" "I")))

(defun insert-lower-theta ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\vartheta" "" "j")
    (space-cadet-insert-char "ϑ" "" "j")))
  
(defun insert-upper-theta ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Theta" "" "J")
    (space-cadet-insert-char "Θ" "" "J")))

(defun insert-lower-kappa ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\kappa" "" "k")
    (space-cadet-insert-char "κ" "" "k")))

(defun insert-upper-kappa ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Kappa" "" "K")
    (space-cadet-insert-char "Κ" "" "K")))

(defun insert-lower-lambda ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\lambda" "" "l")
    (space-cadet-insert-char "λ" "" "l")))

(defun insert-upper-lambda ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Lambda" "" "L")
    (space-cadet-insert-char "Λ" "" "L")))

(defun insert-lower-mu ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\mu" "" "m")
    (space-cadet-insert-char "μ" "" "m")))

(defun insert-upper-mu ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Mu" "" "M")
    (space-cadet-insert-char "Μ" "" "M")))

(defun insert-lower-nu ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\nu" "" "n")
    (space-cadet-insert-char "ν" "" "n")))
  
(defun insert-upper-nu ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Nu" "\\N" "N")
    (space-cadet-insert-char "Ν" "ℕ" "N")))

(defun insert-lower-omicron ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\omicron" "\\lor" "o")
    (space-cadet-insert-char "ο" "∨" "o")))

(defun insert-upper-omicron ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Omicron" "" "O")
    (space-cadet-insert-char "Ο" "" "O")))

(defun insert-lower-pi ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\pi" "" "p")
    (space-cadet-insert-char "π" "" "p")))
  
(defun insert-upper-pi ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Pi" "" "P")
    (space-cadet-insert-char "Π" "" "P")))

(defun insert-lower-theta2 ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\theta" "" "q")
    (space-cadet-insert-char "θ" "" "q")))
  
(defun insert-upper-theta2 ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Theta" "" "Q")
    (space-cadet-insert-char "Θ" "" "Q")))

(defun insert-lower-rho ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\rho" "\\sqrt" "r")
    (space-cadet-insert-char "ρ" "√" "r")))
  
(defun insert-upper-rho ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Rho" "\\R" "R")
    (space-cadet-insert-char "Ρ" "ℝ" "R")))

(defun insert-lower-sigma ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\sigma" "" "s")
    (space-cadet-insert-char "σ" "" "s")))

(defun insert-upper-sigma ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Sigma" "" "S")
    (space-cadet-insert-char "Σ" "" "S")))

(defun insert-lower-tau ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\tau" "" "t")
    (space-cadet-insert-char "τ" "" "t")))
  
(defun insert-upper-tau ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Tau" "" "T")
    (space-cadet-insert-char "Τ" "" "T")))

(defun insert-lower-upsilon ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\upsilon" "\\cup" "u")
    (space-cadet-insert-char "υ" "∪" "u")))

(defun insert-upper-upsilon ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Upsilon" "" "U")
    (space-cadet-insert-char "Υ" "" "U")))

(defun insert-lower-sigma2 ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\varsigma" "" "v")
    (space-cadet-insert-char "ς" "" "v")))

(defun insert-upper-sigma2 ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Sigma" "" "V")
    (space-cadet-insert-char "Σ" "" "V")))

(defun insert-lower-omega ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\omega" "" "w")
    (space-cadet-insert-char "ω" "" "w")))

(defun insert-upper-omega ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Omega" "" "W")
    (space-cadet-insert-char "Ω" "" "W")))

(defun insert-lower-xi ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\xi" "\\underline{\\lor}" "x")
    (space-cadet-insert-char "ξ" "⊻" "x")))
  
(defun insert-upper-xi ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Xi" "" "X")
    (space-cadet-insert-char "Ξ" "" "X")))

(defun insert-lower-psi ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\psi" "" "y")
    (space-cadet-insert-char "ψ" "" "y")))

(defun insert-upper-psi ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Psi" "" "Y")
    (space-cadet-insert-char "Ψ" "" "Y")))

(defun insert-lower-zeta ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\zeta" "" "z")
    (space-cadet-insert-char "ζ" "" "z")))
  
(defun insert-upper-zeta ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "\\Zeta" "" "Z")
    (space-cadet-insert-char "Ζ" "ℤ" "Z")))

(defun insert-lower-minus ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\neg" "-")
    (space-cadet-insert-char "" "¬" "-")))

(defun insert-lower-equals ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\neq" "=")
    (space-cadet-insert-char "" "≠" "=")))

(defun insert-upper-equals ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\pm" "+")
    (space-cadet-insert-char "" "±" "+")))

(defun insert-lower-divide ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\div" "/")
    (space-cadet-insert-char "" "÷" "/")))

(defun insert-upper-comma ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\leq" "<")
    (space-cadet-insert-char "" "≤" "<")))

(defun insert-upper-period ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\geq" ">")
    (space-cadet-insert-char "" "≥" ">")))

(defun insert-upper-eight ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\times" "*")
    (space-cadet-insert-char "" "×" "*")))

(defun insert-lower-zero ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\empty" "0")
    (space-cadet-insert-char "" "∅" "0")))

(defun insert-upper-tilde ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\approx" "~")
    (space-cadet-insert-char "" "≈" "~")))

(defun insert-lower-rbracket ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\sup" "]")
    (space-cadet-insert-char "" "⊃" "]")))

(defun insert-upper-rbracket ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\not\\sup" "}")
    (space-cadet-insert-char "" "⊅" "}")))

(defun insert-lower-lbracket ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\sub" "[")
    (space-cadet-insert-char "" "⊂" "[")))

(defun insert-upper-lbracket ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\not\\sub" "{")
    (space-cadet-insert-char "" "⊄" "{")))

(defun insert-lower-left ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\gets" "")
    (space-cadet-insert-char "" "←" "")))

(defun insert-upper-left ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\not\\gets" "")
    (space-cadet-insert-char "" "↚" "")))

(defun insert-lower-right ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\to" "")
    (space-cadet-insert-char "" "→" "")))

(defun insert-upper-right ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\not\\to" "")
    (space-cadet-insert-char "" "↛" "")))

(defun insert-lower-up-down ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\leftrightarrow" "")
    (space-cadet-insert-char "" "↔" "")))

(defun insert-upper-up-down ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\not\\leftrightarrow" "")
    (space-cadet-insert-char "" "↮" "")))

(defun insert-ctrl-lower-rbracket ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\supseteq" "")
    (space-cadet-insert-char "" "⊇" "")))

(defun insert-ctrl-upper-rbracket ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\not\\supseteq" "")
    (space-cadet-insert-char "" "⊉" "")))

(defun insert-ctrl-lower-lbracket ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\subseteq" "")
    (space-cadet-insert-char "" "⊆" "")))

(defun insert-ctrl-upper-lbracket ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\not\\supseteq" "")
    (space-cadet-insert-char "" "⊈" "{")))

(defun insert-ctrl-lower-left ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\Leftarrow" "")
    (space-cadet-insert-char "" "⇐" "")))

(defun insert-ctrl-upper-left ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\not\\Leftarrow" "")
    (space-cadet-insert-char "" "⇍" "")))

(defun insert-ctrl-lower-right ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\Rightarrow" "")
    (space-cadet-insert-char "" "⇒" "")))

(defun insert-ctrl-upper-right ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\not\\Rightarrow" "")
    (space-cadet-insert-char "" "⇏" "")))

(defun insert-ctrl-lower-up-down ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\Leftrightarrow" "")
    (space-cadet-insert-char "" "⇔" "")))

(defun insert-ctrl-upper-up-down ()
  (interactive)
  (if (and space-cadet-org-latex-chars (derived-mode-p (space-cadet-guess-major-mode)))
      (space-cadet-insert-char "" "\\not\\Leftrightarrow" "")
    (space-cadet-insert-char "" "⇎" "")))

(define-minor-mode space-cadet-mode
  "Map Greek and Math layers to F17 and F18 respectively, for a modern Space Cadet experience"
  :lighter " space"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "<f17>") 'toggle-greek)
	    (define-key map (kbd "<f18>") 'toggle-math)
	    (define-key map (kbd "a") 'insert-lower-alpha)
	    (define-key map (kbd "\S-a") 'insert-upper-alpha)
	    (define-key map (kbd "b") 'insert-lower-beta)
	    (define-key map (kbd "\S-b") 'insert-upper-beta)
	    (define-key map (kbd "c") 'insert-lower-chi)
	    (define-key map (kbd "\S-c") 'insert-upper-chi)
	    (define-key map (kbd "d") 'insert-lower-delta)
	    (define-key map (kbd "\S-d") 'insert-upper-delta)
	    (define-key map (kbd "e") 'insert-lower-epsilon)
	    (define-key map (kbd "\S-e") 'insert-upper-epsilon)
	    (define-key map (kbd "f") 'insert-lower-phi)
	    (define-key map (kbd "\S-f") 'insert-upper-phi)
	    (define-key map (kbd "g") 'insert-lower-gamma)
	    (define-key map (kbd "\S-g") 'insert-upper-gamma)
	    (define-key map (kbd "h") 'insert-lower-eta)
	    (define-key map (kbd "\S-h") 'insert-upper-eta)
	    (define-key map (kbd "i") 'insert-lower-iota)
	    (define-key map (kbd "\S-i") 'insert-upper-iota)
	    (define-key map (kbd "j") 'insert-lower-theta)
	    (define-key map (kbd "\S-j") 'insert-upper-theta)
	    (define-key map (kbd "k") 'insert-lower-kappa)
	    (define-key map (kbd "\S-k") 'insert-upper-kappa)
	    (define-key map (kbd "l") 'insert-lower-lambda)
	    (define-key map (kbd "\S-l") 'insert-upper-lambda)
	    (define-key map (kbd "m") 'insert-lower-mu)
	    (define-key map (kbd "\S-m") 'insert-upper-mu)
	    (define-key map (kbd "n") 'insert-lower-nu)
	    (define-key map (kbd "\S-n") 'insert-upper-nu)
	    (define-key map (kbd "o") 'insert-lower-omicron)
	    (define-key map (kbd "\S-o") 'insert-upper-omicron)
	    (define-key map (kbd "p") 'insert-lower-pi)
	    (define-key map (kbd "\S-p") 'insert-upper-pi)
	    (define-key map (kbd "q") 'insert-lower-theta2)
	    (define-key map (kbd "\S-q") 'insert-upper-theta2)
	    (define-key map (kbd "r") 'insert-lower-rho)
	    (define-key map (kbd "\S-r") 'insert-upper-rho)
	    (define-key map (kbd "s") 'insert-lower-sigma)
	    (define-key map (kbd "\S-s") 'insert-upper-sigma)
	    (define-key map (kbd "t") 'insert-lower-tau)
	    (define-key map (kbd "\S-t") 'insert-upper-tau)
	    (define-key map (kbd "u") 'insert-lower-upsilon)
	    (define-key map (kbd "\S-u") 'insert-upper-upsilon)
	    (define-key map (kbd "v") 'insert-lower-sigma2)
	    (define-key map (kbd "\S-v") 'insert-upper-sigma2)
	    (define-key map (kbd "w") 'insert-lower-omega)
	    (define-key map (kbd "\S-w") 'insert-upper-omega)
	    (define-key map (kbd "x") 'insert-lower-xi)
	    (define-key map (kbd "\S-x") 'insert-upper-xi)
	    (define-key map (kbd "y") 'insert-lower-psi)
	    (define-key map (kbd "\S-y") 'insert-upper-psi)
	    (define-key map (kbd "z") 'insert-lower-zeta)
	    (define-key map (kbd "\S-z") 'insert-upper-zeta)
	    (define-key map (kbd "-") 'insert-lower-minus)
	    (define-key map (kbd "=") 'insert-lower-equals)
	    (define-key map (kbd "+") 'insert-upper-equals)
	    (define-key map (kbd "/") 'insert-lower-divide)
	    (define-key map (kbd "<") 'insert-upper-comma)
	    (define-key map (kbd ">") 'insert-upper-period)
	    (define-key map (kbd "*") 'insert-upper-eight)
	    (define-key map (kbd "0") 'insert-lower-zero)
	    (define-key map (kbd "~") 'insert-upper-tilde)
	    (define-key map (kbd "[") 'insert-lower-lbracket)
	    (define-key map (kbd "{") 'insert-upper-lbracket)
	    (define-key map (kbd "]") 'insert-lower-rbracket)
	    (define-key map (kbd "}") 'insert-upper-rbracket)
	    (define-key map [left] 'insert-lower-left)
	    (define-key map [\S-left] 'insert-upper-left)
	    (define-key map [right] 'insert-lower-right)
	    (define-key map [\S-right] 'insert-upper-right)
	    (define-key map [up] 'insert-lower-up-down)
	    (define-key map [\S-up] 'insert-upper-up-down)
	    (define-key map [down] 'insert-lower-up-down)
	    (define-key map [\S-down] 'insert-upper-up-down)
	    ;;	    (define-key map (kbd "C-[") 'insert-ctrl-lower-lbracket)
	    (define-key map (kbd "C-{") 'insert-ctrl-upper-lbracket)
	    (define-key map (kbd "C-]") 'insert-ctrl-lower-rbracket)
	    (define-key map (kbd "C-}") 'insert-ctrl-upper-rbracket)
	    (define-key map [\C-left] 'insert-ctrl-lower-left)
	    (define-key map [\C-\S-left] 'insert-ctrl-upper-left)
	    (define-key map [\C-right] 'insert-ctrl-lower-right)
	    (define-key map [\C-\S-right] 'insert-ctrl-upper-right)
	    (define-key map [\C-up] 'insert-ctrl-lower-up-down)
	    (define-key map [\C-\S-up] 'insert-ctrl-upper-up-down)
	    (define-key map [\C-down] 'insert-ctrl-lower-up-down)
	    (define-key map [\C-\S-down] 'insert-ctrl-upper-up-down)
	    map))

;;(add-hook 'text-mode-hook 'space-cadet-mode)
;;(use-local-map space-cadet-mode-map)

(provide 'space-cadet-mode)



