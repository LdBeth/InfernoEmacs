;;; tex-math-mode.el --- minor mode for math  -*- lexical-binding: t; -*-

;; Copyright (C) 1991, 1993-2024 Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file provides AUCTeX support for LaTeX.

;;; Code:
(require 'plain-tex)

;;; Math Minor Mode

(defvar TeX-math-mode-map)

(defgroup TeX-math nil
  "Mathematics in AUCTeX."
  :group 'TeX-macro)

(defvar TeX-math-keymap (make-sparse-keymap)
  "Keymap used for `TeX-math-mode' commands.")

(defcustom TeX-math-abbrev-prefix "`"
  "Prefix key for use in `TeX-math-mode'.
This has to be a string representing a key sequence in a format
understood by the `kbd' macro.  This corresponds to the syntax
usually used in the Emacs and Elisp manuals.

Setting this variable directly does not take effect;
use \\[customize]."
  :group 'TeX-math
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (define-key TeX-math-mode-map (TeX-math-abbrev-prefix) t)
         (set-default symbol value)
         (define-key TeX-math-mode-map
           (TeX-math-abbrev-prefix) TeX-math-keymap))
  :type '(string :tag "Key sequence"))

(defun TeX-math-abbrev-prefix ()
  "Make a key definition from the variable `TeX-math-abbrev-prefix'."
  (if (stringp TeX-math-abbrev-prefix)
      (read-kbd-macro TeX-math-abbrev-prefix)
    TeX-math-abbrev-prefix))

(defvar TeX-math-menu
  '("Math"
    ("Greek Uppercase") ("Greek Lowercase") ("Binary Op") ("Relational")
    ("Arrows") ("Punctuation") ("Misc Symbol") ("Var Symbol") ("Log-like")
    ("Delimiters") ("Constructs") ("Accents") ("AMS") ("Wasysym"))
  "Menu containing TeX math commands.
The menu entries will be generated dynamically, but you can specify
the sequence by initializing this variable.")

(defconst TeX-math-default
  '((?a "alpha" "Greek Lowercase" 945) ;; #X03B1
    (?b "beta" "Greek Lowercase" 946) ;; #X03B2
    (?g "gamma" "Greek Lowercase" 947) ;; #X03B3
    (?d "delta" "Greek Lowercase" 948) ;; #X03B4
    (?e "epsilon" "Greek Lowercase" 1013) ;; #X03F5
    (?z "zeta" "Greek Lowercase" 950) ;; #X03B6
    (?h "eta" "Greek Lowercase" 951) ;; #X03B7
    (?j "theta" "Greek Lowercase" 952) ;; #X03B8
    (nil "iota" "Greek Lowercase" 953) ;; #X03B9
    (?k "kappa" "Greek Lowercase" 954) ;; #X03BA
    (?l "lambda" "Greek Lowercase" 955) ;; #X03BB
    (?m "mu" "Greek Lowercase" 956) ;; #X03BC
    (?n "nu" "Greek Lowercase" 957) ;; #X03BD
    (?x "xi" "Greek Lowercase" 958) ;; #X03BE
    (?p "pi" "Greek Lowercase" 960) ;; #X03C0
    (?r "rho" "Greek Lowercase" 961) ;; #X03C1
    (?s "sigma" "Greek Lowercase" 963) ;; #X03C3
    (?t "tau" "Greek Lowercase" 964) ;; #X03C4
    (?u "upsilon" "Greek Lowercase" 965) ;; #X03C5
    (?f "phi" "Greek Lowercase" 981) ;; #X03D5
    (?q "chi" "Greek Lowercase" 967) ;; #X03C7
    (?y "psi" "Greek Lowercase" 968) ;; #X03C8
    (?w "omega" "Greek Lowercase" 969) ;; #X03C9
    ("v e" "varepsilon" "Greek Lowercase" 949) ;; #X03B5
    ("v j" "vartheta" "Greek Lowercase" 977) ;; #X03D1
    ("v p" "varpi" "Greek Lowercase" 982) ;; #X03D6
    ("v r" "varrho" "Greek Lowercase" 1009) ;; #X03F1
    ("v s" "varsigma" "Greek Lowercase" 962) ;; #X03C2
    ("v f" "varphi" "Greek Lowercase" 966) ;; #X03C6
    (?G "Gamma" "Greek Uppercase" 915) ;; #X0393
    (?D "Delta" "Greek Uppercase" 916) ;; #X0394
    (?J "Theta" "Greek Uppercase" 920) ;; #X0398
    (?L "Lambda" "Greek Uppercase" 923) ;; #X039B
    (?X "Xi" "Greek Uppercase" 926) ;; #X039E
    (?P "Pi" "Greek Uppercase" 928) ;; #X03A0
    (?S "Sigma" "Greek Uppercase" 931) ;; #X03A3
    (?U "Upsilon" "Greek Uppercase" 978) ;; #X03D2
    (?F "Phi" "Greek Uppercase" 934) ;; #X03A6
    (?Y "Psi" "Greek Uppercase" 936) ;; #X03A8
    (?W "Omega" "Greek Uppercase" 937) ;; #X03A9
    (?c TeX-math-cal "Cal-whatever")
    (nil "pm" "Binary Op" 177) ;; #X00B1
    (nil "mp" "Binary Op" 8723) ;; #X2213
    (?* "times" "Binary Op" 215) ;; #X00D7
    (nil "div" "Binary Op" 247) ;; #X00F7
    (nil "ast" "Binary Op" 8727) ;; #X2217
    (nil "star" "Binary Op" 8902) ;; #X22C6
    (nil "circ" "Binary Op" 8728) ;; #X2218
    (nil "bullet" "Binary Op" 8729) ;; #X2219
    (?. "cdot" "Binary Op" 8901) ;; #X22C5
    (?- "cap" "Binary Op" 8745) ;; #X2229
    (?+ "cup" "Binary Op" 8746) ;; #X222A
    (nil "uplus" "Binary Op" 8846) ;; #X228E
    (nil "sqcap" "Binary Op" 8851) ;; #X2293
    (?| "vee" "Binary Op" 8744) ;; #X2228
    (?& "wedge" "Binary Op" 8743) ;; #X2227
    (?\\ "setminus" "Binary Op" 8726) ;; #X2216
    (nil "wr" "Binary Op" 8768) ;; #X2240
    (nil "diamond" "Binary Op" 8900) ;; #X22C4
    (nil "bigtriangleup" "Binary Op" 9651) ;; #X25B3
    (nil "bigtriangledown" "Binary Op" 9661) ;; #X25BD
    (nil "triangleleft" "Binary Op" 9665) ;; #X25C1
    (nil "triangleright" "Binary Op" 9655) ;; #X25B7
    (nil "lhd" "Binary Op" 8882) ;; #X22B2
    (nil "rhd" "Binary Op" 8883) ;; #X22B3
    (nil "unlhd" "Binary Op" 8884) ;; #X22B4
    (nil "unrhd" "Binary Op" 8885) ;; #X22B5
    (nil "oplus" "Binary Op" 8853) ;; #X2295
    (nil "ominus" "Binary Op" 8854) ;; #X2296
    (nil "otimes" "Binary Op" 8855) ;; #X2297
    (nil "oslash" "Binary Op" 8709) ;; #X2205
    (nil "odot" "Binary Op" 8857) ;; #X2299
    (nil "bigcirc" "Binary Op" 9675) ;; #X25CB
    (nil "dagger" "Binary Op" 8224) ;; #X2020
    (nil "ddagger" "Binary Op" 8225) ;; #X2021
    (nil "amalg" "Binary Op" 10815) ;; #X2A3F
    (?< "leq" "Relational" 8804) ;; #X2264
    (?> "geq" "Relational" 8805) ;; #X2265
    (nil "qed" "Relational" 8718) ;; #X220E
    (nil "equiv" "Relational" 8801) ;; #X2261
    (nil "models" "Relational" 8871) ;; #X22A7
    (nil "prec" "Relational" 8826) ;; #X227A
    (nil "succ" "Relational" 8827) ;; #X227B
    (nil "sim" "Relational" 8764) ;; #X223C
    (nil "perp" "Relational" 10178) ;; #X27C2
    (nil "preceq" "Relational" 10927) ;; #X2AAF
    (nil "succeq" "Relational" 10928) ;; #X2AB0
    (nil "simeq" "Relational" 8771) ;; #X2243
    (nil "mid" "Relational" 8739) ;; #X2223
    (nil "ll" "Relational" 8810) ;; #X226A
    (nil "gg" "Relational" 8811) ;; #X226B
    (nil "asymp" "Relational" 8781) ;; #X224D
    (nil "parallel" "Relational" 8741) ;; #X2225
    (?\{ "subset" "Relational" 8834) ;; #X2282
    (?\} "supset" "Relational" 8835) ;; #X2283
    (nil "approx" "Relational" 8776) ;; #X2248
    (nil "bowtie" "Relational" 8904) ;; #X22C8
    (?\[ "subseteq" "Relational" 8838) ;; #X2286
    (?\] "supseteq" "Relational" 8839) ;; #X2287
    (nil "cong" "Relational" 8773) ;; #X2245
    (nil "Join" "Relational" 10781) ;; #X2A1D
    (nil "sqsubset" "Relational" 8847) ;; #X228F
    (nil "sqsupset" "Relational" 8848) ;; #X2290
    (nil "neq" "Relational" 8800) ;; #X2260
    (nil "smile" "Relational" 8995) ;; #X2323
    (nil "sqsubseteq" "Relational" 8849) ;; #X2291
    (nil "sqsupseteq" "Relational" 8850) ;; #X2292
    (nil "doteq" "Relational" 8784) ;; #X2250
    (nil "frown" "Relational" 8994) ;; #X2322
    (?i "in" "Relational" 8712) ;; #X2208
    (nil "ni" "Relational" 8715) ;; #X220B
    (nil "propto" "Relational" 8733) ;; #X221D
    (nil "vdash" "Relational" 8866) ;; #X22A2
    (nil "dashv" "Relational" 8867) ;; #X22A3
    (?\C-b "leftarrow" "Arrows" 8592) ;; #X2190
    (nil "Leftarrow" "Arrows" 8656) ;; #X21D0
    (?\C-f "rightarrow" "Arrows" 8594) ;; #X2192
    (nil "Rightarrow" "Arrows" 8658) ;; #X21D2
    (nil "leftrightarrow" "Arrows" 8596) ;; #X2194
    (nil "Leftrightarrow" "Arrows" 8660) ;; #X21D4
    (nil "mapsto" "Arrows" 8614) ;; #X21A6
    (nil "hookleftarrow" "Arrows" 8617) ;; #X21A9
    (nil "leftharpoonup" "Arrows" 8636) ;; #X21BC
    (nil "leftharpoondown" "Arrows" 8637) ;; #X21BD
    (nil "longleftarrow" "Arrows" 10229) ;; #X27F5
    (nil "Longleftarrow" "Arrows" 10232) ;; #X27F8
    (nil "longrightarrow" "Arrows" 10230) ;; #X27F6
    (nil "Longrightarrow" "Arrows" 10233) ;; #X27F9
    (nil "longleftrightarrow" "Arrows" 10231) ;; #X27F7
    (nil "Longleftrightarrow" "Arrows" 10234) ;; #X27FA
    (nil "iff" "Arrows" 10234) ;; #X27FA
    (nil "longmapsto" "Arrows" 10236) ;; #X27FC
    (nil "hookrightarrow" "Arrows" 8618) ;; #X21AA
    (nil "rightharpoonup" "Arrows" 8640) ;; #X21C0
    (nil "rightharpoondown" "Arrows" 8641) ;; #X21C1
    (?\C-p "uparrow" "Arrows" 8593) ;; #X2191
    (nil "Uparrow" "Arrows" 8657) ;; #X21D1
    (?\C-n "downarrow" "Arrows" 8595) ;; #X2193
    (nil "Downarrow" "Arrows" 8659) ;; #X21D3
    (nil "updownarrow" "Arrows" 8597) ;; #X2195
    (nil "Updownarrow" "Arrows" 8661) ;; #X21D5
    (nil "nearrow" "Arrows" 8599) ;; #X2197
    (nil "searrow" "Arrows" 8600) ;; #X2198
    (nil "swarrow" "Arrows" 8601) ;; #X2199
    (nil "nwarrow" "Arrows" 8598) ;; #X2196
    (nil "ldots" "Punctuation" 8230) ;; #X2026
    (nil "cdots" "Punctuation" 8943) ;; #X22EF
    (nil "vdots" "Punctuation" 8942) ;; #X22EE
    (nil "ddots" "Punctuation" 8945) ;; #X22F1
    (?: "colon" "Punctuation" 58) ;; #X003A
    (?N "nabla" "Misc Symbol" 8711) ;; #X2207
    (nil "aleph" "Misc Symbol" 8501) ;; #X2135
    (nil "prime" "Misc Symbol" 8242) ;; #X2032
    (?A "forall" "Misc Symbol" 8704) ;; #X2200
    (?I "infty" "Misc Symbol" 8734) ;; #X221E
    (nil "hbar" "Misc Symbol" 8463) ;; #X210F
    (?0 "emptyset" "Misc Symbol" 8709) ;; #X2205
    (?E "exists" "Misc Symbol" 8707) ;; #X2203
    (nil "surd" "Misc Symbol" 8730) ;; #X221A
    (nil "Box" "Misc Symbol" 9633) ;; #X25A1
    (nil "triangle" "Misc Symbol" 9651) ;; #X25B3
    (nil "Diamond" "Misc Symbol" 9671) ;; #X25C7
    (nil "imath" "Misc Symbol" 120484) ;; #X1D6A4
    (nil "jmath" "Misc Symbol" 120485) ;; #X1D6A5
    (nil "ell" "Misc Symbol" 8467) ;; #X2113
    (nil "neg" "Misc Symbol" 172) ;; #X00AC
    (?/ "not" "Misc Symbol" 824) ;; #X0338
    (nil "top" "Misc Symbol" 8868) ;; #X22A4
    (nil "flat" "Misc Symbol" 9837) ;; #X266D
    (nil "natural" "Misc Symbol" 9838) ;; #X266E
    (nil "sharp" "Misc Symbol" 9839) ;; #X266F
    (nil "wp" "Misc Symbol" 8472) ;; #X2118
    (nil "bot" "Misc Symbol" 8869) ;; #X22A5
    (nil "clubsuit" "Misc Symbol" 9827) ;; #X2663
    (nil "diamondsuit" "Misc Symbol" 9826) ;; #X2662
    (nil "heartsuit" "Misc Symbol" 9825) ;; #X2661
    (nil "spadesuit" "Misc Symbol" 9824) ;; #X2660
    (nil "mho" "Misc Symbol" 8487) ;; #X2127
    (nil "Re" "Misc Symbol" 8476) ;; #X211C
    (nil "Im" "Misc Symbol" 8465) ;; #X2111
    (nil "angle" "Misc Symbol" 8736) ;; #X2220
    (nil "partial" "Misc Symbol" 8706) ;; #X2202
    (nil "sum" "Var Symbol" 8721) ;; #X2211
    (nil "prod" "Var Symbol" 8719) ;; #X220F
    (nil "coprod" "Var Symbol" 8720) ;; #X2210
    (nil "int" "Var Symbol" 8747) ;; #X222B
    (nil "oint" "Var Symbol" 8750) ;; #X222E
    (nil "bigcap" "Var Symbol" 8898) ;; #X22C2
    (nil "bigcup" "Var Symbol" 8899) ;; #X22C3
    (nil "bigsqcup" "Var Symbol" 10758) ;; #X2A06
    (nil "bigvee" "Var Symbol" 8897) ;; #X22C1
    (nil "bigwedge" "Var Symbol" 8896) ;; #X22C0
    (nil "bigodot" "Var Symbol" 10752) ;; #X2A00
    (nil "bigotimes" "Var Symbol" 10754) ;; #X2A02
    (nil "bigoplus" "Var Symbol" 10753) ;; #X2A01
    (nil "biguplus" "Var Symbol" 10756) ;; #X2A04
    (nil "arccos" "Log-like")
    (nil "arcsin" "Log-like")
    (nil "arctan" "Log-like")
    (nil "arg" "Log-like")
    (?\C-c "cos" "Log-like")
    (nil "cosh" "Log-like")
    (nil "cot" "Log-like")
    (nil "coth" "Log-like")
    (nil "csc" "Log-like")
    (nil "deg" "Log-like")
    (?\C-d "det" "Log-like")
    (nil "dim" "Log-like")
    (?\C-e "exp" "Log-like")
    (nil "gcd" "Log-like")
    (nil "hom" "Log-like")
    (?\C-_ "inf" "Log-like")
    (nil "ker" "Log-like")
    (nil "lg" "Log-like")
    (?\C-l "lim" "Log-like")
    (nil "liminf" "Log-like")
    (nil "limsup" "Log-like")
    (nil "ln" "Log-like")
    (nil "log" "Log-like")
    (nil "max" "Log-like")
    (nil "min" "Log-like")
    (nil "Pr" "Log-like")
    (nil "sec" "Log-like")
    (?\C-s "sin" "Log-like")
    (nil "sinh" "Log-like")
    (?\C-^ "sup" "Log-like")
    (?\C-t "tan" "Log-like")
    (nil "tanh" "Log-like")
    (nil "{" "Delimiters" ?{)
    (nil "}" "Delimiters" ?})
    (nil "lfloor" "Delimiters" 8970) ;; #X230A
    (nil "rfloor" "Delimiters" 8971) ;; #X230B
    (nil "lceil" "Delimiters" 8968) ;; #X2308
    (nil "rceil" "Delimiters" 8969) ;; #X2309
    (?\( "langle" "Delimiters" 10216) ;; #X27E8
    (?\) "rangle" "Delimiters" 10217) ;; #X27E9
    (nil "rmoustache" "Delimiters" 9137) ;; #X23B1
    (nil "lmoustache" "Delimiters" 9136) ;; #X23B0
    (nil "rgroup" "Delimiters" 9133) ;; #X23AD
    (nil "lgroup" "Delimiters" 9129) ;; #X23A9
    (nil "backslash" "Delimiters" 92) ;; #X005C
    (nil "|" "Delimiters" 8214) ;; #X2016)
    (nil "arrowvert" "Delimiters")
    (nil "Arrowvert" "Delimiters")
    (nil "bracevert" "Delimiters")
    (nil "widetilde" "Constructs" 771) ;; #X0303
    (nil "widehat" "Constructs" 770) ;; #X0302
    (nil "overleftarrow" "Constructs" 8406) ;; #X20D6
    (nil "overrightarrow" "Constructs")
    (nil "overline" "Constructs" 773) ;; #X0305
    (nil "underline" "Constructs" 818) ;; #X0332
    (nil "overbrace" "Constructs" 65079) ;; #XFE37
    (nil "underbrace" "Constructs" 65080) ;; #XFE38
    (nil "sqrt" "Constructs" 8730) ;; #X221A
    (nil "frac" "Constructs")
    (?^ "hat" "Accents" 770) ;; #X0302
    (nil "acute" "Accents" 769) ;; #X0301
    (nil "bar" "Accents" 772) ;; #X0304
    (nil "dot" "Accents" 775) ;; #X0307
    (nil "breve" "Accents" 774) ;; #X0306
    (nil "check" "Accents" 780) ;; #X030C
    (nil "grave" "Accents" 768) ;; #X0300
    (nil "vec" "Accents" 8407) ;; #X20D7
    (nil "ddot" "Accents" 776) ;; #X0308
    (?~ "tilde" "Accents" 771) ;; #X0303
    (nil "mathring" "Accents" 778) ;; #X030A
    (nil "beth" ("AMS" "Hebrew") 8502) ;; #X2136
    (nil "daleth" ("AMS" "Hebrew") 8504) ;; #X2138
    (nil "gimel" ("AMS" "Hebrew") 8503) ;; #X2137
    (nil "digamma" ("AMS" "Greek Lowercase") 989) ;; #X03DD
    ("v k" "varkappa" ("AMS" "Greek Lowercase") 1008) ;; #X03F0
    ("v G" "varGamma" ("AMS" "Greek Uppercase") 120548) ;; #X1D6E4
    ("v D" "varDelta" ("AMS" "Greek Uppercase") 120549) ;; #X1D6E5
    ("v J" "varTheta" ("AMS" "Greek Uppercase") 120553) ;; #X1D6E9
    ("v L" "varLambda" ("AMS" "Greek Uppercase") 120556) ;; #X1D6EC
    ("v X" "varXi" ("AMS" "Greek Uppercase") 120559) ;; #X1D6EF
    ("v P" "varPi" ("AMS" "Greek Uppercase") 120561) ;; #X1D6F1
    ("v S" "varSigma" ("AMS" "Greek Uppercase") 120564) ;; #X1D6F4
    ("v U" "varUpsilon" ("AMS" "Greek Uppercase") 120566) ;; #X1D6F6
    ("v F" "varPhi" ("AMS" "Greek Uppercase") 120567) ;; #X1D6F7
    ("v Y" "varPsi" ("AMS" "Greek Uppercase") 120569) ;; #X1D6F9
    ("v W" "varOmega" ("AMS" "Greek Uppercase") 120570) ;; #X1D6FA
    (nil "dashrightarrow" ("AMS" "Arrows"))
    (nil "dashleftarrow" ("AMS" "Arrows"))
    (nil "impliedby" ("AMS" "Arrows") 10232) ;; #X27F8
    (nil "implies" ("AMS" "Arrows") 10233) ;; #X27F9
    (nil "leftleftarrows" ("AMS" "Arrows") 8647) ;; #X21C7
    (nil "leftrightarrows" ("AMS" "Arrows") 8646) ;; #X21C6
    (nil "Lleftarrow" ("AMS" "Arrows") 8666) ;; #X21DA
    (nil "twoheadleftarrow" ("AMS" "Arrows") 8606) ;; #X219E
    (nil "leftarrowtail" ("AMS" "Arrows") 8610) ;; #X21A2
    (nil "looparrowleft" ("AMS" "Arrows") 8619) ;; #X21AB
    (nil "leftrightharpoons" ("AMS" "Arrows") 8651) ;; #X21CB
    (nil "curvearrowleft" ("AMS" "Arrows") 8630) ;; #X21B6
    (nil "circlearrowleft" ("AMS" "Arrows") 8634) ;; #X21BA
    (nil "Lsh" ("AMS" "Arrows") 8624) ;; #X21B0
    (nil "upuparrows" ("AMS" "Arrows") 8648) ;; #X21C8
    (nil "upharpoonleft" ("AMS" "Arrows") 8639) ;; #X21BF
    (nil "downharpoonleft" ("AMS" "Arrows") 8643) ;; #X21C3
    (nil "multimap" ("AMS" "Arrows") 8888) ;; #X22B8
    (nil "leftrightsquigarrow" ("AMS" "Arrows") 8621) ;; #X21AD
    (nil "looparrowright" ("AMS" "Arrows") 8620) ;; #X21AC
    (nil "rightleftharpoons" ("AMS" "Arrows") 8652) ;; #X21CC
    (nil "curvearrowright" ("AMS" "Arrows") 8631) ;; #X21B7
    (nil "circlearrowright" ("AMS" "Arrows"))
    (nil "Rsh" ("AMS" "Arrows") 8625) ;; #X21B1
    (nil "downdownarrows" ("AMS" "Arrows") 8650) ;; #X21CA
    (nil "upharpoonright" ("AMS" "Arrows") 8638) ;; #X21BE
    (nil "downharpoonright" ("AMS" "Arrows") 8642) ;; #X21C2
    (nil "rightsquigarrow" ("AMS" "Arrows") 8605) ;; #X219D
    (nil "nleftarrow" ("AMS" "Neg Arrows") 8602) ;; #X219A
    (nil "nrightarrow" ("AMS" "Neg Arrows") 8603) ;; #X219B
    (nil "nLeftarrow" ("AMS" "Neg Arrows") 8653) ;; #X21CD
    (nil "nRightarrow" ("AMS" "Neg Arrows") 8655) ;; #X21CF
    (nil "nleftrightarrow" ("AMS" "Neg Arrows") 8622) ;; #X21AE
    (nil "nLeftrightarrow" ("AMS" "Neg Arrows") 8654) ;; #X21CE
    (nil "leqq" ("AMS" "Relational I") 8806) ;; #X2266
    (nil "leqslant" ("AMS" "Relational I") 10877) ;; #X2A7D
    (nil "eqslantless" ("AMS" "Relational I") 10901) ;; #X2A95
    (nil "lesssim" ("AMS" "Relational I") 8818) ;; #X2272
    (nil "lessapprox" ("AMS" "Relational I") 10885) ;; #X2A85
    (nil "approxeq" ("AMS" "Relational I") 8778) ;; #X224A
    (nil "lessdot" ("AMS" "Relational I") 8918) ;; #X22D6
    (nil "lll" ("AMS" "Relational I") 8920) ;; #X22D8
    (nil "lessgtr" ("AMS" "Relational I") 8822) ;; #X2276
    (nil "lesseqgtr" ("AMS" "Relational I") 8922) ;; #X22DA
    (nil "lesseqqgtr" ("AMS" "Relational I") 10891) ;; #X2A8B
    (nil "doteqdot" ("AMS" "Relational I") 8785) ;; #X2251
    (nil "risingdotseq" ("AMS" "Relational I") 8787) ;; #X2253
    (nil "fallingdotseq" ("AMS" "Relational I") 8786) ;; #X2252
    (nil "backsim" ("AMS" "Relational I") 8765) ;; #X223D
    (nil "backsimeq" ("AMS" "Relational I") 8909) ;; #X22CD
    (nil "subseteqq" ("AMS" "Relational I") 10949) ;; #X2AC5
    (nil "Subset" ("AMS" "Relational I") 8912) ;; #X22D0
    (nil "sqsubset" ("AMS" "Relational I") 8847) ;; #X228F
    (nil "preccurlyeq" ("AMS" "Relational I") 8828) ;; #X227C
    (nil "curlyeqprec" ("AMS" "Relational I") 8926) ;; #X22DE
    (nil "precsim" ("AMS" "Relational I") 8830) ;; #X227E
    (nil "precapprox" ("AMS" "Relational I") 10935) ;; #X2AB7
    (nil "vartriangleleft" ("AMS" "Relational I") 8882) ;; #X22B2
    (nil "trianglelefteq" ("AMS" "Relational I") 8884) ;; #X22B4
    (nil "vDash" ("AMS" "Relational I") 8872) ;; #X22A8
    (nil "Vvdash" ("AMS" "Relational I") 8874) ;; #X22AA
    (nil "smallsmile" ("AMS" "Relational I") 8995) ;; #X2323
    (nil "smallfrown" ("AMS" "Relational I") 8994) ;; #X2322
    (nil "bumpeq" ("AMS" "Relational I") 8783) ;; #X224F
    (nil "Bumpeq" ("AMS" "Relational I") 8782) ;; #X224E
    (nil "geqq" ("AMS" "Relational II") 8807) ;; #X2267
    (nil "geqslant" ("AMS" "Relational II") 10878) ;; #X2A7E
    (nil "eqslantgtr" ("AMS" "Relational II") 10902) ;; #X2A96
    (nil "gtrsim" ("AMS" "Relational II") 8819) ;; #X2273
    (nil "gtrapprox" ("AMS" "Relational II") 10886) ;; #X2A86
    (nil "gtrdot" ("AMS" "Relational II") 8919) ;; #X22D7
    (nil "ggg" ("AMS" "Relational II") 8921) ;; #X22D9
    (nil "gtrless" ("AMS" "Relational II") 8823) ;; #X2277
    (nil "gtreqless" ("AMS" "Relational II") 8923) ;; #X22DB
    (nil "gtreqqless" ("AMS" "Relational II") 10892) ;; #X2A8C
    (nil "eqcirc" ("AMS" "Relational II") 8790) ;; #X2256
    (nil "circeq" ("AMS" "Relational II") 8791) ;; #X2257
    (nil "triangleq" ("AMS" "Relational II") 8796) ;; #X225C
    (nil "thicksim" ("AMS" "Relational II") 8764) ;; #X223C
    (nil "thickapprox" ("AMS" "Relational II") 8776) ;; #X2248
    (nil "supseteqq" ("AMS" "Relational II") 10950) ;; #X2AC6
    (nil "Supset" ("AMS" "Relational II") 8913) ;; #X22D1
    (nil "sqsupset" ("AMS" "Relational II") 8848) ;; #X2290
    (nil "succcurlyeq" ("AMS" "Relational II") 8829) ;; #X227D
    (nil "curlyeqsucc" ("AMS" "Relational II") 8927) ;; #X22DF
    (nil "succsim" ("AMS" "Relational II") 8831) ;; #X227F
    (nil "succapprox" ("AMS" "Relational II") 10936) ;; #X2AB8
    (nil "vartriangleright" ("AMS" "Relational II") 8883) ;; #X22B3
    (nil "trianglerighteq" ("AMS" "Relational II") 8885) ;; #X22B5
    (nil "Vdash" ("AMS" "Relational II") 8873) ;; #X22A9
    (nil "shortmid" ("AMS" "Relational II") 8739) ;; #X2223
    (nil "shortparallel" ("AMS" "Relational II") 8741) ;; #X2225
    (nil "between" ("AMS" "Relational II") 8812) ;; #X226C
    (nil "pitchfork" ("AMS" "Relational II") 8916) ;; #X22D4
    (nil "varpropto" ("AMS" "Relational II") 8733) ;; #X221D
    (nil "blacktriangleleft" ("AMS" "Relational II") 9664) ;; #X25C0
    (nil "therefore" ("AMS" "Relational II") 8756) ;; #X2234
    (nil "backepsilon" ("AMS" "Relational II") 1014) ;; #X03F6
    (nil "blacktriangleright" ("AMS" "Relational II") 9654) ;; #X25B6
    (nil "because" ("AMS" "Relational II") 8757) ;; #X2235
    (nil "nless" ("AMS" "Neg Rel I") 8814) ;; #X226E
    (nil "nleq" ("AMS" "Neg Rel I") 8816) ;; #X2270
    (nil "nleqslant" ("AMS" "Neg Rel I"))
    (nil "nleqq" ("AMS" "Neg Rel I"))
    (nil "lneq" ("AMS" "Neg Rel I") 10887) ;; #X2A87
    (nil "lneqq" ("AMS" "Neg Rel I") 8808) ;; #X2268
    (nil "lvertneqq" ("AMS" "Neg Rel I"))
    (nil "lnsim" ("AMS" "Neg Rel I") 8934) ;; #X22E6
    (nil "lnapprox" ("AMS" "Neg Rel I") 10889) ;; #X2A89
    (nil "nprec" ("AMS" "Neg Rel I") 8832) ;; #X2280
    (nil "npreceq" ("AMS" "Neg Rel I"))
    (nil "precnsim" ("AMS" "Neg Rel I") 8936) ;; #X22E8
    (nil "precnapprox" ("AMS" "Neg Rel I") 10937) ;; #X2AB9
    (nil "nsim" ("AMS" "Neg Rel I") 8769) ;; #X2241
    (nil "nshortmid" ("AMS" "Neg Rel I") 8740) ;; #X2224
    (nil "nmid" ("AMS" "Neg Rel I") 8740) ;; #X2224
    (nil "nvdash" ("AMS" "Neg Rel I") 8876) ;; #X22AC
    (nil "nvDash" ("AMS" "Neg Rel I") 8877) ;; #X22AD
    (nil "ntriangleleft" ("AMS" "Neg Rel I") 8938) ;; #X22EA
    (nil "ntrianglelefteq" ("AMS" "Neg Rel I") 8940) ;; #X22EC
    (nil "nsubseteq" ("AMS" "Neg Rel I") 8840) ;; #X2288
    (nil "subsetneq" ("AMS" "Neg Rel I") 8842) ;; #X228A
    (nil "varsubsetneq" ("AMS" "Neg Rel I"))
    (nil "subsetneqq" ("AMS" "Neg Rel I") 10955) ;; #X2ACB
    (nil "varsubsetneqq" ("AMS" "Neg Rel I"))
    (nil "ngtr" ("AMS" "Neg Rel II") 8815) ;; #X226F
    (nil "ngeq" ("AMS" "Neg Rel II") 8817) ;; #X2271
    (nil "ngeqslant" ("AMS" "Neg Rel II"))
    (nil "ngeqq" ("AMS" "Neg Rel II"))
    (nil "gneq" ("AMS" "Neg Rel II") 10888) ;; #X2A88
    (nil "gneqq" ("AMS" "Neg Rel II") 8809) ;; #X2269
    (nil "gvertneqq" ("AMS" "Neg Rel II"))
    (nil "gnsim" ("AMS" "Neg Rel II") 8935) ;; #X22E7
    (nil "gnapprox" ("AMS" "Neg Rel II") 10890) ;; #X2A8A
    (nil "nsucc" ("AMS" "Neg Rel II") 8833) ;; #X2281
    (nil "nsucceq" ("AMS" "Neg Rel II"))
    (nil "succnsim" ("AMS" "Neg Rel II") 8937) ;; #X22E9
    (nil "succnapprox" ("AMS" "Neg Rel II") 10938) ;; #X2ABA
    (nil "ncong" ("AMS" "Neg Rel II") 8775) ;; #X2247
    (nil "nshortparallel" ("AMS" "Neg Rel II") 8742) ;; #X2226
    (nil "nparallel" ("AMS" "Neg Rel II") 8742) ;; #X2226
    (nil "nvDash" ("AMS" "Neg Rel II") 8877) ;; #X22AD
    (nil "nVDash" ("AMS" "Neg Rel II") 8879) ;; #X22AF
    (nil "ntriangleright" ("AMS" "Neg Rel II") 8939) ;; #X22EB
    (nil "ntrianglerighteq" ("AMS" "Neg Rel II") 8941) ;; #X22ED
    (nil "nsupseteq" ("AMS" "Neg Rel II") 8841) ;; #X2289
    (nil "nsupseteqq" ("AMS" "Neg Rel II"))
    (nil "supsetneq" ("AMS" "Neg Rel II") 8843) ;; #X228B
    (nil "varsupsetneq" ("AMS" "Neg Rel II"))
    (nil "supsetneqq" ("AMS" "Neg Rel II") 10956) ;; #X2ACC
    (nil "varsupsetneqq" ("AMS" "Neg Rel II"))
    (nil "dotplus" ("AMS" "Binary Op") 8724) ;; #X2214
    (nil "smallsetminus" ("AMS" "Binary Op") 8726) ;; #X2216
    (nil "Cap" ("AMS" "Binary Op") 8914) ;; #X22D2
    (nil "Cup" ("AMS" "Binary Op") 8915) ;; #X22D3
    (nil "barwedge" ("AMS" "Binary Op") 8892) ;; #X22BC
    (nil "veebar" ("AMS" "Binary Op") 8891) ;; #X22BB
    (nil "doublebarwedge" ("AMS" "Binary Op") 8966) ;; #X2306
    (nil "boxminus" ("AMS" "Binary Op") 8863) ;; #X229F
    (nil "boxtimes" ("AMS" "Binary Op") 8864) ;; #X22A0
    (nil "boxdot" ("AMS" "Binary Op") 8865) ;; #X22A1
    (nil "boxplus" ("AMS" "Binary Op") 8862) ;; #X229E
    (nil "divideontimes" ("AMS" "Binary Op") 8903) ;; #X22C7
    (nil "ltimes" ("AMS" "Binary Op") 8905) ;; #X22C9
    (nil "rtimes" ("AMS" "Binary Op") 8906) ;; #X22CA
    (nil "leftthreetimes" ("AMS" "Binary Op") 8907) ;; #X22CB
    (nil "rightthreetimes" ("AMS" "Binary Op") 8908) ;; #X22CC
    (nil "curlywedge" ("AMS" "Binary Op") 8911) ;; #X22CF
    (nil "curlyvee" ("AMS" "Binary Op") 8910) ;; #X22CE
    (nil "circleddash" ("AMS" "Binary Op") 8861) ;; #X229D
    (nil "circledast" ("AMS" "Binary Op") 8859) ;; #X229B
    (nil "circledcirc" ("AMS" "Binary Op") 8858) ;; #X229A
    (nil "centerdot" ("AMS" "Binary Op"))
    (nil "intercal" ("AMS" "Binary Op") 8890) ;; #X22BA
    (nil "hbar" ("AMS" "Misc") 8463) ;; #X210F
    (nil "hslash" ("AMS" "Misc") 8463) ;; #X210F
    (nil "vartriangle" ("AMS" "Misc") 9653) ;; #X25B5
    (nil "triangledown" ("AMS" "Misc") 9663) ;; #X25BF
    (nil "square" ("AMS" "Misc") 9633) ;; #X25A1
    (nil "lozenge" ("AMS" "Misc") 9674) ;; #X25CA
    (nil "circledS" ("AMS" "Misc") 9416) ;; #X24C8
    (nil "angle" ("AMS" "Misc") 8736) ;; #X2220
    (nil "measuredangle" ("AMS" "Misc") 8737) ;; #X2221
    (nil "nexists" ("AMS" "Misc") 8708) ;; #X2204
    (nil "mho" ("AMS" "Misc") 8487) ;; #X2127
    (nil "Finv" ("AMS" "Misc") 8498) ;; #X2132
    (nil "Game" ("AMS" "Misc") 8513) ;; #X2141
    (nil "Bbbk" ("AMS" "Misc") 120156) ;; #X1D55C
    (nil "backprime" ("AMS" "Misc") 8245) ;; #X2035
    (nil "varnothing" ("AMS" "Misc") 8709) ;; #X2205
    (nil "blacktriangle" ("AMS" "Misc") 9652) ;; #X25B4
    (nil "blacktriangledown" ("AMS" "Misc") 9662) ;; #X25BE
    (nil "blacksquare" ("AMS" "Misc") 9632) ;; #X25A0
    (nil "blacklozenge" ("AMS" "Misc") 10731) ;; #X29EB
    (nil "bigstar" ("AMS" "Misc") 9733) ;; #X2605
    (nil "sphericalangle" ("AMS" "Misc") 8738) ;; #X2222
    (nil "complement" ("AMS" "Misc") 8705) ;; #X2201
    (nil "eth" ("AMS" "Misc") 240) ;; #X00F0
    (nil "diagup" ("AMS" "Misc") 9585) ;; #X2571
    (nil "diagdown" ("AMS" "Misc") 9586) ;; #X2572
    (nil "dddot" ("AMS" "Accents") 8411) ;; #X20DB
    (nil "ddddot" ("AMS" "Accents") 8412) ;; #X20DC
    (nil "bigl" ("AMS" "Delimiters"))
    (nil "bigr" ("AMS" "Delimiters"))
    (nil "Bigl" ("AMS" "Delimiters"))
    (nil "Bigr" ("AMS" "Delimiters"))
    (nil "biggl" ("AMS" "Delimiters"))
    (nil "biggr" ("AMS" "Delimiters"))
    (nil "Biggl" ("AMS" "Delimiters"))
    (nil "Biggr" ("AMS" "Delimiters"))
    (nil "lvert" ("AMS" "Delimiters"))
    (nil "rvert" ("AMS" "Delimiters"))
    (nil "lVert" ("AMS" "Delimiters"))
    (nil "rVert" ("AMS" "Delimiters"))
    (nil "ulcorner" ("AMS" "Delimiters") 8988) ;; #X231C
    (nil "urcorner" ("AMS" "Delimiters") 8989) ;; #X231D
    (nil "llcorner" ("AMS" "Delimiters") 8990) ;; #X231E
    (nil "lrcorner" ("AMS" "Delimiters") 8991) ;; #X231F
    (nil "nobreakdash" ("AMS" "Special"))
    (nil "leftroot" ("AMS" "Special"))
    (nil "uproot" ("AMS" "Special"))
    (nil "accentedsymbol" ("AMS" "Special"))
    (nil "xleftarrow" ("AMS" "Special"))
    (nil "xrightarrow" ("AMS" "Special"))
    (nil "overset" ("AMS" "Special"))
    (nil "underset" ("AMS" "Special"))
    (nil "dfrac" ("AMS" "Special"))
    (nil "genfrac" ("AMS" "Special"))
    (nil "tfrac" ("AMS" "Special"))
    (nil "binom" ("AMS" "Special"))
    (nil "dbinom" ("AMS" "Special"))
    (nil "tbinom" ("AMS" "Special"))
    (nil "smash" ("AMS" "Special"))
    (nil "eucal" ("AMS" "Special"))
    (nil "boldsymbol" ("AMS" "Special"))
    (nil "text" ("AMS" "Special"))
    (nil "intertext" ("AMS" "Special"))
    (nil "substack" ("AMS" "Special"))
    (nil "subarray" ("AMS" "Special"))
    (nil "sideset" ("AMS" "Special"))
    ;; Wasysym symbols:
    (nil "lhd" ("Wasysym" "Binary Op") 9665) ;; #X22C1
    (nil "LHD" ("Wasysym" "Binary Op") 9664) ;; #X25C0
    (nil "ocircle" ("Wasysym" "Binary Op") 9675) ;; #X25CB
    (nil "rhd" ("Wasysym" "Binary Op") 9655) ;; #X25B7
    (nil "RHD" ("Wasysym" "Binary Op") 9654) ;; #X25B6
    (nil "unlhd" ("Wasysym" "Binary Op") 8884) ;; #X22B4
    (nil "unrhd" ("Wasysym" "Binary Op") 8885) ;; #X22B5
    (nil "apprle" ("Wasysym" "Relational") 8818) ;; #X2272
    (nil "apprge" ("Wasysym" "Relational") 8819) ;; #X2273
    (nil "invneg" ("Wasysym" "Relational") 8976) ;; #X2310
    (nil "Join" ("Wasysym" "Relational") 10781) ;; #X2A1D
    (nil "leadsto" ("Wasysym" "Relational") 10547) ;; #X2933
    (nil "sqsubset" ("Wasysym" "Relational") 8847) ;; #X228f
    (nil "sqsupset" ("Wasysym" "Relational") 8848) ;; #X2290
    (nil "wasypropto" ("Wasysym" "Relational") 8733) ;; #X221D
    (nil "Box" ("Wasysym" "Misc Symbol") 9633) ;; #X25A1
    (nil "Diamond" ("Wasysym" "Misc Symbol") 9671) ;; #X25C7
    (nil "logof" ("Wasysym" "Misc Symbol")))
  "Alist of TeX math symbols.

Each entry should be a list with upto four elements, KEY, VALUE,
MENU and CHARACTER, see `TeX-math-list' for details.")

(defcustom TeX-math-menu-unicode
  (or (string-match "\\<GTK\\>" (emacs-version))
      (eq window-system 'w32))
  "Whether the TeX menu should try using Unicode for effect."
  :type 'boolean
  :group 'TeX-math)

(defvar TeX-math-list) ;; Defined further below.

(defun TeX-math-initialize ()
  (let ((math (reverse (append TeX-math-list TeX-math-default)))
        (map TeX-math-keymap)
        (unicode TeX-math-menu-unicode))
    (while math
      (let* ((entry (car math))
             (key (nth 0 entry))
             (prefix
              (and unicode
                   (nth 3 entry)))
             value menu name)
        (setq math (cdr math))
        (if (and prefix
                 (setq prefix (nth 3 entry)))
            (setq prefix (concat (string prefix) " \\"))
          (setq prefix "\\"))
        (if (listp (cdr entry))
            (setq value (nth 1 entry)
                  menu (nth 2 entry))
          (setq value (cdr entry)
                menu nil))
        (if (stringp value)
            (progn
              (setq name (intern (concat "TeX-math-" value)))
              (fset name (lambda (arg) (interactive "*P")
                           (TeX-math-insert value arg))))
          (setq name value))
        (if key
            (progn
              (setq key (cond ((numberp key) (char-to-string key))
                              ((stringp key) (read-kbd-macro key))
                              (t (vector key))))
              (define-key map key name)))
        (if menu
            (let ((parent TeX-math-menu))
              (if (listp menu)
                  (progn
                    (while (cdr menu)
                      (let ((sub (assoc (car menu) TeX-math-menu)))
                        (if sub
                            (setq parent sub)
                          (setcdr parent (cons (list (car menu)) (cdr parent))))
                        (setq menu (cdr menu))))
                    (setq menu (car menu))))
              (let ((sub (assoc menu parent)))
                (if sub
                    (if (stringp value)
                        (setcdr sub (cons (vector (concat prefix value)
                                                  name t)
                                          (cdr sub)))
                      (error "Cannot have multiple special math menu items"))
                  (setcdr parent
                          (cons (if (stringp value)
                                    (list menu (vector (concat prefix value)
                                                       name t))
                                  (vector menu name t))
                                (cdr parent)))))))))
    ;; Make the math prefix char available if it has not been used as a prefix.
    (unless (lookup-key map (TeX-math-abbrev-prefix))
      (define-key map (TeX-math-abbrev-prefix) #'self-insert-command))))

(defcustom TeX-math-list nil
  "Alist of your personal TeX math symbols.

Each entry should be a list with up to four elements, KEY, VALUE,
MENU and CHARACTER.

KEY is the key (after `TeX-math-abbrev-prefix') to be redefined
in math minor mode.  KEY can be a character (for example ?o) for a
single stroke or a string (for example \"o a\") for a multi-stroke
binding.  If KEY is nil, the symbol has no associated
keystroke (it is available in the menu, though).  Note that
predefined keys in `TeX-math-default' cannot be overridden in
this variable.  Currently, only the lowercase letter \\='o\\=' is free
for user customization, more options are available in uppercase
area.

VALUE can be a string with the name of the macro to be inserted,
or a function to be called.  The macro must be given without the
leading backslash.

The third element MENU is the name of the submenu where the
command should be added.  MENU can be either a string (for
example \"greek\"), a list (for example (\"AMS\" \"Delimiters\"))
or nil.  If MENU is nil, no menu item will be created.

The fourth element CHARACTER is a Unicode character position for
menu display.  When nil, no character is shown.

See also `TeX-math-menu'."
  :group 'TeX-math
  :set (lambda (symbol value)
         (set-default symbol value)
         (TeX-math-initialize))
  :type '(repeat (group (choice :tag "Key"
                                (const :tag "none" nil)
                                (choice (character)
                                        (string :tag "Key sequence")))
                        (choice :tag "Value"
                                (string :tag "Macro")
                                (function))
                        (choice :tag "Menu"
                                (string :tag "Top level menu" )
                                (repeat :tag "Submenu"
                                        (string :tag "Menu")))
                        (choice :tag "Unicode character"
                                (const :tag "none" nil)
                                (integer :tag "Number")))))

(defun TeX--completion-annotation-from-math-menu (sym)
  "Return a completion annotation for a SYM.
The annotation is usually a unicode representation of the macro
SYM's compiled representation, for example, if SYM is alpha, α
is returned."
  (catch 'found
    (dolist (var (list TeX-math-list TeX-math-default))
      (dolist (e var)
        (let ((val (cadr e)))
          (when (and (stringp val)
                     (string= val sym))
            (let ((char (nth 3 e)))
              (when char
                (throw 'found
                       (concat " " (char-to-string char)))))))))))

(defvar TeX-math-mode-menu)

;;;###autoload
(define-minor-mode TeX-math-mode
  "A minor mode with easy access to TeX math macros.

Easy insertion of TeX math symbols.  If you give a prefix argument,
the symbols will be surrounded by dollar signs.  The following
commands are defined:

\\{TeX-math-mode-map}"
  :init-value nil
  :lighter nil
  :keymap (list (cons (TeX-math-abbrev-prefix) TeX-math-keymap))
  (TeX-set-mode-name))

(easy-menu-define TeX-math-mode-menu
  TeX-math-mode-map
  "Menu used in math minor mode."
  TeX-math-menu)

(defcustom TeX-math-insert-function #'TeX-insert-macro
  "Function called with argument STRING to insert \\STRING."
  :group 'TeX-math
  :type 'function)

(defun TeX-math-insert (string dollar)
  "Insert \\STRING{}.  If DOLLAR is non-nil, put $'s around it.
If `TeX-electric-math' is non-nil wrap that symbols around the
string."
  (let ((active (TeX-active-mark))
        m closer)
    (if (and active (> (point) (mark)))
        (exchange-point-and-mark))
    (when dollar
      (insert (or (car TeX-electric-math) "$"))
      (save-excursion
        (if active (goto-char (mark)))
        ;; Store closer string for later reference.
        (setq closer (or (cdr TeX-electric-math) "$"))
        (insert closer)
        ;; Set temporal marker to decide whether to put the point
        ;; after the math mode closer or not.
        (setq m (point-marker))))
    (funcall TeX-math-insert-function string)
    (when dollar
      ;; If the above `TeX-math-insert-function' resulted in
      ;; inserting, e.g., a pair of "\langle" and "\rangle" by
      ;; typing "`(", keep the point between them.  Otherwise
      ;; move the point after the math mode closer.
      (if (= m (+ (point) (length closer)))
          (goto-char m))
      ;; Make temporal marker point nowhere not to slow down the
      ;; subsequent editing in the buffer.
      (set-marker m nil))))

(defun TeX-math-cal (char dollar)
  "Insert a {\\cal CHAR}.  If DOLLAR is non-nil, put $'s around it.
If `TeX-electric-math' is non-nil wrap that symbols around the
char."
  (interactive "*c\nP")
  (if dollar (insert (or (car TeX-electric-math) "$")))
  (if (member "latex2e" (TeX-style-list))
      (insert "\\mathcal{" (char-to-string char) "}")
    (insert "{\\cal " (char-to-string char) "}"))
  (if dollar (insert (or (cdr TeX-electric-math) "$"))))

(provide 'tex-math-mode)
