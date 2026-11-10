;;; gml-mode.el --- Major mode for IBM DCF SCRIPT/VS + GML documents  -*- lexical-binding: t; -*-

;; Keywords: languages, wp, tex
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1

;;; Commentary:

;; A major mode for documents in the DCF Release 3 dialect of SCRIPT/VS
;; with GML markup (the Starter Set of SH35-0050), as read by the `gml'
;; engine of this project.
;;
;; It knows the line structure of SCRIPT/VS: a line that begins with
;; the control word indicator `.' is a control word line, `.*' is a
;; comment, `...name' is a label, and a `;' followed by a control word
;; splits a line.  On text lines it knows GML tags (`:tag attr=v.' and
;; `:etag.'), and everywhere symbol references (`&name.', `&a(3)',
;; `&*1', `&L'&x').
;;
;; Features:
;;  - font-lock for control words (by kind), macros, labels, tags,
;;    attributes, symbols and :xmp examples;
;;  - comments with `.*' in column 1 (M-; works on lines and regions);
;;  - filling and auto-fill that never join control word lines, never
;;    fill :xmp examples or one-line tags (:h1, :dt ...) and never break
;;    a line so that the next one starts with `.';
;;  - outline and imenu by headings, plus macros, labels and tags;
;;  - tag insertion (C-c C-t) and closing of the innermost open
;;    structure (C-c C-e);
;;  - completion of tags, attributes, control words, macros and symbols;
;;  - eldoc for control words and Starter Set tags;
;;  - xref (M-.) for macros, symbols, labels, refid= ids, tags and .im
;;    files, also searching imbedded files and the profile;
;;  - flymake with the diagnostics of the `gml' engine;
;;  - C-c C-c runs gmlfmt to a chosen output, C-c C-v previews the HTML.
;;
;; Setup:
;;   (add-to-list 'load-path "/path/to/GML/emacs")
;;   (require 'gml-mode)            ; or: (autoload 'gml-mode "gml-mode" nil t)
;;   (add-hook 'gml-mode-hook #'flymake-mode)   ; optional

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'xref)
(require 'flymake)
(require 'compile)

(defgroup gml nil
  "Editing IBM DCF SCRIPT/VS documents with GML markup."
  :group 'languages
  :prefix "gml-")

;;;; Customisation

(defcustom gml-root nil
  "Root of the GML project (the directory with `gmlfmt').
When nil, it is searched upward from the document, so documents
inside the project find it; set it to use the project's tools for
documents elsewhere."
  :type '(choice (const :tag "Search upward" nil) directory))

(defcustom gml-program nil
  "Command list that runs the `gml' engine, e.g. (\"gml\" \"-share\" \"/x/share\").
When nil: the project's `_build/default/bin/main.exe' with
`-share ROOT/share' if `gml-root' finds it, else \"gml\"."
  :type '(choice (const :tag "Automatic" nil) (repeat string)))

(defcustom gml-gmlfmt-program nil
  "The gmlfmt script.  When nil, ROOT/gmlfmt or \"gmlfmt\" on the path."
  :type '(choice (const :tag "Automatic" nil) file))

(defcustom gml-profile "starter"
  "Profile passed to gml and gmlfmt with -profile (nil for none)."
  :type '(choice (const :tag "None" nil) string))

(defcustom gml-symbols nil
  "Symbols passed as -set NAME=VALUE to gml and gmlfmt.
An alist of (NAME . VALUE); handy as a file- or dir-local variable."
  :type '(alist :key-type string :value-type string)
  :safe (lambda (v) (and (listp v)
                         (cl-every (lambda (c) (and (consp c) (stringp (car c))
                                                    (stringp (cdr c))))
                                   v))))

(defcustom gml-output-format "html"
  "Default -to target of `gml-format'."
  :type '(choice (const "xml") (const "resolved") (const "scoped")
                 (const "semantic") (const "html") (const "tex") (const "fo")))

(defcustom gml-eol-tags
  '("h0" "h1" "h2" "h3" "h4" "h5" "h6" "title" "docnum" "date" "author"
    "dt" "gt" "figcap" "aline" "i1" "i2" "i3" "ih1" "ih2" "ih3")
  "Tags whose element ends at the end of the line (`.xd NAME eol').
Lines that start with one of them are never filled.  Tags given
`eol' by an `.xd' line of the buffer are added to these."
  :type '(repeat string))

;;;; Faces

(defface gml-control-word-face '((t :inherit font-lock-keyword-face))
  "Face for control words of the programmable layer (.se .if .dm ...).")
(defface gml-format-word-face '((t :inherit font-lock-builtin-face))
  "Face for formatting control words, which become hints (.sp .in .ce ...).")
(defface gml-file-word-face '((t :inherit font-lock-preprocessor-face))
  "Face for file control words (.im .ap .ef .qu).")
(defface gml-unknown-word-face '((t :inherit font-lock-warning-face))
  "Face for two-letter control words the engine does not know.")
(defface gml-macro-face '((t :inherit font-lock-function-call-face))
  "Face for macro calls on control word lines.")
(defface gml-label-face '((t :inherit font-lock-constant-face))
  "Face for `...label' lines and .go targets.")
(defface gml-tag-face '((t :inherit font-lock-type-face))
  "Face for GML start tags.")
(defface gml-end-tag-face '((t :inherit font-lock-type-face :slant italic))
  "Face for GML end tags.")
(defface gml-attribute-face '((t :inherit font-lock-variable-name-face))
  "Face for GML attribute names.")
(defface gml-attribute-value-face '((t :inherit font-lock-string-face))
  "Face for GML attribute values.")
(defface gml-symbol-face '((t :inherit font-lock-variable-use-face))
  "Face for symbol references (&name.).")
(defface gml-symbol-function-face '((t :inherit font-lock-builtin-face))
  "Face for symbol attributes and functions (&L' &'substr).")
(defface gml-separator-face '((t :inherit font-lock-warning-face))
  "Face for the control word separator `;' where it splits a line.")
(defface gml-example-face '((t :inherit fixed-pitch))
  "Face added to the lines of a :xmp example.")

;;;; Data: control words and tags

(defconst gml-control-words
  ;; (NAME KIND GRAMMAR DESCRIPTION).  KIND: ctl, file, fmt, gml, dev.
  '(("aa" gml "tag apf [(rules)] [endapf [(rules)]]" "associate a GML tag with its APF")
    ("an" ctl "c1 op c2 [line]" "and: combine with the last .if")
    ("ap" file "name [args]" "append a file")
    ("bc" fmt "on|off" "balance columns")
    ("bf" fmt "font... [=]" "begin font")
    ("bl" fmt "(blank line)" "blank line: space 1")
    ("bm" fmt "[v]" "bottom margin")
    ("br" fmt "" "break")
    ("bx" fmt "[off|can|new|set|add|char] positions" "box")
    ("cb" fmt "" "column begin")
    ("cc" fmt "[v]" "conditional column begin")
    ("cd" fmt "n positions" "column definition")
    ("ce" fmt "[n|on|off|text]" "center lines")
    ("cl" fmt "[h]" "column width")
    ("cm" ctl "text" "comment (the line is split at a separator)")
    ("cp" fmt "[v]" "conditional page eject")
    ("cs" ctl "n on|off|ignore|include" "conditional section")
    ("ct" ctl "text" "continue the previous text")
    ("dc" ctl "name char..." "define character: gml, cont, asep, cw, mcs ...")
    ("dd" dev "" "define data file id (not supported)")
    ("df" fmt "name spec" "define font")
    ("dh" fmt "n options" "define heading level")
    ("dl" dev "" "device word (not supported)")
    ("dm" ctl "name on|off | name /line/... | name(n) /line" "define macro")
    ("do" ctl "begin|end" "group lines for .if/.th/.el")
    ("dr" fmt "name spec" "define rule")
    ("dv" fmt "name [text|font f] /string" "define variable (not supported)")
    ("ec" ctl ".xx [operands]" "execute the native control word")
    ("ef" file "[close]" "end of file")
    ("el" ctl "line" "else: when the last .if was false")
    ("em" ctl ".xx [operands]" "execute a macro, also under .ms off")
    ("es" fmt "normal h | by f" "extra space")
    ("fl" fmt "on|off|dump ..." "floats")
    ("fn" fmt "on|off|leader" "footnote")
    ("fo" fmt "[on|off|left|right|center|inside|outside] [extend|fold|trunc]" "format mode")
    ("go" ctl "label" "go to ...label")
    ("h0" fmt "text" "heading level 0")
    ("h1" fmt "text" "heading level 1")
    ("h2" fmt "text" "heading level 2")
    ("h3" fmt "text" "heading level 3")
    ("h4" fmt "text" "heading level 4")
    ("h5" fmt "text" "heading level 5")
    ("h6" fmt "text" "heading level 6")
    ("hr" fmt "rule spec" "horizontal rule")
    ("hw" fmt "word" "hyphenate word")
    ("hy" fmt "[on|off] [options]" "hyphenation")
    ("ic" fmt "..." "include control")
    ("if" ctl "c1 op c2 [line]  (op: eq ne lt gt le ge = ¬= < > <= >=)" "conditional")
    ("il" fmt "h" "indent line")
    ("im" file "name [args]" "imbed a file")
    ("in" fmt "[h] [after v] [for v] [nobreak]" "indent")
    ("ir" fmt "[h] [after v] [for v] [nobreak]" "indent right")
    ("is" fmt "[to] h [blank|break|absolute]" "input space")
    ("it" dev "" "device word (not supported)")
    ("ju" fmt "[on|off]" "justify")
    ("kp" fmt "on|off|float|delay|inline|v[+v]" "keep")
    ("lb" fmt "(leading blank)" "leading blank: break")
    ("li" ctl "[n|on|off|text]" "literal: control words and tags off")
    ("ll" fmt "[h]" "line length")
    ("lo" dev "" "device word (not supported)")
    ("ls" fmt "[v] | normal v | by f | skip|space|text f [f]" "line spacing")
    ("lt" fmt "(leading tab)" "leading tab: break")
    ("mc" fmt "" "multicolumn mode")
    ("me" ctl "[line]" "macro exit")
    ("mg" ctl "/id/text/" "message")
    ("ms" ctl "on|off" "macro substitution")
    ("nf" fmt "[on|off|left|right|center|inside|outside]" "no format")
    ("nl" fmt "(null line)" "null line")
    ("oc" dev "" "output comment (not supported)")
    ("of" fmt "[h]" "offset (hanging indent)")
    ("or" ctl "c1 op c2 [line]" "or: combine with the last .if")
    ("pa" fmt "[n|odd|even|nostart|nobreak] [on|off]" "page eject")
    ("pf" fmt "" "previous font")
    ("pl" fmt "[v]" "page length")
    ("pm" fmt "[nobind] h [h]" "page margins")
    ("pn" fmt "on|off|offno|roman|arabic|alpha|n|pref x" "page numbering")
    ("pw" fmt "[h]" "page width")
    ("qq" file "" "quit quickly")
    ("qu" file "" "quit")
    ("rc" fmt "n [on|off|c] | adjust h | * c" "revision code")
    ("rf" fmt "[odd|even] on|off|cancel|sup|res|execute" "running footing")
    ("rh" fmt "[odd|even] on|off|cancel|sup|res|execute" "running heading")
    ("ri" fmt "[n|on|off|text]" "right-align lines")
    ("sb" fmt "[v]" "space before")
    ("sc" fmt "" "single column mode")
    ("se" ctl "name [=] value | name = 'str' | name off | name() = v" "set symbol")
    ("sk" fmt "[v] [a|c]" "skip lines")
    ("sp" fmt "[v] [a]" "space lines")
    ("su" ctl "on|off|n" "substitute symbols")
    ("sv" dev "" "device word (not supported)")
    ("sx" fmt "[f] [c] /left/fill/right/" "split text")
    ("sy" dev "" "system command (not supported)")
    ("tb" fmt "[add|del|set] stops" "tab settings")
    ("tc" fmt "[n] [title|/]" "table of contents")
    ("te" dev "" "terminal input (not supported)")
    ("th" ctl "line" "then: when the last .if was true")
    ("tm" fmt "[v]" "top margin")
    ("tp" fmt "[add|del|set] stops" "tab positions")
    ("tr" ctl "pairs of c|hh" "translate characters on output")
    ("ty" ctl "text" "type a message on the terminal")
    ("uc" fmt "[n|on|off|text]" "underscore and capitalize")
    ("ud" fmt "on|off|rule v" "underscore definition")
    ("un" fmt "h" "undent")
    ("up" fmt "[n|on|off|text]" "uppercase")
    ("ur" ctl "line" "use (substitute) the rest of the line")
    ("us" fmt "[n|on|off|text]" "underscore")
    ("uw" dev "" "device word (not supported)")
    ("vr" fmt "rule spec" "vertical rule")
    ("ws" fmt "normal h | by f" "word space")
    ("wz" fmt "on|off" "widow zone")
    ("gs" gml "tag|rules|prefix|exatt|attval|scan|copy|insert ..." "GML services")
    ("xa" gml "a=v..." "set attributes of the open element")
    ("xd" gml "name [closes=a,b] [closedby=x,y] [empty] [eol] [inline|block]" "element model")
    ("xe" gml "[name]" "end an element")
    ("xr" gml "backend /text/" "raw backend text")
    ("xs" gml "name a=v..." "start an element"))
  "Control words the gml engine knows: (NAME KIND GRAMMAR DESCRIPTION).")

(defconst gml-starter-tags
  ;; (NAME KIND DESCRIPTION ATTRIBUTES).  KIND: block (paired, on its own
  ;; lines), inline (paired, in text), text (takes the rest of the line),
  ;; line (alone on a line), ref (empty, needs refid=).
  '(("gdoc" block "general document" "sec")
    ("frontm" line "front matter")
    ("titlep" block "title page")
    ("title" text "document title (one line each)" "stitle")
    ("docnum" text "document number")
    ("date" text "date (&date. when empty)")
    ("author" text "author")
    ("address" block "address")
    ("aline" text "address line")
    ("abstract" line "abstract")
    ("preface" line "preface")
    ("toc" line "table of contents")
    ("figlist" line "list of figures")
    ("body" line "body")
    ("appendix" line "appendix section")
    ("backm" line "back matter")
    ("index" line "the index")
    ("h0" text "heading, level 0 (part)" "id stitle")
    ("h1" text "heading, level 1" "id stitle")
    ("h2" text "heading, level 2" "id stitle")
    ("h3" text "heading, level 3" "id stitle")
    ("h4" text "heading, level 4" "id stitle")
    ("h5" text "heading, level 5" "id stitle")
    ("h6" text "heading, level 6" "id stitle")
    ("p" text "paragraph")
    ("pc" text "paragraph continuation")
    ("note" text "note")
    ("ol" block "ordered list" "compact")
    ("ul" block "unordered list" "compact")
    ("sl" block "simple list" "compact")
    ("dl" block "definition list" "compact break headhi termhi tsize")
    ("gl" block "glossary list" "compact termhi")
    ("l" block "list" "compact")
    ("li" text "list item" "id")
    ("lp" text "list part")
    ("dthd" text "definition term heading")
    ("ddhd" text "definition description heading")
    ("dt" text "definition term (one line)")
    ("dd" text "definition description")
    ("gt" text "glossary term (one line)")
    ("gd" text "glossary description")
    ("lq" block "long quotation")
    ("xmp" block "example: lines kept as written" "depth")
    ("fig" block "figure" "id frame place width depth")
    ("figcap" text "figure caption (one line)")
    ("figdesc" text "figure description")
    ("fn" inline "footnote" "id")
    ("q" inline "quotation (nests)")
    ("hp0" inline "highlighted phrase 0")
    ("hp1" inline "highlighted phrase 1 (italic)")
    ("hp2" inline "highlighted phrase 2 (bold)")
    ("hp3" inline "highlighted phrase 3 (bold italic)")
    ("cit" inline "citation")
    ("psc" block "process-specific controls" "proc")
    ("hdref" ref "heading reference" "refid page")
    ("figref" ref "figure reference" "refid page")
    ("fnref" ref "footnote reference" "refid")
    ("liref" ref "list item reference" "refid page")
    ("i1" text "index entry, level 1" "id pg")
    ("i2" text "index entry, level 2" "id pg refid")
    ("i3" text "index entry, level 3" "pg refid")
    ("ih1" text "index heading, level 1" "id print see seeid")
    ("ih2" text "index heading, level 2" "id print see seeid")
    ("ih3" text "index heading, level 3" "id print see seeid")
    ("iref" ref "index reference" "refid pg see seeid"))
  "GML Starter Set tags: (NAME KIND DESCRIPTION ATTRIBUTES).")

(defconst gml-system-symbols
  '("SYSYEAR" "SYSMONTH" "SYSDAYOFM" "SYSDAYOFW" "SYSDAYOFY" "SYSHOUR"
    "SYSMINUTE" "SYSSECOND" "SYSVARH" "SYSVARP" "SYSVARD" "SYSVARS" "SYSVART"
    "$GML" "$EGML" "$CW" "$CONT" "$SU" "$LNUM" "$TAB" "$ENV" "$PN"
    "date" "time" "gml" "amp" "semi" "rbl" "oqq" "cqq" "oq" "cq")
  "System symbols and the text symbols of the Starter Set profile.")

(defconst gml--name-chars "A-Za-z0-9@#$"
  "Characters of tag, symbol and macro names (for skip-chars).")

(defconst gml--tag-re
  "\\(:e?\\)\\([A-Za-z@#$][A-Za-z0-9@#$]*\\)"
  "A GML start or end tag: group 1 the delimiter, group 2 the name.")

(defconst gml--control-re
  "^\\(\\.'?\\)\\([A-Za-z@#$][A-Za-z0-9@#$]*\\)"
  "A control word or macro at the start of a line.")

;;;; Buffer scanning helpers

(defun gml--collect (regexp group &optional buffer)
  "Distinct matches of GROUP of REGEXP in BUFFER (default current)."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (let (acc)
          (while (re-search-forward regexp nil t)
            (cl-pushnew (match-string-no-properties group) acc :test #'string=))
          (nreverse acc))))))

(defun gml--buffer-macros ()
  "Macro names defined with .dm in the buffer."
  (cl-remove "off" (gml--collect "^\\.'?dm[ \t]+\\([A-Za-z0-9@#$]+\\)" 1)
             :test #'string-equal))

(defun gml--buffer-tags ()
  "Tags associated with .aa in the buffer."
  (gml--collect "^\\.'?aa[ \t]+\\([A-Za-z0-9@#$]+\\)" 1))

(defun gml--buffer-paired-tags ()
  "Tags given an end APF by an .aa line of the buffer."
  (gml--collect
   "^\\.'?aa[ \t]+\\([A-Za-z0-9@#$]+\\)[ \t]+[^ \t\n(]+\\(?:[ \t]*([^)\n]*)\\)?[ \t]+[^ \t\n(]" 1))

(defun gml--buffer-symbols ()
  "Symbols set with .se in the buffer (without subscripts)."
  (gml--collect "^\\.'?se[ \t]+\\(\\*?[A-Za-z0-9@#$]+\\)" 1))

(defun gml--eol-tags ()
  "`gml-eol-tags' plus the tags given `eol' by .xd in the buffer."
  (append gml-eol-tags
          (gml--collect "^\\.'?xd[ \t]+\\([A-Za-z0-9@#$]+\\)[^\n]*\\_<eol\\_>" 1)))

(defun gml--tag-info (name)
  "The `gml-starter-tags' entry of NAME."
  (assoc (downcase name) gml-starter-tags))

(defun gml--paired-p (name)
  "Non-nil if tag NAME has an end tag."
  (or (memq (nth 1 (gml--tag-info name)) '(block inline))
      (member (downcase name) (gml--buffer-paired-tags))))

(defun gml--in-comment-p (pos)
  "Non-nil if POS is in a comment.  Point does not move."
  (save-excursion (nth 4 (syntax-ppss pos))))

(defun gml--comment-line-p ()
  "Non-nil if the current line is a .* or .cm comment."
  (save-excursion
    (beginning-of-line)
    (looking-at "\\.\\(?:\\*\\|cm\\_>\\)")))

(defun gml--control-line-p ()
  "Non-nil if the current line is a control word line (or a label)."
  (save-excursion (beginning-of-line) (eq (char-after) ?.)))

;;;; Syntax

(defvar gml-mode-syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?' "." st)
    (modify-syntax-entry ?\" "." st)
    (modify-syntax-entry ?& "_" st)
    (modify-syntax-entry ?@ "_" st)
    (modify-syntax-entry ?# "_" st)
    (modify-syntax-entry ?$ "_" st)
    st)
  "Syntax table of `gml-mode'.")

(defconst gml--syntax-propertize
  (syntax-propertize-rules
   ;; `.*' and `.cm' in column 1 begin a comment that runs to the end of
   ;; the line; a `.' anywhere else is punctuation.
   ("^\\(\\.\\)\\(?:\\*\\|cm\\_>\\)" (1 "<")))
  "Syntax propertization of `gml-mode'.")

;;;; Font lock

(defvar gml--tag-end nil
  "End of the markup of the tag being fontified.")

(defvar gml--tag-mcs-face nil
  "Face of the tag being fontified, for its markup control sequence.")

(defun gml--markup-end ()
  "From point, just after a tag name, the position where its markup ends.
That is the markup control sequence `.', the next tag or the end of
the line; quoted attribute values are skipped."
  (save-match-data
    (save-excursion
      (let ((eol (line-end-position)) end)
	(while (and (not end) (< (point) eol))
          (pcase (char-after)
            (?' (forward-char)
		(while (and (search-forward "'" eol 'move)
                            (eq (char-after) ?'))
                  (forward-char)))
            (?. (setq end (point)))
            (?: (if (looking-at gml--tag-re) (setq end (point)) (forward-char)))
            (_ (forward-char))))
	(or end (point))))))

(defun gml--match-tag (limit)
  "Font-lock matcher for GML tags before LIMIT."
  (let (found)
    (while (and (not found) (re-search-forward gml--tag-re limit t))
      (let ((name (match-string 2)))
        (if (and (<= (length name) 8)
                 (not (gml--in-comment-p (match-beginning 0))))
            (setq found t)
          ;; a name longer than 8 characters is text (tagscan.ml)
          (goto-char (match-end 0)))))
    found))

(defun gml--tag-face ()
  "Face of the tag just matched by `gml--match-tag'."
  (if (= (- (match-end 1) (match-beginning 1)) 2) 'gml-end-tag-face 'gml-tag-face))

(defconst gml--attr-re
  (concat "\\([A-Za-z@#$][A-Za-z0-9@#$]*\\)"
          "\\(?:[ \t]*=[ \t]*\\('\\(?:[^'\n]\\|''\\)*'?\\|[^ \t\n.]*\\)\\)?")
  "An attribute: group 1 its name, group 2 its value.")

(defun gml--match-attr (_limit)
  "Anchored font-lock matcher for the attributes of the current tag."
  (and (< (point) gml--tag-end)
       (re-search-forward gml--attr-re gml--tag-end t)))

(defun gml--match-mcs (_limit)
  "Anchored font-lock matcher for the `.' that ends the current tag."
  (when (<= (point) gml--tag-end)
    (goto-char gml--tag-end)
    (when (looking-at "\\.")
      (goto-char (match-end 0))
      t)))

(defun gml--control-word-face ()
  "Face for the control word or macro name in group 2."
  (let* ((name (downcase (match-string 2)))
         (entry (assoc name gml-control-words)))
    (cond
     ((> (length name) 2) 'gml-macro-face)
     ((null entry)
      (if (member name (mapcar #'downcase (gml--buffer-macros)))
          'gml-macro-face 'gml-unknown-word-face))
     (t (pcase (nth 1 entry)
          ('fmt 'gml-format-word-face)
          ('file 'gml-file-word-face)
          ('dev 'gml-unknown-word-face)
          (_ 'gml-control-word-face))))))

(defun gml--match-example (limit)
  "Font-lock matcher for the lines of :xmp examples before LIMIT."
  (let (found)
    (while (and (not found) (re-search-forward "^[^\n]*?:xmp\\_>[^\n]*\n" limit t))
      (let ((beg (match-end 0)))
        (unless (gml--in-comment-p (match-beginning 0))
          (let ((end (if (re-search-forward ":exmp\\_>" nil t)
                         (match-beginning 0)
                       (point-max))))
            (when (> end beg)
              (set-match-data (list beg end))
              (setq found t))))))
    found))

(defvar font-lock-beg)
(defvar font-lock-end)

(defun gml--font-lock-extend-region ()
  "Extend the font-lock region to whole :xmp examples."
  (let ((changed nil))
    (save-excursion
      (goto-char font-lock-beg)
      (when (and (re-search-backward ":\\(e?\\)xmp\\_>" nil t)
                 (string-empty-p (match-string 1))
                 (< (line-beginning-position) font-lock-beg))
        (setq font-lock-beg (line-beginning-position) changed t))
      (goto-char font-lock-end)
      (when (and (re-search-backward ":\\(e?\\)xmp\\_>" font-lock-beg t)
                 (string-empty-p (match-string 1))
                 (re-search-forward ":exmp\\_>" nil t)
                 (> (point) font-lock-end))
        (setq font-lock-end (line-end-position) changed t)))
    changed))

(defconst gml-font-lock-keywords
  `(;; labels
    ("^\\.\\.\\.\\([^ \t\n]*\\).*" (0 'gml-label-face))
    ;; control words and macros
    (,gml--control-re (2 (gml--control-word-face)))
    ;; a separator followed by a control word
    ("\\(;\\)\\.'?\\([A-Za-z@#$][A-Za-z0-9@#$]*\\)"
     (1 'gml-separator-face) (2 (gml--control-word-face)))
    ;; definitions
    ("^\\.'?dm[ \t]+\\([A-Za-z0-9@#$]+\\)\\(?:[ \t]+\\(on\\|off\\)\\_>\\)?"
     (1 (if (string-equal (match-string 1) "off")
            'gml-control-word-face 'font-lock-function-name-face))
     (2 'gml-control-word-face nil t))
    ("^\\.'?se[ \t]+\\(\\*?[A-Za-z0-9@#$]+\\)" (1 'font-lock-variable-name-face))
    (,(concat "^\\.'?aa[ \t]+\\([A-Za-z0-9@#$]+\\)"
              "\\(?:[ \t]+\\([^ \t\n(]+\\)\\)?"
              "\\(?:[ \t]*\\(([^)\n]*)\\)\\)?"
              "\\(?:[ \t]+\\([^ \t\n(]+\\)\\)?")
     (1 'gml-tag-face) (2 'font-lock-function-name-face nil t)
     (3 'font-lock-constant-face nil t) (4 'font-lock-function-name-face nil t))
    ("^\\.'?x[dse][ \t]+\\([A-Za-z0-9@#$]+\\)" (1 'gml-tag-face))
    ("\\(?:^\\|[ \t]\\)\\.'?go[ \t]+\\([^ \t\n;]+\\)" (1 'gml-label-face))
    ;; the line governed by .th/.el, or by an .if/.an/.or condition
    ("^\\.'?\\(?:th\\|el\\)[ \t]+\\.'?\\(?2:[A-Za-z@#$][A-Za-z0-9@#$]*\\)"
     (2 (gml--control-word-face)))
    (,(concat "^\\.'?\\(?:if\\|an\\|or\\)[ \t]+[^ \t\n]+[ \t]+[^ \t\n]+[ \t]+[^ \t\n]+"
              "[ \t]+\\.'?\\(?2:[A-Za-z@#$][A-Za-z0-9@#$]*\\)")
     (2 (gml--control-word-face)))
    ("^\\.'?\\(?:im\\|ap\\)[ \t]+\\([^ \t\n;]+\\)" (1 'font-lock-string-face))
    ;; comparison operators of .if/.an/.or
    ("^\\.'?\\(?:if\\|an\\|or\\)\\_>"
     ("[ \t]\\(eq\\|ne\\|lt\\|gt\\|le\\|ge\\|¬?=\\|\\^=\\|<>\\|[<>]=?\\)[ \t]"
      nil nil (1 'gml-control-word-face)))
    ;; tags with their attributes and the markup control sequence
    (gml--match-tag
     (1 (gml--tag-face)) (2 (gml--tag-face))
     (gml--match-attr
      (progn (setq gml--tag-end (gml--markup-end)
                   gml--tag-mcs-face (gml--tag-face))
             nil) nil
      (1 'gml-attribute-face) (2 'gml-attribute-value-face nil t))
     (gml--match-mcs nil nil (0 gml--tag-mcs-face)))
    ;; symbol attributes and functions, then references
    ("&\\(?:[A-Za-z][A-Za-z]?'\\|'[A-Za-z]+\\)" (0 'gml-symbol-function-face))
    ("&\\(?:\\*[A-Za-z0-9@#$]*\\|[A-Za-z0-9@#$]+\\)\\(?:([^)\n]*)\\)?\\.?"
     (0 'gml-symbol-face))
    ;; examples
    (gml--match-example (0 'gml-example-face append)))
  "Font-lock keywords of `gml-mode'.")

;;;; Comments

(defun gml-comment-dwim (arg)
  "Comment or uncomment with `.*' in column 1.
With an active region, (un)comment its lines; otherwise toggle the
comment on the current line (or ARG lines).  A SCRIPT/VS comment
is a whole line, so no comment is added at the end of a line."
  (interactive "*P")
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end) arg)
    (let ((n (prefix-numeric-value arg)))
      (comment-or-uncomment-region
       (line-beginning-position)
       (save-excursion (forward-line (1- n)) (line-end-position))))))

;;;; Filling

(defun gml--unfillable-line-p ()
  "Non-nil if the current line must not be filled or broken."
  (save-excursion
    (beginning-of-line)
    (or (eq (char-after) ?.)
        (gml--in-example-p)
        (and (looking-at ":\\([A-Za-z0-9@#$]+\\)")
             (member (downcase (match-string 1)) (gml--eol-tags))))))

(defun gml--in-example-p (&optional pos)
  "Non-nil if POS (default point) is inside a :xmp example."
  (save-excursion
    (when pos (goto-char pos))
    (beginning-of-line)
    (and (re-search-backward ":\\(e?\\)xmp\\_>" nil t)
         (string-empty-p (match-string 1)))))

(defun gml--fill-nobreak-p ()
  "Do not break where the next line would start with `.' or inside a tag."
  (or (eq (char-after) ?.)
      (save-excursion
        (let ((pos (point)) (bol (line-beginning-position)) in)
          (goto-char bol)
          (while (and (not in) (re-search-forward gml--tag-re pos t))
            (when (> (gml--markup-end) pos) (setq in t))
            (goto-char (match-end 0)))
          in))))

(defun gml-fill-paragraph (&optional justify)
  "Fill the paragraph at point, unless it is a control line or example.
JUSTIFY is passed on to `fill-paragraph'."
  (if (gml--unfillable-line-p)
      t
    (let ((fill-paragraph-function nil))
      (fill-paragraph justify))))

(defun gml-do-auto-fill ()
  "Auto-fill, except on lines that must not be broken."
  (unless (gml--unfillable-line-p)
    (do-auto-fill)))

(defun gml--paragraph-separate ()
  "Regexp for `paragraph-separate': blank, control and one-line tag lines."
  (concat "[ \t\f]*$\\|\\.\\|:\\(?:" (regexp-opt (gml--eol-tags)) "\\)\\_>"))

;;;; Outline and imenu

(defconst gml--outline-re
  "\\(?::\\(?:h[0-6]\\|frontm\\|body\\|appendix\\|backm\\)\\|\\.h[0-6]\\)\\_>"
  "Outline headings: :h0-:h6, .h0-.h6 and the document sections.")

(defun gml-outline-level ()
  "Outline level: sections 1, h0 2 ... h6 8."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[:.]h\\([0-6]\\)") (+ 2 (string-to-number (match-string 1))) 1)))

(defun gml--heading-text (bound)
  "Text of the heading after the tag at point, up to BOUND."
  (let ((end (gml--markup-end)))
    (string-trim
     (buffer-substring-no-properties
      (if (eq (char-after end) ?.) (1+ end) end) bound))))

(defun gml-imenu-create-index ()
  "Imenu index: headings, then macros, labels, tags and ids."
  (let (heads macros labels tags ids)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[:.]h\\([0-6]\\)" nil t)
        (let ((lvl (string-to-number (match-string 1)))
              (pos (match-beginning 0)))
          (unless (gml--in-comment-p pos)
            (push (cons (concat (make-string (* 2 (max 0 (1- lvl))) ?\s)
                                (gml--heading-text (line-end-position)))
                        pos)
                  heads))))
      (dolist (spec `((macros "^\\.'?dm[ \t]+\\([A-Za-z0-9@#$]+\\)")
                      (labels "^\\.\\.\\.\\([^ \t\n]+\\)")
                      (tags "^\\.'?aa[ \t]+\\([A-Za-z0-9@#$]+\\)")
                      (ids "\\_<id[ \t]*=[ \t]*'?\\([A-Za-z0-9@#$]+\\)")))
        (goto-char (point-min))
        (let (acc)
          (while (re-search-forward (cadr spec) nil t)
            (unless (or (string-equal (match-string 1) "off")
                        (assoc (match-string 1) acc))
              (push (cons (match-string-no-properties 1) (match-beginning 1)) acc)))
          (pcase (car spec)
            ('macros (setq macros acc)) ('labels (setq labels acc))
            ('tags (setq tags acc)) ('ids (setq ids acc))))))
    (append (nreverse heads)
            (delq nil
                  (list (and macros (cons "Macros" (nreverse macros)))
                        (and labels (cons "Labels" (nreverse labels)))
                        (and tags (cons "Tags" (nreverse tags)))
                        (and ids (cons "Ids" (nreverse ids))))))))

;;;; Tag insertion and closing

(defun gml--all-tags ()
  "Every known tag name: the Starter Set and the buffer's .aa tags."
  (cl-remove-duplicates
   (append (mapcar #'car gml-starter-tags) (mapcar #'downcase (gml--buffer-tags)))
   :test #'string= :from-end t))

(defun gml--tag-kind (name)
  "Insertion kind of tag NAME (see `gml-starter-tags')."
  (or (nth 1 (gml--tag-info name))
      (if (member name (gml--buffer-paired-tags)) 'block 'text)))

(defun gml--fresh-line ()
  "Move to the start of a fresh line, opening one if need be."
  (unless (bolp)
    (if (save-excursion (skip-chars-forward " \t") (eolp))
        (progn (delete-horizontal-space) (insert "\n"))
      (insert "\n"))))

(defun gml-insert-tag (name)
  "Insert the GML tag NAME, with its end tag if it has one.
With an active region, a paired tag wraps it.  Block structures go
on lines of their own; a reference is left at `refid='."
  (interactive
   (list (downcase (completing-read "Tag: " (gml--all-tags) nil nil))))
  (let* ((kind (gml--tag-kind name))
         (region (and (use-region-p)
                      (cons (copy-marker (region-beginning))
                            (copy-marker (region-end) t)))))
    (pcase kind
      ('block
       (if region
           (progn
             (goto-char (cdr region))
             (unless (bolp) (insert "\n"))
             (insert ":e" name "\n")
             (goto-char (car region))
             (gml--fresh-line)
             (insert ":" name "\n"))
         (gml--fresh-line)
         (insert ":" name "\n")
         (save-excursion (insert "\n:e" name "\n"))))
      ('inline
        (if region
            (progn (goto-char (cdr region)) (insert ":e" name ".")
                   (goto-char (car region)) (insert ":" name "."))
          (insert ":" name ".")
          (save-excursion (insert ":e" name "."))))
      ('ref
       (insert ":" name " refid=")
       (save-excursion (insert ".")))
      ('line
       (gml--fresh-line)
       (insert ":" name "\n"))
      (_
       ;; paragraph-level tags and one-line tags begin a line
       (when (or (member name (gml--eol-tags))
                 (member name '("p" "pc" "note" "li" "lp" "dthd" "ddhd" "dd"
                                "gd" "figdesc")))
         (gml--fresh-line))
       (insert ":" name ".")))
    (when region
      (set-marker (car region) nil) (set-marker (cdr region) nil)
      (deactivate-mark))))

(defun gml--section-tag-p (name)
  "Non-nil if NAME ends every open structure (a heading or a section)."
  (member name '("h0" "h1" "h2" "h3" "h4" "h5" "h6" "frontm" "body"
                 "appendix" "backm" "abstract" "preface" "toc" "figlist" "index")))

(defun gml-open-structures (&optional pos)
  "The paired tags open at POS (default point), innermost first.
A heading or a document section ends the structures open before it,
as the Starter Set's DSM#RSET does, except :gdoc."
  (let ((pos (or pos (point))) (paired (gml--buffer-paired-tags)) stack)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward gml--tag-re pos t)
        (let ((endp (= (- (match-end 1) (match-beginning 1)) 2))
              (name (downcase (match-string-no-properties 2))))
          (unless (or (gml--in-comment-p (match-beginning 0))
                      (> (length name) 8))
            (cond
             ((and endp (member name stack))
              (setq stack (cdr (member name stack))))
             (endp nil)
             ((gml--section-tag-p name)
              (setq stack (seq-filter (lambda (n) (string= n "gdoc")) stack)))
             ((or (memq (nth 1 (gml--tag-info name)) '(block inline))
                  (member name paired))
              (push name stack)))))))
    stack))

(defun gml-close-tag ()
  "Insert the end tag of the innermost open structure."
  (interactive "*")
  (let ((name (car (gml-open-structures))))
    (unless name (user-error "No open structure"))
    (if (eq (gml--tag-kind name) 'inline)
        (insert ":e" name ".")
      (gml--fresh-line)
      (insert ":e" name)
      (unless (eolp) (insert "\n")))))

(defun gml-insert-macro (name)
  "Insert an inline macro definition `.dm NAME on' ... `.dm off'."
  (interactive "*sMacro name: ")
  (gml--fresh-line)
  (insert ".dm " name " on\n")
  (save-excursion (insert "\n.dm off\n")))

;;;; Completion

(defun gml-completion-at-point ()
  "Complete tags, attributes, control words, macros and symbols."
  (let ((pos (point)))
    (save-excursion
      (skip-chars-backward gml--name-chars)
      (let ((beg (point)) (bol (line-beginning-position)))
        (cond
         ;; &symbol
         ((or (eq (char-before) ?&)
              (and (eq (char-before) ?*) (eq (char-before (1- (point))) ?&)))
          (when (eq (char-before) ?*) (setq beg (1- beg)))
          (list beg pos
                (completion-table-dynamic
                 (lambda (_) (cl-remove-duplicates
                              (append (gml--buffer-symbols) gml-system-symbols)
                              :test #'string=)))
                :exclusive 'no))
         ;; .control word or macro at the start of a line
         ((or (and (eq (char-before) ?.) (= (1- beg) bol))
              (and (eq (char-before) ?') (= (- beg 2) bol) (eq (char-after bol) ?.)))
          (list beg pos
                (completion-table-dynamic
                 (lambda (_) (append (mapcar #'car gml-control-words)
                                     (gml--buffer-macros))))
                :annotation-function
                (lambda (c) (when-let* ((e (assoc c gml-control-words)))
                              (concat "  " (nth 3 e))))
                :exclusive 'no))
         ;; :tag or :etag
         ((eq (char-before) ?:)
          (list beg pos
                (completion-table-dynamic (lambda (_) (gml--all-tags)))
                :annotation-function
                (lambda (c) (when-let* ((e (gml--tag-info c)))
                              (concat "  " (nth 2 e))))
                :exclusive 'no))
         ;; an attribute inside a tag
         ((when-let* ((tag (gml--tag-around beg)))
            (when-let* ((attrs (nth 3 (gml--tag-info tag))))
              (list beg pos (split-string attrs) :exclusive 'no)))))))))

(defun gml--tag-around (pos)
  "Name of the tag whose markup contains POS, if any."
  (save-excursion
    (goto-char pos)
    (let ((bol (line-beginning-position)) found)
      (while (and (not found) (re-search-backward gml--tag-re bol t))
        (let ((name (match-string-no-properties 2)))
          (save-excursion
            (goto-char (match-end 0))
            (when (<= pos (gml--markup-end))
              (setq found (downcase name))))))
      found)))

;;;; Eldoc

(defun gml-eldoc-function (&rest _)
  "Describe the control word or tag at point."
  (save-excursion
    (let ((pos (point)) (bol (line-beginning-position)))
      (or
       ;; on a tag (or in its attributes)
       (let ((tag (save-excursion
                    (skip-chars-backward gml--name-chars)
                    (when (eq (char-before) ?:) (backward-char))
                    (if (looking-at gml--tag-re)
                        (downcase (match-string 2))
                      (gml--tag-around pos)))))
         (when-let* ((e (and tag (gml--tag-info tag))))
           (format ":%s  %s%s" (car e) (nth 2 e)
                   (if (nth 3 e) (concat "   [" (nth 3 e) "]") ""))))
       ;; on a control word line
       (progn
         (goto-char bol)
         (when (looking-at gml--control-re)
           (let* ((name (downcase (match-string 2)))
                  (e (assoc name gml-control-words)))
             (cond
              (e (format ".%s %s   %s" name (nth 2 e) (nth 3 e)))
              ((member name (mapcar #'downcase (gml--buffer-macros)))
               (format ".%s   macro" name))))))))))

;;;; Running the engine

(defun gml--root ()
  "The GML project root, or nil."
  (or gml-root
      (and default-directory
           (when-let* ((d (locate-dominating-file default-directory "gmlfmt")))
             (and (file-directory-p (expand-file-name "xslt" d))
                  (file-name-as-directory (expand-file-name d)))))))

(defun gml--engine-command ()
  "Command list that runs the gml engine."
  (or gml-program
      (let* ((root (gml--root))
             (exe (and root (expand-file-name "_build/default/bin/main.exe" root))))
        (if (and exe (file-executable-p exe))
            (list exe "-share" (expand-file-name "share" root))
          (list "gml")))))

(defun gml--set-args ()
  "The -set arguments for `gml-symbols'."
  (mapcan (lambda (c) (list "-set" (format "%s=%s" (car c) (cdr c)))) gml-symbols))

(defun gml--gmlfmt ()
  "The gmlfmt program."
  (or gml-gmlfmt-program
      (let ((root (gml--root)))
        (if root (expand-file-name "gmlfmt" root) "gmlfmt"))))

(defun gml--gmlfmt-environment ()
  "`process-environment' for gmlfmt: GML set to the fast engine command."
  (let ((cmd (gml--engine-command)))
    (if (and (null gml-program) (string= (car cmd) "gml"))
        process-environment
      (cons (concat "GML=" (mapconcat #'shell-quote-argument cmd " "))
            process-environment))))

(defun gml--gmlfmt-command (to file)
  "Shell command running gmlfmt -to TO on FILE."
  (mapconcat #'shell-quote-argument
             (append (list (gml--gmlfmt) "-to" to)
                     (and gml-profile (list "-profile" gml-profile))
                     (gml--set-args)
                     (list (file-name-nondirectory file)))
             " "))

(defun gml--output-file (to file)
  "The output file of format TO for FILE."
  (concat (file-name-sans-extension file)
          (pcase to
            ("xml" ".xml") ("resolved" ".r.xml") ("scoped" ".s.xml")
            ("semantic" ".sem.xml") ("html" ".html") ("tex" ".tex") ("fo" ".fo"))))

(defun gml-format (to)
  "Run gmlfmt -to TO on the visited file, writing FILE.EXT next to it.
Diagnostics go to a compilation buffer."
  (interactive
   (list (if current-prefix-arg
             (completing-read "Format: " '("xml" "resolved" "scoped" "semantic"
                                           "html" "tex" "fo")
                              nil t nil nil gml-output-format)
           gml-output-format)))
  (let ((file (or buffer-file-name (user-error "Buffer is not visiting a file"))))
    (save-buffer)
    (let ((process-environment (gml--gmlfmt-environment))
          (out (gml--output-file to file)))
      (compilation-start
       (format "%s > %s" (gml--gmlfmt-command to file)
               (shell-quote-argument (file-name-nondirectory out)))
       nil (lambda (_) "*gml*")))))

(defun gml-preview ()
  "Make the HTML of the visited file and show it in a browser."
  (interactive)
  (let* ((file (or buffer-file-name (user-error "Buffer is not visiting a file")))
         (out (gml--output-file "html" file))
         (buf (progn (save-buffer)
                     (let ((process-environment (gml--gmlfmt-environment)))
                       (compilation-start
                        (format "%s > %s" (gml--gmlfmt-command "html" file)
                                (shell-quote-argument (file-name-nondirectory out)))
                        nil (lambda (_) "*gml*"))))))
    (with-current-buffer buf
      (add-hook 'compilation-finish-functions
                (lambda (_buf status)
                  (if (string-prefix-p "finished" status)
                      (browse-url-of-file out)
                    (message "gml: HTML not made: %s" (string-trim status))))
                nil t))))

;;;; Compilation

(add-to-list 'compilation-error-regexp-alist-alist
             '(gml "^\\([^ :\n]+\\):\\([0-9]+\\)\\(?: ([^)\n]*)\\)?: \\(?:error\\|\\(warning\\)\\|\\(info\\)\\):"
                   1 2 nil (3 . 4)))
(add-to-list 'compilation-error-regexp-alist 'gml)

;;;; Flymake

(defvar-local gml--flymake-proc nil
  "The running flymake check of the buffer.")

(defconst gml--diag-re
  (concat "^\\([^:\n]+\\)\\(?::\\([0-9]+\\)\\)?\\(?: ([^)\n]*)\\)?: "
          "\\(error\\|warning\\|info\\): \\(.*\\)$")
  "A diagnostic of the gml engine: file, line, severity, message.")

(defun gml--parse-diagnostics (output file source)
  "Flymake diagnostics for SOURCE from the gml OUTPUT about FILE.
Diagnostics in other files (imbeds, the profile) go on line 1,
with the file named in the message."
  (let (diags)
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (re-search-forward gml--diag-re nil t)
        (let* ((dfile (match-string 1))
               (line (and (match-string 2) (string-to-number (match-string 2))))
               (type (pcase (match-string 3)
                       ("error" :error) ("warning" :warning) (_ :note)))
               (msg (match-string 4))
               (here (string= (file-name-nondirectory dfile)
                              (file-name-nondirectory file))))
          (unless here
            (setq msg (format "%s:%s: %s" dfile (or line "") msg) line 1))
          (with-current-buffer source
            (let ((reg (flymake-diag-region source (max 1 (or line 1)))))
              (push (flymake-make-diagnostic source (car reg) (cdr reg) type msg)
                    diags))))))
    (nreverse diags)))

(defun gml-flymake (report-fn &rest _)
  "Flymake backend: run the gml engine on the buffer, call REPORT-FN."
  (when (process-live-p gml--flymake-proc)
    (kill-process gml--flymake-proc))
  (let* ((source (current-buffer))
         (dir (if buffer-file-name (file-name-directory buffer-file-name)
                default-directory))
         (tmp (make-temp-file "gml-flymake" nil ".gml"))
         (cmd (append (gml--engine-command)
                      (and gml-profile (list "-profile" gml-profile))
                      (gml--set-args)
                      (list "-I" (expand-file-name dir) "-o" null-device tmp))))
    (save-restriction
      (widen)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region nil nil tmp nil 'silent)))
    (let ((default-directory dir))
      (condition-case err
          (setq gml--flymake-proc
                (make-process
                 :name "gml-flymake" :noquery t :connection-type 'pipe
                 :buffer (generate-new-buffer " *gml-flymake*")
                 :command cmd
                 :sentinel
                 (lambda (proc _event)
                   (when (memq (process-status proc) '(exit signal))
                     (unwind-protect
                         (when (and (buffer-live-p source)
                                    (eq proc (buffer-local-value 'gml--flymake-proc source)))
                           (funcall report-fn
                                    (gml--parse-diagnostics
                                     (with-current-buffer (process-buffer proc) (buffer-string))
                                     tmp source)))
                       (delete-file tmp)
                       (kill-buffer (process-buffer proc)))))))
        (error (delete-file tmp)
               (funcall report-fn :panic
                        :explanation (format "cannot run %s: %s" (car cmd)
                                             (error-message-string err))))))))

;;;; Xref

(defun gml--xref-backend () "Xref backend of `gml-mode'." 'gml)

(defun gml--identifier-at-point ()
  "Identifier at point, as a string with a kind prefix.
`&' symbol, `.' macro, `...' label, `#' id, `:' tag, `<' file."
  (save-excursion
    (let ((pos (point)) (bol (line-beginning-position)))
      (skip-chars-backward gml--name-chars)
      (let* ((beg (point))
             (word (progn (skip-chars-forward gml--name-chars)
                          (buffer-substring-no-properties beg (point)))))
        (goto-char beg)
        (cond
         ((string-empty-p word)
          (goto-char bol)
          (when (and (looking-at "^\\.'?\\(?:im\\|ap\\)[ \t]+\\([^ \t\n;]+\\)")
                     (<= (match-beginning 1) pos))
            (concat "<" (match-string-no-properties 1))))
         ((memq (char-before) '(?& ?*))
          (concat "&" (if (eq (char-before) ?*) "*" "") word))
         ((eq (char-before) ?:)
          (concat ":" (if (save-excursion (backward-char) (looking-at gml--tag-re))
                          (downcase (match-string-no-properties 2))
                        word)))
         ((looking-back "\\_<refid[ \t]*=[ \t]*'?" bol) (concat "#" word))
         ((looking-back "\\(?:^\\|[ \t]\\)\\.'?go[ \t]+" bol)
          (concat "..." word))
         ((save-excursion
            (goto-char bol)
            (looking-at "^\\.'?\\(?:im\\|ap\\)[ \t]+\\([^ \t\n;]+\\)"))
          (concat "<" (match-string-no-properties 1)))
         ((or (and (eq (char-before) ?.) (= (1- beg) bol))
              (and (eq (char-before) ?') (= (- beg 2) bol)))
          (concat "." word))
         ((save-excursion (goto-char bol) (looking-at "^\\.'?aa[ \t]"))
          (concat "." word))
         (t nil))))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql gml)))
  (gml--identifier-at-point))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql gml)))
  (append (mapcar (lambda (s) (concat "&" s)) (gml--buffer-symbols))
          (mapcar (lambda (s) (concat "." s)) (gml--buffer-macros))
          (mapcar (lambda (s) (concat ":" s)) (gml--all-tags))))

(defun gml--resolve-file (name dir)
  "The file that `.im NAME' reads, searched in DIR then the profile directory."
  (let ((name (string-trim name "(" ")")))
    (cl-loop for d in (delq nil (list dir (when-let* ((r (gml--root)))
                                            (expand-file-name "share/profile" r))))
             thereis (cl-loop for n in (if (string-suffix-p ".gml" name)
                                           (list name)
                                         (list name (concat name ".gml")))
                              for f = (expand-file-name n d)
                              thereis (and (file-regular-p f) f)))))

(defun gml--search-files ()
  "Files searched for definitions: imbedded files, then the profile."
  (let ((dir (if buffer-file-name (file-name-directory buffer-file-name)
               default-directory))
        files)
    (dolist (n (gml--collect "^\\.'?\\(?:im\\|ap\\)[ \t]+\\([^ \t\n;]+\\)" 1))
      (when-let* ((f (gml--resolve-file n dir))) (push f files)))
    (when-let* ((p (and gml-profile (gml--resolve-file gml-profile dir))))
      (push p files))
    (cl-remove-duplicates (nreverse files) :test #'string=)))

(defun gml--definition-regexps (id)
  "Regexps whose group 1 is a definition of identifier ID."
  (let ((kind (substring id 0 1)) (name (regexp-quote (substring id 1))))
    (pcase kind
      ("&" (list (concat "^\\.'?se[ \t]+\\(" name "\\)\\_>")))
      ("." (if (string-prefix-p "..." id)
               (list (concat "^\\.\\.\\.\\(" (regexp-quote (substring id 3)) "\\)\\_>"))
             (list (concat "^\\.'?dm[ \t]+\\(" name "\\)\\_>"))))
      ("#" (list (concat "\\_<id[ \t]*=[ \t]*'?\\(" name "\\)\\_>")))
      (":" (list (concat "^\\.'?aa[ \t]+\\(" name "\\)\\_>")
                 (concat "^\\.'?xd[ \t]+\\(" name "\\)\\_>"))))))

(defun gml--xrefs-in (regexps buffer file)
  "Xref items for REGEXPS in BUFFER (visiting FILE, or nil for itself)."
  (with-current-buffer buffer
    (save-excursion
      (let ((case-fold-search t) acc)
        (dolist (re regexps)
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (let* ((line (line-number-at-pos (match-beginning 1)))
                   (col (save-excursion (goto-char (match-beginning 1)) (current-column)))
                   (summary (buffer-substring (line-beginning-position) (line-end-position))))
              (push (xref-make summary
                               (if file
                                   (xref-make-file-location file line col)
                                 (xref-make-buffer-location buffer (match-beginning 1))))
                    acc))))
        (nreverse acc)))))

(cl-defmethod xref-backend-definitions ((_backend (eql gml)) id)
  (if (string-prefix-p "<" id)
      (let* ((dir (if buffer-file-name (file-name-directory buffer-file-name)
                    default-directory))
             (f (gml--resolve-file (substring id 1) dir)))
        (and f (list (xref-make f (xref-make-file-location f 1 0)))))
    (let* ((res (gml--definition-regexps id))
           (here (gml--xrefs-in res (current-buffer) nil)))
      (or here
          (cl-loop for f in (gml--search-files)
                   nconc (with-temp-buffer
                           (insert-file-contents f)
                           (gml--xrefs-in res (current-buffer) f)))))))

;;;; Keymap, menu and the mode

(defvar gml-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-t") #'gml-insert-tag)
    (define-key m (kbd "C-c C-e") #'gml-close-tag)
    (define-key m (kbd "C-c /") #'gml-close-tag)
    (define-key m (kbd "C-c C-d") #'gml-insert-macro)
    (define-key m (kbd "C-c C-c") #'gml-format)
    (define-key m (kbd "C-c C-v") #'gml-preview)
    (define-key m [remap comment-dwim] #'gml-comment-dwim)
    m)
  "Keymap of `gml-mode'.")

(easy-menu-define gml-mode-menu gml-mode-map "Menu of `gml-mode'."
  '("GML"
    ["Insert tag..." gml-insert-tag]
    ["Close structure" gml-close-tag]
    ["Define macro..." gml-insert-macro]
    "---"
    ["Format with gmlfmt" gml-format]
    ["Format to..." (let ((current-prefix-arg '(4))) (call-interactively #'gml-format))]
    ["Preview HTML" gml-preview]
    "---"
    ["Check with gml (Flymake)" flymake-mode
     :style toggle :selected (bound-and-true-p flymake-mode)]
    ["Show diagnostics" flymake-show-buffer-diagnostics]))

;;;###autoload
(define-derived-mode gml-mode text-mode "GML"
  "Major mode for IBM DCF SCRIPT/VS documents with GML markup.

Control word lines begin with `.', comments with `.*', labels with
`...'; GML tags are `:tag attr=value.' and `:etag.'; symbols are
`&name.'.  Lines are significant to SCRIPT/VS, so filling never joins
control word lines and never starts a line with `.'.

\\{gml-mode-map}"
  :syntax-table gml-mode-syntax-table
  (setq-local syntax-propertize-function gml--syntax-propertize)
  (setq-local comment-start ".* ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "^\\.\\(?:\\*\\|cm\\_>\\) *")
  (setq-local comment-style 'plain)
  (setq-local comment-padding "")
  (setq-local comment-use-syntax t)
  (setq-local font-lock-defaults '(gml-font-lock-keywords nil t))
  (setq-local font-lock-multiline t)
  (add-hook 'font-lock-extend-region-functions #'gml--font-lock-extend-region nil t)
  ;; filling
  (setq-local paragraph-start (concat "[ \t\f]*$\\|[.:]\\|[ \t]+[^ \t\n]"))
  (setq-local paragraph-separate (gml--paragraph-separate))
  (setq-local adaptive-fill-mode nil)
  (setq-local fill-paragraph-function #'gml-fill-paragraph)
  (setq-local normal-auto-fill-function #'gml-do-auto-fill)
  (add-hook 'fill-nobreak-predicate #'gml--fill-nobreak-p nil t)
  ;; a leading blank is a break in SCRIPT/VS: never indent automatically
  (setq-local indent-line-function #'indent-relative)
  (setq-local electric-indent-inhibit t)
  (setq-local indent-tabs-mode nil)
  ;; navigation
  (setq-local outline-regexp gml--outline-re)
  (setq-local outline-level #'gml-outline-level)
  (setq-local imenu-create-index-function #'gml-imenu-create-index)
  (add-hook 'xref-backend-functions #'gml--xref-backend nil t)
  (add-hook 'completion-at-point-functions #'gml-completion-at-point nil t)
  (add-hook 'eldoc-documentation-functions #'gml-eldoc-function nil t)
  (add-hook 'flymake-diagnostic-functions #'gml-flymake nil t)
  (setq-local case-fold-search t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gml\\'" . gml-mode))

(provide 'gml-mode)

;;; gml-mode.el ends here
