;; Emacs mode for ADP (Algebraic Dynamic Programming) ASCII notation
;;
;; include this file in your emacs via copying it to ~/.emacs-lisp/ and 
;; adding the following two lines to your ~/.emacs file:
;;   (setq load-path  (cons (expand-file-name "~/.emacs-lisp/") load-path))
;;   (require 'adp-mode)
;;
;; some links if you extend this mode or write some new language orientated
;; emacs mode:
;;   http://two-wugs.net/emacs/mode-tutorial.html
;;   http://www.gnu.org/software/emacs/elisp-manual/html_node/index.html
;;   http://www.emacswiki.org/cgi-bin/wiki?SampleMode
;;
;;   TODO
;;     indentation


;; other elisp code can check for this
(defvar adp-mode-hook nil)

;; auto-load if adp-mode if opened filename ends with .adp
;; alternatively enter Meta+x adp-mode in emacs
(add-to-list 'auto-mode-alist '("\\.adp\\'" . adp-mode))


;; for a list of font-lock-foo-face constants (?) look into info lisp
;; or http://www.gnu.org/software/emacs/elisp-manual/html_node/Faces-for-Font-Lock.html#Faces-for-Font-Lock
(defconst adp-font-lock-keywords-1
  (list
   '("#\\(import\\|algebratype\\|extern\\|algebra\\|grammar\\)" . font-lock-keyword-face)
   '("type\\|where" . font-lock-keyword-face)
   ; \\sw enthaelt _'
   '("\\(\\sw+\\) *::" (1 font-lock-function-name-face))
   '("\\(\\sw+\\) .*= " (1 font-lock-function-name-face))
   '("#algebra\\[\\([a-zA-Z_0-9]+\\)" (1 font-lock-function-name-face))
   '("type +\\([A-Za-z_0-9]+\\)" (1 font-lock-type-face))
   '("Char\\|Int\\|String" . font-lock-type-face)
   ; font-lock-builtin-face possible because they are builtin functions
   ; but to make them easier to spot:
   '(" \\(<<<\\|[~-]~[-~]\\||||\\|\\.\\.\\.\\)\\($\\| \\)" (1 font-lock-warning-face))
   '("\\<\\(id\\|sum\\|minimum\\|maximum\\|char\\|decode\\|tabulated\\|axiom\\)\\>" (1 font-lock-builtin-face))
   '("\\<\\(with\\|empty\\|achar\\|astring\\|astringp\\|char\\|string\\|loc\\|minsize\\|maxsize\\|size\\|pairing\\)\\>" (1 font-lock-builtin-face))
   '("\\<\\(basepairing\\|stackpairing\\|region\\|if\\|then\\)\\>" (1 font-lock-builtin-face))
   '("[ A-Za-z0-9']\\(->\\|::\\)[ A-Za-z0-9']" (1 font-lock-builtin-face))
   '("[ A-Za-z0-9'\"]\\(\\+\\+\\|\\+\\|-\\|\\*\\|/\\|\\*\\*\\|<\\|>\\|<=\\|>=\\|=\\|!=\\|:\\)[ A-Za-z0-9'\"]" (1 font-lock-builtin-face))
   '("#import +\\([A-Za-z0-9._-]+\\)" (1 font-lock-constant-face))
   ;'("\\(bar\\|car\\)x" . font-lock-variable-name-face)
  )
  "adp keywords")


(defvar adp-font-lock-keywords adp-font-lock-keywords-1
  "default highlighting level vor adp")

(defvar adp-mode-syntax-table
   (let ((table (make-syntax-table)))
     ; _ is part of a word
     (modify-syntax-entry ?_ "w" table )
     ; ' is part of a word
     (modify-syntax-entry ?' "w" table )
     ; bracket handling '$' is opening '^' is closing
     ; () {} [] are included in default syntax-table
     ;(modify-syntax-entry ?$ "(^" table)
     ;(modify-syntax-entry ?^ ")$" table)

     ; C++ style comments - here '--' is the comment style
     (modify-syntax-entry ?- ". 124b" table)
     (modify-syntax-entry ?\n "> b" table)
     ; single quoted 'strings' - actually chars - how to do it with emacs?
     ; NT with suffix ' have to work too
     ;( modify-syntax-entry ?' "\"" table)
   table)
   "Syntax table for adp mode")

(define-derived-mode adp-mode fundamental-mode "ADP"
  "ADP-Mode for stupid emacs."
  (set (make-local-variable 'font-lock-defaults) '(adp-font-lock-keywords))

  ; TODO
  ; (set (make-local-variable 'indent-line-function) 'adp-indent-line)
  
  ; from haskell-mode - does not work
  ;(set (make-local-variable 'comment-start) "-- ")
  ;(set (make-local-variable 'comment-end) "")
  ;(set (make-local-variable 'comment-start-skip) "[-{]-[ \t]*")
  ;(set (make-local-variable 'comment-end-skip) "[ \t]*\\(-}\\|\\s>\\)")

  ; because of offside rule - from haskell-mode
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'tab-width) 8)

)

(provide 'adp-mode)
