;; This code is the same as byte-pretty.el but for Emacs 22

(require 'cl)

(assert (= emacs-major-version 22))

;; Functions from later versions of Emacs for Emacs 22
(defun alist-get (key alist &optional default remove)
  "Return the value associated with KEY in ALIST, using `assq'.
If KEY is not found in ALIST, return DEFAULT.

This is a generalized variable suitable for use with `setf'.
When using it to set a value, optional argument REMOVE non-nil
means to remove KEY from ALIST if the new value is `eql' to DEFAULT."
  (ignore remove) ;;Silence byte-compiler.
  (let ((x (assq key alist)))
    (if x (cdr x) default)))

(defvar use-empty-active-region nil
  "Whether \"region-aware\" commands should act on empty regions.
If nil, region-aware commands treat the empty region as inactive.
If non-nil, region-aware commands treat the region as active as
long as the mark is active, even if the region is empty.

Region-aware commands are those that act on the region if it is
active and Transient Mark mode is enabled, and on the text near
point otherwise.")

(defun use-region-p ()
  "Return t if the region is active and it is appropriate to act on it.
This is used by commands that act specially on the region under
Transient Mark mode.

The return value is t if Transient Mark mode is enabled and the
mark is active; furthermore, if `use-empty-active-region' is nil,
the region must not be empty.  Otherwise, the return value is nil.

For some commands, it may be appropriate to ignore the value of
`use-empty-active-region'; in that case, use `region-active-p'."
  (and (region-active-p)
       (or use-empty-active-region (> (region-end) (region-beginning)))))

(defun region-active-p ()
  "Return non-nil if Transient Mark mode is enabled and the mark is active.

Some commands act specially on the region when Transient Mark
mode is enabled.  Usually, such commands should use
`use-region-p' instead of this function, because `use-region-p'
also checks the value of `use-empty-active-region'."
  (and transient-mark-mode mark-active
       ;; FIXME: Somehow we sometimes end up with mark-active non-nil but
       ;; without the mark being set (e.g. bug#17324).  We really should fix
       ;; that problem, but in the mean time, let's make sure we don't say the
       ;; region is active when there's no mark.
       (progn (assert (mark)) t)))

;;;;;;;
;; Byte pretty starts here...

(defun byte-pretty-op-arg-len (bytes off)
  (let* ((opcode (aref bytes off))
         (arg nil)
         (len 1))
    (cond ((< opcode byte-nth)
           (let ((tem (logand opcode 7)))
             (setq opcode (- opcode tem))
             (cond ((= tem 6)
                    (list opcode 0 2))
                   ((= tem 7)
                    (list opcode 0 3))
                   (t
                    (list opcode tem 1)))))
          ((>= opcode byte-constant)
           (list byte-constant (- opcode byte-constant) 1))
          ((and (>= opcode byte-constant2)
                    (<= opcode byte-goto-if-not-nil-else-pop))
           (list opcode nil 3))
          ((and (>= opcode byte-listN)
                (<= opcode byte-discardN))
           (list opcode nil 2))
          (t (list opcode nil 1)))))

(defun byte-pretty-name-arg-len (bytes off)
  (let* ((tem (byte-pretty-op-arg-len bytes off))
         (opcode (car tem))
         (arg (cadr tem))
         (len (caddr tem))
         (type (cond ((memq opcode (mapcar #'symbol-value byte-goto-ops)) 'pc)
                     ((memq opcode (mapcar #'symbol-value byte-constref-ops)) 'cv)
                     (t 'stack-or-count)))
         (name (substring (symbol-name (aref byte-code-vector opcode)) 5)))
    (and arg (setq name (concat name "[" (format "%S" arg) "]")))
    (cond ((= len 3)
           (setq arg (+ (aref bytes (1+ off))
                        (lsh (aref bytes (+ 2 off)) 8)))
           (setq name (concat name " " (format "[%S]" arg))))
          ((= len 2)
           (setq arg (aref bytes (1+ off)))
           (setq name (concat name " " (format "[%S]" arg))))
          (t t))
    (list name (cons type arg) len)))

(defun byte-pretty-arg (arg constvec)
  (cond ((eq (car arg) 'cv)
         (format " %S" (aref constvec (cdr arg))))
        (t "")))

(defun byte-pretty-disassemble (bytes &optional constvec)
  (let ((beg 0)
        (end (length bytes))
        res)
    (while (< beg end)
      (let* ((tem (byte-pretty-name-arg-len bytes beg))
             (name (car tem))
             (arg (byte-pretty-arg (cadr tem) constvec))
             (len (caddr tem)))
        (push (cons beg (cons name arg)) res)
        (setq beg (+ beg len))))
    (nreverse res)))

(defun byte--pretty-bytes (bytes)
  (mapconcat (lambda (x) (format "%3d" x)) bytes " "))

(defun byte-pretty-compile-decompile-texinfo (form &optional optimize)
  "Compile FORM, then disassemble it, producing output suitable
for texinfo input."
  (let* ((byte-optimize optimize)
         (v (byte-compile form))
         (constvec (aref v 2))
         (bytes (aref v 1))
         (bytecode (byte-pretty-disassemble bytes constvec))
         (rbc (reverse bytecode))
         (pc (length bytes))
         (str "@end verbatim\n")
	 (width (max 2 (ceiling (log pc 10))))
	 (pc-width (format "%%%dd  " width))
	 (str-width (format "%%%ds " width))
	 )
    (if (> (length constvec) 0)
        (setq str (concat (format "\nConstants Vector: %S\n" constvec) str)))
    (while (> pc 0)
      (let* ((op (cdar rbc))
             (npc (caar rbc))
             (lstr ""))
        (while (< (1+ npc) pc)
          (setq str (concat "         "
                            (byte--pretty-bytes (substring bytes (1- pc) pc))
                            "\n"
                            str))
          (setq pc (1- pc)))
        (setq str (concat lstr
                          (format pc-width npc)
                          (byte--pretty-bytes (substring bytes npc (1+ npc)))
                          "   "
                          (car op)
                          (cdr op)
                          "\n"
                          str))
          (setq rbc (cdr rbc))
          (setq pc npc)))
    (setq str (format "@verbatim\n%s Byte  Instruction\n%s"
		      (format str-width "PC") str))
    str))

(defun byte-collect-comments (beg end)
  (let ((res nil))
    (goto-char beg)
    (while (search-forward-regexp "^ *\\([0-9]+\\).*?\\( *;;.*\\)$" end t)
      (push (cons (read (match-string 1)) (match-string 2))
            res))
    (goto-char end)
    res))


(defun byte-insert-comments (beg end comments)
  (setq end (copy-marker end))
  (goto-char beg)
  (while (search-forward-regexp "^ *\\([0-9]+\\) +[0-9].*$" end t)
    (let ((comment (alist-get (read (match-string 1)) comments)))
      (when comment
        (insert comment))))
  (goto-char end))

(defun byte-recalc-examples (beg end)
  "Recalculate the examples in elisp-bytecode.texi"
  (interactive "r")
  (save-excursion
    (unless (use-region-p)
      (setq beg (point-min))
      (setq end (point-max)))
    (goto-char beg)
    (while (search-forward-regexp "@code{\\([^}]*\\)} generates:$" end t)
      (let* ((code (read (match-string 1)))
             (form nil)
             (warnings t)
             (comments nil)
             (alist nil)
             (lexical nil)
             (optimize t))
        (forward-char 1)
        (when (looking-at "^@c \\((.*)\\)$")
          (setq alist (read (match-string 1)))
          (setq lexical (alist-get 'lexical alist lexical))
          (setq optimize (alist-get 'optimize alist optimize))
          (setq warnings (alist-get 'warnings alist warnings))
          (forward-line 1))
        (setq form (cond ((eq (car-safe code) 'defun)
                          (with-no-warnings (eval code)))
                         (t
                          `(lambda () ,code))))
        (let ((p0 (point)))
          (when (looking-at "^@verbatim$")
            (let ((p1 (search-forward-regexp "@end verbatim\n*")))
              (setq comments (byte-collect-comments p0 p1))
              (delete-region p0 p1))))
        (let* ((p0 (point))
               (byte-compile-warnings nil)
               (pretty (with-no-warnings (byte-pretty-compile-decompile-texinfo form optimize))))
          (insert pretty)
          (byte-insert-comments p0 (point) comments)))
      (when (not (looking-at "\n\n"))
        (insert "\n")))))
