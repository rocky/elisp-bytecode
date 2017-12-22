;; This de-compiler is used for inline expansion of compiled functions,
;; and by the disassembler.
;;
;; This list contains numbers, which are pc values,
;; before each instruction.
(defun byte-pretty-decompile-bytecode (bytes constvec)
  "Turn BYTECODE into lapcode, referring to CONSTVEC."
  (let ((byte-compile-constants nil)
	(byte-compile-variables nil)
	(byte-compile-tag-number 0))
    (byte-pretty-decompile-bytecode-1 bytes constvec nil t)))

;; As byte-decompile-bytecode, but updates
;; byte-compile-{constants, variables, tag-number}.
;; If MAKE-SPLICEABLE is true, then `return' opcodes are replaced
;; with `goto's destined for the end of the code.
;; That is for use by the compiler.
;; If MAKE-SPLICEABLE is nil, we are being called for the disassembler.
;; In that case, we put a pc value into the list
;; before each insn (or its label).
(defun byte-pretty-decompile-bytecode-1 (bytes constvec &optional make-spliceable verbatim)
  (let ((length (length bytes))
        (bytedecomp-ptr 0) optr tags bytedecomp-op offset
	lap tmp last-constant)
    (while (not (= bytedecomp-ptr length))
      (or make-spliceable
	  (push bytedecomp-ptr lap))
      (setq bytedecomp-op (aref bytes bytedecomp-ptr)
	    optr bytedecomp-ptr
            ;; This uses dynamic-scope magic.
            offset (disassemble-offset bytes))
      (let ((opcode (aref byte-code-vector bytedecomp-op)))
	(cl-assert opcode)
	(setq bytedecomp-op opcode))
      (cond ((memq bytedecomp-op byte-goto-ops)
	     ;; It's a pc.
	     (setq offset
		   (cdr (or (assq offset tags)
                            (let ((new (cons offset (byte-compile-make-tag))))
                              (push new tags)
                              new)))))
	    ((cond ((eq bytedecomp-op 'byte-constant2)
		    (unless verbatim (setq bytedecomp-op 'byte-constant)) t)
		   ((memq bytedecomp-op byte-constref-ops)))
	     (setq tmp (if (>= offset (length constvec))
			   (list 'out-of-range offset)
			 (aref constvec offset))
		   offset (if (eq bytedecomp-op 'byte-constant)
			      (byte-compile-get-constant tmp)
			    (or (assq tmp byte-compile-variables)
                                (let ((new (list tmp)))
                                  (push new byte-compile-variables)
                                  new)))
                   last-constant tmp))
	    ((eq bytedecomp-op 'byte-stack-set2)
	     (unless verbatim (setq bytedecomp-op 'byte-stack-set)))
	    ((and (eq bytedecomp-op 'byte-discardN) (>= offset #x80))
	     ;; The top bit of the operand for byte-discardN is a flag,
	     ;; saying whether the top-of-stack is preserved.  In
	     ;; lapcode, we represent this by using a different opcode
	     ;; (with the flag removed from the operand).
	     (setq bytedecomp-op 'byte-discardN-preserve-tos)
	     (setq offset (- offset #x80)))
            ((eq bytedecomp-op 'byte-switch)
             (cl-assert (hash-table-p last-constant) nil
                        "byte-switch used without preceeding hash table")
             ;; We cannot use the original hash table referenced in the op,
             ;; so we create a copy of it, and replace the addresses with
             ;; TAGs.
             (let ((orig-table last-constant))
               (cl-loop for e across constvec
                        when (eq e last-constant)
                        do (setq last-constant (copy-hash-table e))
                        and return nil)
               ;; Replace all addresses with TAGs.
               (maphash #'(lambda (value tag)
                            (let (newtag)
                              (setq newtag (byte-compile-make-tag))
                              (push (cons tag newtag) tags)
                              (puthash value newtag last-constant)))
                        last-constant)
               ;; Replace the hash table referenced in the lapcode with our
               ;; modified one.
               (cl-loop for el in-ref lap
                        when (and (listp el) ;; make sure we're at the correct op
                                  (eq (nth 1 el) 'byte-constant)
                                  (eq (nth 2 el) orig-table))
                        ;; Jump tables are never reused, so do this exactly
                        ;; once.
                        do (setf (nth 2 el) last-constant) and return nil))))
      ;; lap = ( [ (pc . (op . arg)) ]* )
      (push (cons optr (cons bytedecomp-op offset))
            lap)
      (setq bytedecomp-ptr (1+ bytedecomp-ptr)))
    (let ((rest lap))
      (while rest
	(cond ((numberp (car rest)))
	      ((setq tmp (assq (car (car rest)) tags))
	       ;; This addr is jumped to.
	       (setcdr rest (cons (cons nil (cdr tmp))
				  (cdr rest)))
	       (setq tags (delq tmp tags))
	       (setq rest (cdr rest))))
	(setq rest (cdr rest))))
    (if tags (error "optimizer error: missed tags %s" tags))
    ;; Remove addrs, lap = ( [ (op . arg) | (TAG tagno) ]* )
    (mapcar (function (lambda (elt)
			(if (numberp elt)
			    elt
			  (cdr elt))))
	    (nreverse lap))))


(defun byte--pretty-bytes (bytes)
  (mapconcat (lambda (x) (format "%3d" x)) bytes " "))

(defun byte-pretty-compile-decompile (form)
  "Compile FORM, then disassemble it, producing a string listing
bytecode and LAP-code side-by-side."
  (let* ((v (byte-compile form))
         (constvec (aref v 2))
         (bytes (aref v 1))
         (bytecode (byte-pretty-decompile-bytecode bytes constvec))
         (rbc (reverse bytecode))
         (pc (length bytes))
         (str ""))
    (while (> pc 0)
      (if (eq (caar rbc) 'TAG)
          (setq rbc (cdr rbc))
        (let* ((op (car rbc))
               (npc (cadr rbc))
               (lstr ""))
          (while (eq (car-safe npc) 'TAG)
            (setq rbc (cdr rbc))
            (setq lstr (concat (format "%S:\n" npc) lstr))
            (setq npc (cadr rbc)))
          (while (< (1+ npc) pc)
            (setq str (concat "      "
                              (byte--pretty-bytes (substring bytes (1- pc) pc))
                              "\n"
                              str))
            (setq pc (1- pc)))
          (setq str (concat lstr
                            (format "%5d " npc)
                            (byte--pretty-bytes (substring bytes npc (1+ npc)))
                            " "
                            (format "%S\n" op)
                            str))
          (setq rbc (if (eq (car-safe op) 'TAG) (cdr rbc) (cddr rbc)))
          (setq pc npc))))
    str))

(defun byte-pretty-compile-decompile-texinfo (form)
  "Compile FORM, then disassemble it, producing output suitable
for texinfo input."
  (let* ((byte-optimize nil)
         (v (byte-compile form))
         (constvec (aref v 2))
         (bytes (aref v 1))
         (bytecode (byte-pretty-decompile-bytecode bytes constvec))
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
      (if (eq (caar rbc) 'TAG)
          (setq rbc (cdr rbc))
        (let* ((op (car rbc))
               (npc (cadr rbc))
               (lstr ""))
          (while (eq (car-safe npc) 'TAG)
            (setq rbc (cdr rbc))
            (setq lstr (concat (format "%S:\n" npc) lstr))
            (setq npc (cadr rbc)))
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
                            (substring (format "%S" (car op)) 5)
                            (cond
                             ((consp (cdr op))
                              (concat " " (mapconcat #'prin1-to-string (cdr op) " ")))
                             ((cdr op)
                              (concat " . " (format "%S" (cdr op))))
                             (t
                              ""))
                            "\n"
                            str))
          (setq rbc (if (eq (car-safe op) 'TAG) (cdr rbc) (cddr rbc)))
          (setq pc npc))))
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

(defun byte-recalc-examples ()
  "Recalculate the examples in elisp-bytecode.texi"
  (interactive)
  (goto-char (point-min))
  (while (search-forward-regexp "@code{\\([^}]*\\)} generates:$" nil t)
    (let* ((lexical (save-excursion
                      (beginning-of-line)
                      (while (not (looking-at-p ".*@code"))
                        (forward-line -1))
                      (looking-at-p ".*lexical")))
           (code (read (match-string 1)))
           (form (cond ((eq (car-safe code) 'defun)
                        (eval code lexical))
                       (t
                        `(lambda () ,code))))
           (comments nil))
      (forward-char 1)
      (when (looking-at-p "^@verbatim$")
        (let* ((p0 (point))
               (p1 (search-forward-regexp "@end verbatim\n*")))
          (setq comments (byte-collect-comments p0 p1))
          (delete-region p0 p1)))
      (let ((p0 (point)))
        (insert (byte-pretty-compile-decompile-texinfo form))
        (byte-insert-comments p0 (point) comments)))
    (when (not (looking-at-p "\n\n"))
      (insert "\n"))))
