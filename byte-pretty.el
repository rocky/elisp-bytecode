(defun byte--pretty-bytes (bytes)
  (mapconcat (lambda (x) (format "%3d" x)) bytes " "))

(defun byte-pretty-compile-decompile (form)
  "Compile FORM, then disassemble it, producing a string listing
bytecode and LAP-code side-by-side."
  (let* ((v (byte-compile form))
         (constvec (aref v 2))
         (bytes (aref v 1))
         (bytecode (byte-decompile-bytecode bytes constvec))
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
         (bytecode (byte-decompile-bytecode bytes constvec))
         (rbc (reverse bytecode))
         (pc (length bytes))
         (str "@end verbatim\n")
	 (pc-width (format "%%%dd " (ceiling (log pc 10))))
	 )
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
                            (format pc-width npc)
                            (byte--pretty-bytes (substring bytes npc (1+ npc)))
                            " "
                            (format "%S\n" op)
                            str))
          (setq rbc (if (eq (car-safe op) 'TAG) (cdr rbc) (cddr rbc)))
          (setq pc npc))))
    (setq str (concat "@verbatim\n"
                      "PC byte Instruction\n"
                      str))
    str))
