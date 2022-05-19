;; utilities.lsp 
;; Evolutionary programming in LISP
;; educational materials inspired by Nils. J. Nilsson
;; March 17, 2000
;; https://github.com/plazajan
;; (c) 2000 Jan A. Plaza
;; Use under Creative Commons Attribution Share-Alike International License 4.0


;(unless 
;  (find-package 'utilities) 
;  (make-package 'utilities)
;)
;(in-package 'utilities)
;(export
; '(
;    e
;    l
;    compile-and-load
;    clear-screen
;    move-cursor
;    tee-2
;    tee-n
;    time-string
;    delay
;  )
;)

;;=============================================================================

;; Utilities to aid the edit-load cycle of program writing.
;; Calls: (e) or (e a) ...
;; The argument is the first character of filename.
;; [Improve: take cursor to given line.]
;; Note: some Lisp's use "system" insted of "execute".

(defmacro e (&optional (name 'user::p)) 
 `(progn
    (case (quote ,name)
      ('user::a (execute "/usr/bin/nano" "agents.lsp"))
      ('user::d (execute "/usr/bin/nano" "log.dat"))
      ('user::e (execute "/usr/bin/nano" "evaluate.lsp"))
      ('user::g (execute "/usr/bin/nano" "genetic.lsp"))
      ('user::i (execute "/usr/bin/nano" "interface.lsp"))
      ('user::m (execute "/usr/bin/nano" "main.lsp"))
      ('user::o (execute "/usr/bin/nano" "output.lsp"))
      ('user::p (execute "/usr/bin/nano" "parameters.lsp"))
      ('user::r (execute "/usr/bin/nano" "readme.txt"))
      ('user::t (execute "/usr/bin/nano" "log.txt"))
      ('user::u (execute "/usr/bin/nano" "utilities.lsp"))
      ('user::w (execute "/usr/bin/nano" "world.lsp"))
    )
    (l)
  )
)

;;-----------------------------------------------------------------------------

(defun l ()
  (load "main.lsp")
)

;;-----------------------------------------------------------------------------

;; compile-and-load <filename>
;; Preconditions:
;; <filename> is a path to file with no extension (no .lsp, no .o).
;; File <filename>.lsp exists.
;; Postcondition:
;; If both .lsp and .o exist, the .lsp will be recompiled if it is newer.
;; If only .lsp exits, it will be compiled.
;; Then, if .o exists it will be loaded, otherwise .lsp will be loaded.

(defun compile-and-load (filename)
  (unless
    (and
      (simple-string-p filename)
      (or
        (<= (length filename) 5)
        (string-not-equal filename ".lsp" :start1 (- (length filename) 4))
      )
    )
    (error "Wrong parameter in (compile-and-load filename=~a)." filename)
  )
  (let*
    ((source-file (format nil "~a.lsp" filename))
     (object-file (format nil "~a.o"   filename))
     (source-exists (probe-file source-file))
     (object-exists (probe-file object-file))
    )
    (when 
      (and 
        source-exists 
        object-exists
        (< (file-write-date object-file) (file-write-date source-file))
      )
      (compile-file source-file)
    )
    (when 
      (and 
        source-exists
        (not object-exists)
      )
      (compile-file source-file)
    )
    (load filename)
  )
)

;;=============================================================================

;; clear-screen
;; Will work only on vt100 compatible terminals.

(defun clear-screen ()
  (write-char (int-char 27)) ;; ESC
  (write-char #\[ )
  (write-char #\2)
  (write-char #\J)
  nil
)

;;-----------------------------------------------------------------------------

;; move-cursor <row> <column>
;; <row> is an integer 1..width; typical width is 80.
;; <column> is an integer 1..length; typical length is 24.
;; move-cursor assumes standard numbering in which left top position is 1,1.
;; Will work only on vt100 compatible terminals.

(defun move-cursor (row column)
  (unless
    (and (integerp row) (integerp column) (> row 0) (> column 0)) 
    (error "Wrong parameters in (move-cursor row=~a, column=~a.)" row column
    )
  )
  (write-char (int-char 27)) ;; ESC
  (write-char #\[ )
  (write row) 
  (write-char #\;) 
  (write column)
  (write-char #\f)
)

;;-----------------------------------------------------------------------------

;; (tee-n <printing-form>)
;; Precondition: <out-stream> is the last item in <printing-form>
;; Postcondition: Will print both on the screen and to the out-stream.
;; Example: (tee-n (write-char c <out-stream>));

(defmacro tee-n (printing-form)
  (list
    'progn
    (append (butlast printing-form) '(t))
    printing-form
  )
)

;;-----------------------------------------------------------------------------

;; (tee-2 <printing-form>)
;; Precondition: <out-stream> is the second item in <printing-form>
;; Postcondition: Will print both on the screen and to the out-stream.
;; Example: (tee-2 (format <out-stream> <string> <args>))

(defmacro tee-2 (printing-form)
  (list
    'progn
    (cons (car printing-form) (cons 't (cddr printing-form)))
    printing-form
  )
)

;;-----------------------------------------------------------------------------

;; time-string
;; Returns a string containing current day and time: "yyyy-mm-dd hh:mm:ss".

(defun time-string ()
  (multiple-value-bind
    (second minute hour day month year)
    (decode-universal-time (get-universal-time))    
    (format nil "~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" 
      year month day hour minute second
    )
  )
)

;;-----------------------------------------------------------------------------

;; delay <seconds>
;; <seconds> can be any non-negative number (float or integer or rational)
;; Returns after <seconds>.
;; Note: Lisp built-in (sleep <seconds>) is accurate only up to 1 second.

(defun delay (seconds)
  (unless
    (and (numberp seconds) (>= seconds 0))
    (error "Wrong parameter in (delay seconds=~a)." seconds)
  )
  (let 
    ((start (get-internal-real-time)))
    (loop
      (when 
        (< 
          (* seconds internal-time-units-per-second)
          (- (get-internal-real-time) start)
        )
        (return)       
      )
    )
  )
)

;;=============================================================================
