;; world.lsp -- Prepare and display grid world.
;; Evolutionary programming in LISP 
;; educational materials inspired by Nils. J. Nilsson
;; March 17, 2000
;; https://github.com/plazajan
;; (c) 2000 Jan A. Plaza
;; Use under Creative Commons Attribution Share-Alike International License 4.0

;;=============================================================================

;; Grid world will be read from a file.
;; The file is assumed to contain an outline of a grid world
;; made of space, # and newline characters (other characters are illegal).
;; (For efficiency reasons do not put extra spaces at the end of a line.)
;; The information will be stored in a two-dimensional array.
;; The values in the array can be thought of as answers to the question
;; "Is the cell free?"
;; If cell is NOT free, the value nil will be stored;
;; if cell is free and next to a wall, the character + will be stored;
;; if cell is free but far from walls, value T will be stored.

;;=============================================================================

;; Global variables
;; *world* is a two-dimensional array representing the grid world.
;; It is initialized in main.lsp using the file specified in *grid-world-file*.
;; Once these variables are initialized, their values do not change.
;; (We do not make them constants to allow the user working in the
;; interactive lisp environment to change *grid-world-file*

;; If a cell is NOT free, the value nil will be stored;
;; if a cell is free and next to a wall, the character - will be stored;
;; if a cell is free but far from walls, value T will be stored.

(defvar *world*) 
(defvar *max-rows*)
(defvar *max-columns*)
(defvar *at-the-wall*)

;;=============================================================================

;; This function displays the array representing grid world. 
;; It does NOT clear the screen before. 
;; Prerequisite: the cursor must be at the beginning of a line
(defun display-world (world &optional (stream t)) 
  (dotimes (row (array-dimension world 0) nil)
    (dotimes (column (array-dimension world 1) nil)
      (if (aref world row column) ;; If place is free ... 
        (write-char #\  stream)   ;; then write space
        (write-char #\# stream)   ;; else write #
      )
    )
    (terpri stream) ;; newline
  )
)

;;-----------------------------------------------------------------------------

;; Prepare and display world.

(defun prepare-world (filename)
  (let* 
    ;; local variables
    ((dimensions (find-dimensions filename))
     (rows (first dimensions))
     (columns (second dimensions))
     (world (make-array `(,rows ,columns) :initial-element nil))
    )
    ;; body
    (initialize-from-file filename world rows columns)
    (add-outside-walls world rows columns)
    (mark-at-the-wall-positions world rows columns)
    world
  )
)

;;-----------------------------------------------------------------------------

(defun find-dimensions (filename)
  (let 
    (c ;; Character
     (new-row T)
     (row 0)
     (column 0) 
     (max-column 0)
    )
    (with-open-file (world-stream filename :direction :input)
      (loop
        (setq c (read-char world-stream nil))
        (unless c (return `(,row ,max-column)))
        (case (char-code c) 
          (13 nil) ;; skip CR character (CRLF used in DOS)
          (10  ;; handling the newline character LF 
            (setq new-row T)
            (setq column 0)
          )
          (otherwise
            (when new-row (setq new-row nil) (incf row))
            (incf column)
            (when (> column max-column) (setq max-column column))
          )
        )
      )
    )
  )
)

;; ----------------------------------------------------------------------------

;; This function will change the contents of array world
;; (the function will return nil.
(defun initialize-from-file (filename world rows columns)
  (with-open-file (world-stream filename :direction :input)
    (do*
      ;;loop parameters
      (
        (c (read-char world-stream nil) ;; initial value
           (read-char world-stream nil) ;; update form
        )
        (row 0)    ;; row 0 in array corresponds to row 1 on screen.
        (column 0) ;; column 0 in array corresponds to column 1 on screen.
      )
      ;; loop termination test and result form
      ((not c) nil)
      ;; loop body
      (cond 
        ((>= row rows) nil) ;; skip a character in such a position
        ((= (char-code c) 13) nil) ;; skip CR character (CRLF is used in DOS)
        ((= (char-code c) 10)  ;; handling the newline character LF 
          (incf row) 
          (setq column 0)
        )
        ((>= column columns) nil) ;;skip a character in such a position
        ((eq c #\#) ;; # character.
          (setf (aref world row column) nil) ;; nil means not free. 
          (incf column)
        )
        ((eq c #\ ) ;; Space character.
          (setf (aref world row column) T) ;; T means free.
          (incf column)
        )
        (T 
          (format t
            "Invalid character in ~a at row ~a and column ~a."
            filename
            (1+ row)
            (1+ column)
          )          
          (incf column)
        )
      )
    )
  )
)

;; ----------------------------------------------------------------------------

;; Frame world with nil's.
;; This function will change the contents of array world
;; (the function will return nil.
(defun add-outside-walls (world rows columns)
  (dotimes (row rows nil)     
    (setf (aref world row 0) nil)
    (setf (aref world row (1- columns)) nil)
  )
  (dotimes (column columns nil)
    (setf (aref world 0 column) nil)
    (setf (aref world (1- rows) column) nil)
  )
)

;; ----------------------------------------------------------------------------

;; Mark places next to walls and obstacles with -'s.
;; Calculate how many such places.
;; Must be called after add-outside-walls.
;; This function will change the contents of array world
;; The function will return nil.
(defun mark-at-the-wall-positions (world rows columns)
  (dotimes (row rows nil)
    (dotimes (column columns nil)
      (when 
        (and 
          (aref world row column) ;; when at a free position
          (not                    ;; not
            (and                  ;; all surrounding positions are free ... 
              (aref world row      (1+ column))
              (aref world (1+ row) (1+ column))
              (aref world (1+ row) column)     
              (aref world (1+ row) (1- column))
              (aref world row      (1- column))
              (aref world (1- row) (1- column))
              (aref world (1- row) column)     
              (aref world (1- row) (1+ column))
            )
          )
        )
        (setf (aref world row column) #\-)
      )
    )
  )
)

;;=============================================================================

(defun count-at-the-wall-places (world rows columns)
  (let ((count 0))
    ;; let's body
    (dotimes (row rows count) ;; return count
      (dotimes (column columns count) ;; return count
        (when (eq (aref world row column) #\-) (incf count))
      )
    )
  )
)

;;-----------------------------------------------------------------------------

;; This function returns two lists with row and column coordinates of places
;; which are at the wall.

(defun list-at-the-wall-places (world rows columns)
  (let 
    ((row-coordinates nil) 
     (column-coordinates nil)
    )
    (dotimes (row rows nil)
      (dotimes (column columns nil) ;; return count
        (when (eq (aref world row column) #\-) 
          (push row row-coordinates)
          (push column column-coordinates)
        )
      )
    )
    (list row-coordinates column-coordinates)
  )
)

;;-----------------------------------------------------------------------------

;; In one evaluation method, an agent marks visited places at the wall
;; by a #\+. This function counts these +'s and changes them back to -'s.
;; (It is assumed that a + may occur only at the wall.)
;; This function is called after every tour by evaluate-c -- improve 
;; efficiency by looking only at the places at the wall (from a list).

(defun count-erase-visited-places (world rows columns)
  (let* 
    ((count 0)
    )
    ;; let's body
    (dotimes (row rows count) ;; return count
      (dotimes (column columns count) ;; return count
        (when (eq (aref world row column) #\+)
          (setf (aref world row column) #\-) 
          (incf count)
        )
      )
    )
  )
)

;;=============================================================================
