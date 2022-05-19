;; main.lsp
;; Evolutionary programming in LISP 
;; educational materials inspired by Nils. J. Nilsson
;; March 25, 2000
;; https://github.com/plazajan
;; (c) 2000 Jan A. Plaza
;; Use under Creative Commons Attribution Share-Alike International License 4.0


;; To run this program invoke lisp and type: (load "main")

;;=============================================================================

(load "utilities.lsp") ;; Edit, compile, time, screen, file output.

(compile-and-load "parameters") ;; User modifiable parameters.
(compile-and-load "world")      ;; Prepare and display grid world.
(compile-and-load "evaluate")   ;; Evaluate agents.
(compile-and-load "agents")     ;; Boolean conditions, trees, agents.
(compile-and-load "genetic")    ;; Genetic/evolutionary process.
(compile-and-load "output")     ;; Output functions.
(compile-and-load "interface")  ;; Trace changes of parameters, start, restart.

;;=============================================================================

;; Print introductory messages when this file is loaded.

;(clear-screen)

(print-introduction)

(format t
"

  To change evolutionary process parameters, edit parameters.lsp - type: (e p)
  To start the evolution, type: (start)

"
)

;;=============================================================================
