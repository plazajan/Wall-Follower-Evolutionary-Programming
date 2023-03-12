;; main.lsp
;; Evolutionary programming in LISP 
;; educational software inspired by Nils. J. Nilsson
;; March 25, 2000
;; https://github.com/plazajan
;; (c) 2000 Jan A. Plaza

;; This file is part of LISP-Evolutionary-Programming-Wall-Follower. 
;; LISP-Evolutionary-Programming-Wall-Follower is free software: 
;; you can redistribute it and/or modify it under the terms of
;; the GNU General Public License as published by the Free Software Foundation, 
;; either version 3 of the License, or (at your option) any later version. 
;; LISP-Evolutionary-Programming-Wall-Follower is distributed in the hope 
;; that it will be useful, but WITHOUT ANY WARRANTY; 
;; without even the implied warranty of MERCHANTABILITY 
;; or FITNESS FOR A PARTICULAR PURPOSE. 
;; See the GNU General Public License for more details. 
;; You should have received a copy of the GNU General Public License 
;; along with LISP-Evolutionary-Programming-Wall-Follower. 
;; If not, see https://www.gnu.org/licenses/.


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
