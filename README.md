# LISP-Evolutionary-Programming-Wall-Following

;; Evolutionary programming in LISP
;; educational materials inspired by Nils. J. Nilsson
;; March 16, 2000
;; https://github.com/plazajan
;; (c) 2000 Jan A. Plaza
;; Use under Creative Commons Attribution Share-Alike International License 4.0

-------------------------------------------------------------------------------

Copy this entire directory to your account.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

If on a UNIX/Linux system with GNU Common LISP (clisp) installed,
in a shell, change to this directory and type type at the shell prompt:

	start

then read carefully and follow instructions on the screen.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

You can also start the program from any Common LISP listener:

     > (load "main")

-------------------------------------------------------------------------------

The program is inspired by the discussion of evolutionary programming in
Nils J. Nilsson, Artificial Inteligence: A New Synthesis.

The program provides options for setting up different evolutionary
programming experiments and observing the pace of evolution.

-------------------------------------------------------------------------------

An agent in a grid world senses its immediate surroundings (in 8 directions)
but has no memory of the past,
and performs a step in one of the 4 cardinal directions.
Goal: evolve an agent which finds a wall and continues moving
following the wall.

More informtion in README.md and in messages the program prints while runing.

===============================================================================
