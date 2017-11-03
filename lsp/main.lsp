#!/usr/bin/env newlisp

(define cmd-args (1 (main-args)))

(define (main)
  (cond ((find "-c" cmd-args)
         (load "connect.lsp")
         (con:main))
        (true (exit 1))))
