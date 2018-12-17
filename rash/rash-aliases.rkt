
#lang rash

(provide (all-defined-out))



(require racket/string
         (only-in racket curryr))
;; ls's
(define-simple-pipeline-alias ls 'ls '-CFh '--color=auto)
(define-simple-pipeline-alias la ls '-a)
(define-simple-pipeline-alias ll ls '-l)
(define-simple-pipeline-alias lla ll '-a)

;; greps
(define-simple-pipeline-alias grep 'grep '--color=auto)
(define-simple-pipeline-alias fgrep 'fgrep '--color=auto)
(define-simple-pipeline-alias egrep 'egrep '--color=auto)

;; mv's
(define-simple-pipeline-alias mv 'mv '-i)

;; emacs
(define-simple-pipeline-alias em
  (map (curryr string-trim "\"\"") (string-split (getenv "EMACS_TERM_CLIENT"))))
(define-simple-pipeline-alias emacs
  (map (curryr string-trim "\"\"") (string-split (getenv "EMACS_CLIENT"))))

;; git
(define-simple-pipeline-alias gc 'git 'commit)
(define-simple-pipeline-alias gs 'git 'status)
(define-simple-pipeline-alias gd 'git 'diff)
(define-simple-pipeline-alias ga 'git 'add)

