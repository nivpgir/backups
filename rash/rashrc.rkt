
#lang rash

(require
 racket/list
 racket/file
 racket/format
 shell/utils/bourne-expansion-utils
 json
 racket/string
 (for-syntax racket/syntax
             syntax/parse
             racket/base
             ))

(provide (all-defined-out)
         (only-in racket curry curryr)
         (all-from-out racket/list
                       racket/file
                       racket/format
                       shell/utils/bourne-expansion-utils
                       json
                       racket/string))




(define-syntax (require-if-file-exists stx)
  (syntax-parse stx
    [(_ path-stx)
     (define path-datum (syntax->datum #'path-stx))
     (define source-file (syntax-source #'path-stx))
     (define (path-dirname p)
       (apply build-path (reverse (cdr (reverse (explode-path p))))))
     (define full-path (if (absolute-path? path-datum)
                           path-datum
                           (build-path (path-dirname source-file) path-datum)))
     (define full-path-string (if (string? full-path)
                                  full-path
                                  (path->string full-path)))
     (define (s x)
       (datum->syntax #'path-stx x))
     (if (file-exists? full-path)
         ;; The syntax context of the provided identifiers is determined by the
         ;; syntax on the outermost bit of syntax to the right of require,
         ;; which may be a parenthesis.
         ;(datum->syntax stx `(require (file ,full-path-string)))
         #`(require #,(datum->syntax #'path-stx (list #'file full-path-string)))
         #`(void))]))




(define-line-macro in-dir
  (syntax-parser
    [(_ dirs:id body)
     #`(in-dir (values #,(dollar-expand-syntax #'dirs #:glob-expand? #t)) body)]
    [(_ dirs:str body)
     #`(in-dir (values #,(dollar-expand-syntax #'dirs #:glob-expand? #t)) body)]
    [(_ dirs body)
     #`(let* ([edirs dirs]
              [err (λ (p) (error 'in-dir "directory doesn't exist: ~a" p))]
              [do-body (λ (d)
                         (define dp (if (not (path-string? d))
                                        (format "~a" d)
                                        d))
                         (parameterize
                             ([current-directory (if (directory-exists? dp)
                                                     dp
                                                     (err dp))])
                           body))])
         (if (list? edirs)
             (for/list ([d edirs])
               (do-body d))
             (do-body edirs)))]))


(define-line-macro val
  (syntax-parser
    ;; pass through the normal case
    [(_ e) #'e]
    ;; I guess multiple values makes sense here...
    [(_ e ...) #'(values e ...)]
    ;; And we want it to work as a first-order function.
    [_ #'(λ args (apply values args))]))

(define (call-with-tmp-dir proc)
  (define td (make-temporary-file "call-with-tmp-dir-~a"
                                  'directory))
  (define result
    (with-handlers ([(λ (e) #t) (λ (e)
                                  (delete-directory/files td #:must-exist? #f)
                                  (raise e))])
      (proc td)))
  (delete-directory/files td #:must-exist? #f)
  result)

(define-line-macro with-tmp-dir
  (syntax-parser
    [(_ dir-name-var:id body:expr)
     #'(call-with-tmp-dir (λ (dir-name-var) body))]))

(define-line-macro in-tmp-dir
  (syntax-parser
    [(_ body:expr)
     #'(with-tmp-dir d (in-dir (values d) body))]))

(define-line-macro to-base
  (syntax-parser [(to_base num) #'(to-base num 16 )]
                 [(_ num base)
                  #'(string-upcase (~r #:base base num))]))

(define-line-macro to-dec
  (syntax-parser [(to-dec num) #'(to-dec num 16)]
                 [(_ num base)
                  #'(string->number (format "~a" num) base)]))

(define-pipeline-operator =map=
  #:joint (syntax-parser
            [(_ arg ...)
             (expand-pipeline-arguments
              #'(arg ...)
              (syntax-parser
                [(#t narg ...)
                 #'(object-pipeline-member-spec
                    (λ (prev-ret)
                      (map (λ (cur-obj-standin) ((narg cur-obj-standin) ...))
                           prev-ret)))]
                [(#f narg ...)
                 #'(object-pipeline-member-spec
                    (λ (prev-ret)
                      (map (λ (cur-obj-standin)
                             ((narg cur-obj-standin) ... cur-obj-standin))
                           prev-ret)))]))]))

(define-pipeline-operator =filter=
  #:joint (syntax-parser
            [(_ arg ...)
             (expand-pipeline-arguments
              #'(arg ...)
              (syntax-parser
                [(#t narg ...)
                 #'(object-pipeline-member-spec
                    (λ (prev-ret)
                      (filter (λ (cur-obj-standin) ((narg cur-obj-standin) ...))
                              prev-ret)))]
                [(#f narg ...)
                 #'(object-pipeline-member-spec
                    (λ (prev-ret)
                      (filter (λ (cur-obj-standin)
                                ((narg cur-obj-standin) ... cur-obj-standin))
                              prev-ret)))]))]))

(define-pipeline-operator =foldl=
  #:joint (syntax-parser
            [(_ accum-name accum-expr arg ...+)
             #'(object-pipeline-member-spec
                (λ (prev-ret)
                  (foldl (λ (iter-arg accum-name)
                           (syntax-parameterize ([current-pipeline-argument
                                                  (make-rename-transformer #'iter-arg)])
                             (arg ...)))
                         accum-expr
                         prev-ret)))]))

(define-syntax =bop=
  (make-rename-transformer #'=basic-object-pipe=))

(define-syntax =bope=
  (make-rename-transformer #'=basic-object-pipe/expression=))


;; this works:
;; i3-msg rename workspace to #{i3-msg -t get_workspaces |>> string->jsexpr =filter= (curryr hash-ref 'focused) |> first |> hash-ref _ 'num | cat |>> string-append _ ":" _ ":" "HIHI"}
;; this doesn't:
(define (rename-workspace new-name)
  (let* ([get-focused-ws (lambda (h) (first (filter (curryr hash-ref 'focused) h)))]
         [make-ws-name (lambda (num name)
                         (string-append num ":" num ":" (string-replace new-name
                                                         #px"\\s+" "-")))])
         {i3-msg rename workspace to
                 (make-ws-name (number->string
                                (hash-ref (get-focused-ws
                                           (string->jsexpr #{i3-msg -t get_workspaces}))
                                          'num))
                               new-name)}))


