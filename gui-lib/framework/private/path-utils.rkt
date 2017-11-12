#lang scheme/unit
  (require "sig.rkt"
           "../preferences.rkt")
  
  (import framework:main^)
  (export framework:path-utils^)
(init-depend framework:main^)

;; preferences initialized in main.rkt

(define current-backup-dir
  (preferences:get/set 'path-utils:backup-dir))

(define current-autosave-dir
  (preferences:get/set 'path-utils:autosave-dir))

  ; generate-autosave-name : (or/c #f path-string? path-for-some-system?) -> path?
  (define (generate-autosave-name maybe-old-path)
    (cond
      [maybe-old-path
       (let*-values ([(base name dir?) (split-path maybe-old-path)]
                     [(base) (cond
                               [(not (path? base))
                                (current-directory)]
                               [(relative-path? base)
                                (build-path (current-directory) base)]
                               [else
                                base])])
         (cond
           [(current-autosave-dir)
            =>
            (λ (dir)
              (make-unique-autosave-name dir (encode-as-path-element base name)))]
           [else
            (make-unique-autosave-name base name)]))]
      [else
       (make-unique-autosave-name (or (current-autosave-dir)
                                      (find-system-path 'doc-dir))
                                  (bytes->path-element #"mredauto"))]))


       ; make-unique-autosave-name : dir-path path-element -> path?
       (define (make-unique-autosave-name dir name)
         (let loop ([n 1])
          (let* ([numb (string->bytes/utf-8 (number->string n))]
                 [new-name
                  (build-path dir
                              (if (eq? (system-type) 'windows)
                                  (bytes->path-element
                                   (bytes-append (regexp-replace #rx#"\\..*$" 
                                                                 (path-element->bytes name)
                                                                 #"")
                                                 #"."
                                                 numb))
                                  (bytes->path-element
                                   (bytes-append #"#"
                                                 (path-element->bytes name)
                                                 #"#"
                                                 numb
                                                 #"#"))))])
            (if (file-exists? new-name)
                (loop (add1 n))
                new-name))))
  
  (define (generate-backup-name full-name)
    (let-values ([(pre-base name dir?) (split-path full-name)])
      (let ([base (if (path? pre-base)
                      pre-base
                      (current-directory))])
        (define name-element
          (let ([name-bytes (path-element->bytes name)])
            (bytes->path-element
             (cond
               [(and (eq? (system-type) 'windows)
                     (regexp-match #rx#"(.*)\\.[^.]*" name-bytes))
                =>
                (λ (m)
                  (bytes-append (cadr m) #".bak"))]
               [(eq? (system-type) 'windows)
                (bytes-append name-bytes #".bak")]
               [else
                (bytes-append name-bytes #"~")]))))
        (cond
          [(current-backup-dir)
           =>
           (λ (dir)
             (build-path dir (encode-as-path-element base name-element)))]
          [else
           (build-path base name-element)]))))



; encode-as-path-element : dir-path path-element -> path-element
; N.B. generate-backup-name may supply a relative directory, but
; we should always use a complete one.
; Using simplify-path does that and ensures no 'up or 'same
; Using ! is not completely robust, but works well enough for Emacs.
(define (encode-as-path-element base-maybe-relative name)
  (bytes->path-element
   (regexp-replace* (case (system-path-convention-type)
                      [(windows) #rx#"\\\\"]
                      [else #rx#"/"])
                    (path->bytes
                     (simplify-path (build-path base-maybe-relative name)))
                    #"!")))


