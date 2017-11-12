#lang racket/unit

(require "sig.rkt"
         "../preferences.rkt")
  
(import)
(export framework:path-utils^)

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
  (define sys
    (system-path-convention-type))
  (let loop ([n 1])
    (let* ([numb (string->bytes/utf-8 (number->string n))]
           [new-name
            (build-path dir
                        (case sys
                          [(windows)
                           (path-replace-extension name
                                                   (bytes-append #"."
                                                                 numb))]
                          [else
                           (bytes->path-element
                            (bytes-append #"#"
                                          (path-element->bytes name)
                                          #"#"
                                          numb
                                          #"#"))]))])
      (if (file-exists? new-name)
          (loop (add1 n))
          new-name))))
  
(define (generate-backup-name full-name)
  (define-values (pre-base old-name dir?)
    (split-path full-name))
  (define base
    (if (path? pre-base)
        pre-base
        (current-directory)))
  (define name-element
    (case (system-path-convention-type)
      [(windows)
       (path-replace-extension old-name #".bak")]
      [else
       (bytes->path-element
        (bytes-append (path-element->bytes old-name) #"~"))]))
  (cond
    [(current-backup-dir)
     =>
     (λ (dir)
       (build-path dir (encode-as-path-element base name-element)))]
    [else
     (build-path base name-element)]))



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


