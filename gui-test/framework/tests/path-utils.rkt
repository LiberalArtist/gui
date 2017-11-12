#lang racket/base

(require rackunit
         (rename-in framework
                    [path-utils:generate-autosave-name
                     generate-autosave-name]
                    [path-utils:generate-backup-name
                     generate-backup-name])
         racket/file
         racket/contract/base
         framework/preferences)

(define (path-base pth)
  (define-values (base name dir?)
    (split-path pth))
  base)

(let ([the-prefs-table (make-hash)])
  (parameterize ([preferences:low-level-put-preferences
                  (λ (syms vals)
                    (for ([sym (in-list syms)]
                          [val (in-list vals)])
                      (hash-set! the-prefs-table sym val)))]
                 [preferences:low-level-get-preference
                  (λ (sym [fail void])
                    (hash-ref the-prefs-table sym fail))])
    (define current-backup-dir
      (preferences:get/set 'path-utils:backup-dir))
    (define current-autosave-dir
      (preferences:get/set 'path-utils:autosave-dir))
    (define elem
      (bytes->path-element #"example.rkt"))
    (define dir
      (simplify-path (current-directory)))
    (define complete
      (build-path dir elem))
    ;; Tests with #f for directories
    (current-backup-dir #f)
    (current-autosave-dir #f)
    (check-equal? (path-base (generate-autosave-name #f))
                  (find-system-path 'doc-dir))
    (check-equal? (path-base (simplify-path (generate-autosave-name elem)))
                  dir)
    (check-equal? (path-base (generate-autosave-name complete))
                  dir)
    (check-equal? (path-base (simplify-path (generate-backup-name elem)))
                  dir)
    (check-equal? (path-base (generate-backup-name complete))
                  dir)
    ;; Tests with designated directories
    (define backup-dir
      (simplify-path
       (make-temporary-file "rkt-backup-dir-~a"
                            'directory)))
    (define autosave-dir
      (simplify-path
       (make-temporary-file "rkt-autosave-dir-~a"
                            'directory)))
    (dynamic-wind
     void
     (λ () 
       (current-backup-dir backup-dir)
       (current-autosave-dir autosave-dir)
       (check-equal? (path-base (generate-autosave-name #f))
                     autosave-dir)
       (check-equal? (path-base (generate-autosave-name elem))
                     autosave-dir)
       (check-equal? (path-base (generate-autosave-name complete))
                     autosave-dir)
       (check-equal? (path-base (generate-backup-name elem))
                     backup-dir)
       (check-equal? (path-base (generate-backup-name complete))
                     backup-dir)
       (define clashing-name
         (build-path dir "elsewhere" elem))
       (check-false (equal? (generate-autosave-name complete)
                            (generate-autosave-name clashing-name)))
       (check-false (equal? (generate-backup-name complete)
                            (generate-backup-name clashing-name))))
     (λ ()
       (delete-directory backup-dir)
       (delete-directory autosave-dir)))))





