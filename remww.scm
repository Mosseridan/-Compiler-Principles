(define remww
  (lambda (code)
    (cdr (fold-right
      (lambda (inst acc)
        (let ((src (cadr inst))
              (dst (caddr inst))
              (dead (car acc))
              (code (cdr acc)))
          (if (andmap (lambda (x) (member x dead)) dst)
              acc
              (cons
                (fold-left (lambda (new-dead x) (remove x new-dead)) (append dead dst) src)
                (cons inst code)))))
        '(())
        code))))
