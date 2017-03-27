;;; compiler.scm

;;; Programers: Idan Mosseri & Guy Hecht
(load "pc.scm")
(load "qq.scm")
(load "pattern-matcher.scm")

;;;;;;;;;;;;;;;;;;;;;;;; Scanner (<sexpr>) ;;;;;;;;;;;;;;;;;;;;;;;;

;; <False> ::= #f
(define <false>
  (new (*parser (word-ci "#f"))
      (*pack
        (lambda (_) #f))
  done))


;; <True> ::= #t
(define <true>
  (new (*parser (word-ci "#t"))
      (*pack
        (lambda (_) #t))
  done))


;; <Boolean> ::= #f | #t
(define <boolean>
  (new (*parser <true>)
      (*parser <false>)
      (*disj 2)
  done))

;; <Char Prefix>
(define <char-prefix>
  (new (*parser (word "#\\"))
  done))

(define <line-comment>
  (new (*parser (char #\;))
      (*parser <any-char>)
      (*parser (char #\newline))
      (*parser <end-of-input>)
      (*disj 2)
      *diff
      *star
      (*parser (char #\newline))
      (*parser <end-of-input>)
      (*disj 2)
      (*caten 3)
    done))

(define <sexpr-comment>
  (new (*parser (word "#;"))
      (*delayed (lambda () <sexpr>))
      (*caten 2)
    done))

;; <WhiteSpace> ::= any char with an ASCII code <= #\space
(define <white-space>
  (plus (range (integer->char 0) (integer->char 32))))


(define <discard>
  (new (*parser <white-space>)
      (*parser <line-comment>)
      (*parser <sexpr-comment>)
      (*disj 3)
      *plus
      (*pack
        (lambda (_) '()))
  done))

;; <Visible Simple Char> ::= c, where c is a character taht is
;;    greater than the space character in the ASCII table
(define <visible-simple-char>
  (new (*parser <any-char>)
      (*parser <white-space>)
      *diff
  done))


;; constructor for named chars and string meta chars
(define ^<named-char>
  (lambda (str ch)
    (new (*parser (word-ci str))
    (*pack (lambda (_) ch))
    done)))


;; <Named Char> ::= lambda, null, space, tab, newline, page, return
(define <named-char>
  (new (*parser (^<named-char> "lambda" (integer->char 955)))
      (*parser (^<named-char> "nul" #\nul))
      (*parser (^<named-char> "space" #\space))
      (*parser (^<named-char> "tab" #\tab))
      (*parser (^<named-char> "newline" #\newline))
      (*parser (^<named-char> "page" #\page))
      (*parser (^<named-char> "return" #\return))
      (*disj 7)
  done))

;; <Hex Char> ::= 0 | ... | 9 | a | ... | f | A | ... | F
(define <hex-char>
    (new (*parser (range #\0 #\9))
        (*parser (range #\a #\f))
        (*parser (range #\A #\F))
        (*disj 3)
    done))

;; <Hex Unicode Char> ::= x <Hex Char>+  (return the char with the given Unicode value)
(define <hex-unicode-char>
  (new (*parser (char-ci #\x))
      (*parser <hex-char>) *plus
      (*caten 2)
      (*pack-with
        (lambda (pref num)
          (integer->char
            (string->number
              (list->string num)
            16))))
  done))


;; <Char> ::= <Char Prefix> (<Hex Unicode Char> | <Named Char> | <Visible Siple Char>)
(define <char>
  (new (*parser <char-prefix>)
      (*parser <hex-unicode-char>)
      (*parser <named-char>)
      (*parser <visible-simple-char>)
      (*disj 3)
      (*caten 2)
      (*parser <any-char>)
      (*parser <discard>)
      (*parser (char #\)))
      (*parser (char #\]))
      (*parser (char #\,))
      (*disj 4)
      *diff
      *not-followed-by
      (*pack-with
        (lambda (pref ch) ch))
  done))


;; <Symbol Char> ::= (0 | ... | 9) | (a | ... | z) | (A | ... | Z)
;;    | ! | $ | ^ | * | - | _ | = | + | > | < | ? | / | :
(define <symbol-char>
  (new (*parser (range #\0 #\9))
      (*parser (range #\a #\z))
      (*parser (range #\A #\Z))
      (*pack (lambda (ch)
                (integer->char (+ (char->integer ch) 32))))
      (*parser (char #\!))
      (*parser (char #\$))
      (*parser (char #\^))
      (*parser (char #\*))
      (*parser (char #\-))
      (*parser (char #\_))
      (*parser (char #\=))
      (*parser (char #\+))
      (*parser (char #\>))
      (*parser (char #\<))
      (*parser (char #\?))
      (*parser (char #\/))
      (*parser (char #\:))
      (*disj 16)
  done))


;; <Symbol> ::= <Symbol Char>+
(define <symbol>
  (new (*parser <symbol-char>) *plus
      (*pack
        (lambda (sym-lst)
          (string->symbol
            (list->string sym-lst))))
  done))


;; <Natural> ::= (0 | ... | 9)+
(define <nat>
  (new (*parser (range #\0 #\9)) *plus
      (*pack
        (lambda (nat)
          (string->number
            (list->string
              `(,@nat)))))
  done))


;; <Integer> ::= (+ | -)? <Natural>
(define <int>
  (new
      ; nat
      (*parser <nat>)
      ; + nat
      (*parser (char #\+))
      (*parser <nat>)
      (*caten 2)
      (*pack-with
        (lambda (plus nat) nat))
      ; - nat
      (*parser (char #\-))
      (*parser <nat>)
      (*caten 2)
      (*pack-with
        (lambda (minus nat) (- nat)))

      (*disj 3)

  done))


;; Ftation ::= <Integer> / <Natural>
(define <fract>
  (new (*parser <int>)
      (*parser (char #\/))
      (*parser <nat>)
      (*guard (lambda (n) (not (zero? n))))
      (*caten 3)
      (*pack-with
        (lambda (num div den)
          (/ num den)))
  done))


;; <Number> ::= <Fraction> |  <Integer>
(define <number>
  (new (*parser <fract>)
      (*parser <int>)
      (*disj 2)
      (*parser <symbol-char>)
      *not-followed-by
  done))


;; <String Literal Char> ::= c, where c is any character other than the backslash character (\)
(define <string-literal-char>
  (new (*parser <any-char>)
      (*parser (char #\\))
      (*parser (char #\"))
      (*disj 2)
      *diff
  done))


;; <String Meta Char> ::= \\ | \" | \t | \f | \n | \r
(define <string-meta-char>
  (new (*parser (^<named-char> "\\\\" #\\))
      (*parser (^<named-char> "\\\"" #\"))
      (*parser (^<named-char> "\\t" #\tab))
      (*parser (^<named-char> "\\f" #\page))
      (*parser (^<named-char> "\\n" #\newline))
      (*parser (^<named-char> "\\r" #\return))
      (*disj 6)
  done))


;; <String Hex Char> ::= \ <Hex Unicode Char> ; (equal to: \x <Hex-Char>+ ;)
(define <string-hex-char>
  (new (*parser (char #\\))
      (*parser <hex-unicode-char>)
      (*parser (char #\;))
      (*caten 3)
      (*pack-with
        (lambda (perf char suf) char))
  done))


;; <String Char> ::= <String Meta Char> | <String Hex Char> | <String Literal Char>
(define <string-char>
  (new (*parser <string-meta-char>)
      (*parser <string-hex-char>)
      (*parser <string-literal-char>)
      (*disj 3)
    done))


;; <String> ::= " <String Char>* "
(define <string>
  (new (*parser (char #\"))
      (*parser <string-char>) *star
      (*parser (char #\"))
      (*caten 3)
      (*pack-with
        (lambda (pr1 str pr2) (list->string str)))
  done))


;; <sexpr> ::= <Boolean> | <Char> | <Number> | <String> | <Symbol> |
;;     <Proper List> | <Improper List> | <Vector> |
;;     <Quoted> | <Quasi Quoted> | <Unquoted> | <Unquote And Spliced> |
(define <sexpr>
  (new (*parser <discard>) *maybe
      (*parser <boolean>)
      (*parser <char>)
      (*parser <number>)
      (*parser <string>)
      (*parser <symbol>)
      (*delayed (lambda () <proper-list>))
      (*delayed (lambda () <improper-list>))
      (*delayed (lambda () <vector>))
      (*delayed (lambda () <quoted>))
      (*delayed (lambda () <quasi-quoted>))
      (*delayed (lambda () <unquoted>))
      (*delayed (lambda () <unquote-spliced>))
      (*delayed (lambda () <infix-extension>))
      (*disj 13)
      (*parser <discard>) *maybe
      (*caten 3)
      (*pack-with
        (lambda (ws1 sexpr ws2) sexpr))
  done))

;; <Proper List> ::= ( <sexpr>* )
(define <proper-list>
  (new (*parser (char #\( ))
      (*parser <sexpr>) *star
      (*parser (char #\) ))
      (*caten 3)
      (*pack-with
        (lambda (pr1 sexpr-lst pr2) sexpr-lst))
  done))


;; <Improper List> ::= ( <sexpr>+ . <sexpr> )
(define <improper-list>
  (new (*parser (char #\( ))
      (*parser <sexpr>) *plus
      (*parser (char #\.))
      (*parser <sexpr>)
      (*parser (char #\) ))
      (*caten 5)
      (*pack-with
        (lambda (pr1 sexpr-lst dot sexpr pr2) (append sexpr-lst sexpr)))
  done))

;; <Vector> ::= # ( <sexpr>* )
(define <vector>
  (new (*parser (char #\#))
      (*parser <proper-list>)
      (*caten 2)
      (*pack-with
        (lambda (pref lst) (list->vector lst)))
  done))


;; <Quoted> ::= ' <sexpr>
(define <quoted>
  (new (*parser (char #\'))
      (*parser <sexpr>)
      (*caten 2)
      (*pack-with
        (lambda (pref sexpr) (list 'quote sexpr)))
  done))


;; <Quasi Quoted> ::= ` <sexpr>
(define <quasi-quoted>
  (new (*parser (char #\`))
      (*parser <sexpr>)
      (*caten 2)
    (*pack-with
        (lambda (pref sexpr)  (list 'quasiquote sexpr)))
  done))


;; <Unquoted> ::= , <sexpr>
(define <unquoted>
  (new (*parser (char #\,))
      (*parser <sexpr>)
      (*caten 2)
    (*pack-with
        (lambda (pref sexpr)  (list 'unquote sexpr)))
  done))


;; <Unquote And Spliced> ::= ,@ <sexpr>
(define <unquote-spliced>
  (new (*parser (word ",@"))
      (*parser <sexpr>)
      (*caten 2)
    (*pack-with
        (lambda (pref sexpr)  (list 'unquote-splicing sexpr)))
  done))


;; <Infix Prefix Extension Prefix> ::= ## | #%
(define <infix-prefix-extension-prefix>
  (new (*parser (word "##"))
      (*parser (word "#%"))
      (*disj 2)
  done))


;; <Infix Symbol> ::= <Symbol> other than +, -, *, **, ^, /.
(define <infix-symbol>
  (new (*parser <symbol-char>)
      (*parser (char #\+))
      (*parser (char #\-))
      (*parser (char #\*))
      (*parser (word "**"))
      (*parser (char #\^))
      (*parser (char #\/))
      (*disj 6)
      *diff
      *plus
      (*pack
        (lambda (sym-lst)
          (string->symbol
            (list->string sym-lst))))
  done))

(define <infix-number>
  (new (*parser <fract>)
      (*parser <int>)
      (*disj 2)
      (*parser <infix-symbol>)
      (*parser (range #\0 #\9))
      *diff
      *not-followed-by
  done))


;; <Infix Expression> ::=  <Infix Add> | <Infix Neg> | <Infix Sub> | <Infix Mul> |
;;      <Infix Div> | <Infix Pow> | <Infix Array Get> | <Infix Funcall> |
;;      <Infix Paren> | <Infix sexpr Escape> | <Infix Symbol> | <Number>
(define <infix-expression>
  (new
      (*delayed (lambda () <infix-priority5>))
    done))


(define <infix-expr-comment>
  (new (*parser (word "#;"))
      (*parser <infix-expression>)
      (*caten 2)
    done))


(define <infix-discard>
  (new (*parser <white-space>)
      (*parser <line-comment>)
      (*parser <infix-expr-comment>)
      (*disj 3)
      *plus
      (*pack
        (lambda (_) '()))
  done))


(define ^<infix-operation>
  (lambda (<first-expr> <op> <rest-expr>)
    (new (*parser <first-expr>)
        (*parser  <op>)
        (*parser <rest-expr>)
        (*caten 2)
        *star
        (*caten 2)
        (*pack-with
          (lambda (first rest)
            (fold-left
              (lambda (a x) `(,(car x) ,a ,@(cdr x)))
              first
              rest
            )))
      done)))


;; <Infix Paren> ::= ( <Infix Expression> )
(define <infix-paren>
  (new (*parser (char #\())
      (*parser <infix-expression>)
      (*parser (char #\)))
      (*caten 3)
      (*pack-with (lambda (pr1 expr pr2) expr))
  done))


;; <Infix sexpr Escape> ::= <Infix Prefix Extension Prefix> <sexpr>
(define <infix-sexpr-escape>
  (new (*parser <infix-prefix-extension-prefix>)
      (*parser <sexpr>)
      (*caten 2)
      (*pack-with (lambda (pref sexpr) sexpr))
  done))


(define <infix-priority1>
  (new (*parser <infix-discard>) *maybe
      (*parser <char>)
      (*parser <infix-number>)
      (*parser <infix-symbol>)
      (*parser <infix-paren>)
      (*parser <infix-sexpr-escape>)
      (*delayed (lambda () <infix-neg>))
      (*disj 6)
      (*parser <infix-discard>) *maybe
      (*caten 3)
      (*pack-with
        (lambda (ws1 expr ws2) expr))
  done))


;; <Infix Array Get> ::= <Infix Expression> [ <Infix Expression> ]
(define <infix-array-get>
  (new (*parser (char #\[))
      (*parser <infix-expression>)
      (*parser (char #\]))
      (*caten 3)
      (*pack-with
        (lambda (pr1 index pr2)
          (lambda (arr) `(vector-ref ,arr ,index))))
  done))


;; <Infix Arg List> ::= <Infix Expression> (, <Infix Expression>) | epsilon
(define <infix-arg-lst>
  (new (*parser (^<separated-exprs> <infix-expression> (char #\,)))
      (*parser <infix-discard>)
      (*parser <epsilon>)
      (*disj 3)
  done))


;; <Infix Funcall> ::= <Infix Expression> ( <Infix Arg List> )
(define <infix-funcall>
  (new (*parser (char #\())
      (*parser <infix-arg-lst>)
      (*parser (char #\)))
      (*caten 3)
      (*pack-with
        (lambda (pr1 args pr2)
          (lambda (fun) `(,fun ,@args))))
  done))


(define <infix-priority2>
  (new (*parser <infix-priority1>)
      (*parser <infix-funcall>)
      (*parser <infix-array-get>)
      (*disj 2)
      (*parser <infix-discard>) *maybe
      (*caten 2)
      (*pack-with (lambda (expr ws) expr))
      *star
      (*caten 2)
      (*pack-with
        (lambda (first rest)
          (fold-left
            (lambda (a x) (x a))
            first
            rest)))
  done))


(define <infix-expt>
  (new (*parser <infix-priority2>)
      (*parser (char #\^))
      (*parser (word "**"))
      (*disj 2)
      (*caten 2)
      (*pack-with (lambda (expr expt) expr))
      *star
      (*parser <infix-priority2>)
      (*caten 2)
      (*pack-with
        (lambda (rest last)
          (fold-right
            (lambda (x a) `(expt ,x ,a))
            last
            rest
          )))
  done))


(define <infix-priority3> <infix-expt>)

(define <infix-priority4>
  (^<infix-operation>
    <infix-priority3>
    (new (*parser (char #\*))
        (*pack (lambda (_) '*))
        (*parser (char #\/))
        (*pack (lambda (_) '/))
        (*disj 2)
      done)
    <infix-priority3>))


(define <infix-priority5>
  (^<infix-operation>
    <infix-priority4>
    (new (*parser (char #\+))
        (*pack (lambda (_) '+))
        (*parser (char #\-))
        (*pack (lambda (_) '-))
        (*disj 2)
      done)
    <infix-priority4>))


;; <Infix Neg> ::= - <Infix Expression>
(define <infix-neg>
  (new (*parser (char #\-))
      (*parser <infix-priority2>)
      (*caten 2)
      (*pack-with
        (lambda (minus expr) (list '- expr)))
  done))


;; <Infix Extension> ::= <Infix Prefix Extension Prefix> <Infix Expression>
(define <infix-extension>
  (new (*parser <infix-prefix-extension-prefix>)
      (*parser <infix-expression>)
      (*caten 2)
      (*pack-with
        (lambda (pref expr) expr))
  done))



;;;;;;;;;;;;;;;;;;;;;;;; tag-Parser (parse) ;;;;;;;;;;;;;;;;;;;;;;;;

(define *reserved-words*
  '(and begin cond define do else if lambda let let* letrec or quasiquote unquote unquote-splicing quote set!)
  )

(define *void-object* (void))


(define not-reserved?
  (lambda (x)
    (not (member x *reserved-words*))))

(define not-null?
  (lambda (x)
    (not (null? x))))

(define valid-varl?
  (lambda (varl)
    (cond ((null? varl) #t)
          ((not (var? (car varl))) #f)
          ((null? (cdr varl)) #t)
          ((member (car varl) (cdr varl)) #f)
          (else (valid-varl? (cdr varl))))))

(define var?
  (lambda (x)
    (and (symbol? x)
      (not-reserved? x))))

(define quasiquote? (^quote? 'quasiquote))

(define simple-const?
  (lambda (x)
    (or
      (vector? x)
      (boolean? x)
      (number? x)
      (char? x)
      (string? x))))


(define beginify
	(lambda (s)
		(cond
			((null? s) *void-object*)
			((null? (cdr s)) (car s))
			(else `(begin ,@s)))))


;;parse: takes an SExpr and returns PSExpr which is the parsed version of the given SExpr
(define parse
	(let ((run
			(compose-patterns

        ;;; constants
        ;; simple constant
        (pattern-rule
					(? 'c simple-const?)
					(lambda (c) `(const ,c)))

        ;;quoted
        (pattern-rule
					(? 'c quote?)
					(lambda (c) `(const ,(cadr c))))

        ;; quasiquoted
        (pattern-rule
          (? 'c quasiquote?)
          (lambda (c) (parse (expand-qq (cadr c)))))

        ;;; variables
        (pattern-rule
					(? 'v var?)
					(lambda (v) `(var ,v)))

        ;;; conditionals
        ;; if
        (pattern-rule
					`(if ,(? 'test) ,(? 'dit))
					(lambda (test dit) `(if3 ,(parse test) ,(parse dit) (const ,*void-object*))))
				(pattern-rule
					`(if ,(? 'test) ,(? 'dit) ,(? 'dif))
					(lambda (test dit dif) `(if3 ,(parse test) ,(parse dit) ,(parse dif))))

        ;; cond
        (pattern-rule
          `(cond (else ,@(? 'exprs)))
          (lambda (exprs) (parse (beginify exprs))))
        (pattern-rule
          `(cond ,(? 'first) . ,(? 'rest ))
          (lambda (first rest)
            (if (pair? first)
                (if (null? rest)
                    (parse `(if ,(car first) ,(beginify (cdr first))))
                    (parse `(if ,(car first) ,(beginify (cdr first)) (cond ,@rest))))
                (error 'parser (format "This is not a valid cond-rib: ~s" first)))))

        ;; disjunctions (or)
        (pattern-rule
          `(or)
          (lambda () `(const #f)))
        (pattern-rule
          `(or ,(? 'expr). ,(? 'exprs list?))
          (lambda (expr exprs)
            (if (null? exprs)
                (parse expr)
                `(or (,(parse expr) ,@(map parse exprs))))))

        ;; conjunction (and)
        (pattern-rule
          `(and)
          (lambda () '(const #t)))
        (pattern-rule
          `(and ,(? 'expr) . ,(? 'exprs))
          (lambda (expr exprs)
            (if (null? exprs)
                  (parse expr)
                  (parse `(if ,expr (and ,@exprs) #f)))))

        ;;; definenitions
        ;; MIT-style define
        (pattern-rule
          `(define ,(? 'varl pair? not-null? (lambda (varl) (var? (car varl)))) ,(? 'expr) . ,(? 'exprs))
          (lambda (varl expr exprs)
            (parse `(define ,(car varl) (lambda ,(cdr varl) ,expr ,@exprs)))))

        ;; define
        (pattern-rule
          `(define ,(? 'var var?) ,@(? 'exprs))
          (lambda (var exprs)
            `(def (var ,var) ,(parse (beginify exprs)))))

        ;;; assignments
        (pattern-rule
          `(set! ,(? 'var var?) ,(? 'expr))
          (lambda (var expr) `(set (var ,var) ,(parse expr))))

        ;;; applicatoins
        (pattern-rule
          `,(? 'list list? (lambda (list) (not-reserved? (car list))))
          (lambda (list)
              `(applic ,(parse (car list)) ,(map parse (cdr list)))))

        ;;; sequences
        (pattern-rule
          `(begin ,@(? 'seq))
          (letrec ((flaten-sequence
                    (lambda (seq)
                      (fold-left
                        (lambda (acc x)
                          (match `(begin ,@(? 'seq)) x (lambda (seq) `(,@acc ,@(flaten-sequence seq))) (lambda () `(,@acc ,x))))
                        '()
                        seq))))
            (lambda (seq)
              (let ((flat-seq (flaten-sequence seq)))
                (cond ((null? flat-seq) `(const ,*void-object*))
                      ((null? (cdr flat-seq)) (parse (car flat-seq)))
                      (else `(seq ,(map parse flat-seq))))))))

        ;;; lambda
        (pattern-rule
          `(lambda ,(? 'args (lambda (x) (or (list? x) (pair? x) (var? x)))) ,@(? 'body))
          (lambda (args body)
            (identify-lambda
                  args
                  (lambda (s) (if (valid-varl? s)
                                  `(lambda-simple ,s ,(parse (beginify body)))
                                  (error 'parser (format "Invalid parameter list: ~s" s))))
                  (lambda (s opt) (if (valid-varl? s)
                                      `(lambda-opt ,s ,opt ,(parse (beginify body)))
                                      (error 'parser (format "Invalid parameter list: ~s" `(,@s . ,opt)))))
                  (lambda (var) `(lambda-var ,var ,(parse (beginify body)))))))

        ;;; let
        (pattern-rule
          `(let ,(? 'bindings list? ) ,(? 'expr) . ,(? 'exprs))
          (lambda (bindings expr exprs)
            (let ((vars (map car bindings))
                  (vals (map cadr bindings)))
                (parse `((lambda ,vars ,expr ,@exprs) ,@vals)))))

        ;;; let*
        (pattern-rule
          `(let* () ,(? 'expr) . ,(? 'exprs))
          (lambda (expr exprs) (parse `(let () ,expr ,@exprs))))
        (pattern-rule
            `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest))  ,(? 'expr) . ,(? 'exprs))
          	 (lambda (var val rest expr exprs)
                (if (null? rest)
                  (parse `(let ((,var ,val)) ,expr ,@exprs))
                  (parse `(let ((,var ,val)) (let* ,rest ,expr ,@exprs))))))

        ;;; letrec
        (pattern-rule
          `(letrec ,(? 'bindings list? (lambda (bindings) (andmap pair? bindings))) ,(? 'expr)  . ,(? 'exprs))
          (lambda (bindings expr exprs)
            (let ((vars (map car bindings))
                  (vals (map (lambda (x) #f) bindings))
                  (setters (map (lambda (binding) `(set! ,(car binding) ,@(cdr binding))) bindings)))
                (parse  `((lambda ,vars ,@setters ((lambda () ,expr ,@exprs))) ,@vals)))))

  		)))
			(lambda (e)
        (if (null? e)
            (error 'parser (format "Unknown form: ~s" e))
				    (run e (lambda () (error 'parser (format "Unknown form: ~s" e))))))))


(define identify-lambda
	(lambda (varl ret-simple ret-opt ret-var)
		(cond
			((null? varl) (ret-simple '()))
			((var? varl) (ret-var varl))
      (else (identify-lambda (cdr varl)
                             (lambda (s) (ret-simple `(,(car varl) ,@s))) ;simple
                             (lambda (s opt) (ret-opt `(,(car varl) ,@s) opt)) ;opt
                             (lambda (var) (ret-opt `(,(car varl)) var))))))) ;variadic





;;;;;;;;;;;;;;;;;;;;;;;; Assignment 3 -  ;;;;;;;;;;;;;;;;;;;;;;;;

(define is?
  (lambda (tag pe)
    (eq? tag (car pe))))

(define ^get
  (lambda (form proc-name getter)
    (lambda (pe)
      (if (is? form pe)
          (getter pe)
          (error proc-name (format "Not a valid ~s form: ~s" form pe))))))

(define var-name (^get 'var 'var-name cadr))

(define fvar-name (^get 'fvar 'fvar-name cadr))

(define pvar-name (^get 'pvar 'pvar-name cadr))
(define pvar-minor (^get 'pvar 'pvar-minor caddr))

(define bvar-name (^get 'bvar 'bvar-name cadr))
(define bvar-major (^get 'bvar 'bvar-major caddr))
(define bvar-minor (^get 'bvar 'bvar-minor cadddr))

(define if3-test (^get 'if3 'if3-test cadr))
(define if3-dit (^get 'if3 'if3-dit caddr))
(define if3-dif (^get 'if3 'if3-dif cadddr))

(define def-var (^get 'def 'def-var cadr))
(define def-expr (^get 'def 'def-exprs caddr))

(define set-var (^get 'set 'set-var cadr))
(define set-expr (^get 'set 'set-expr caddr))

(define or-exprs (^get 'or 'or-exprs cadr))

(define seq-exprs (^get 'seq 'seq-exprs cadr))

(define applic-proc (^get 'applic 'applic-proc cadr))
(define applic-args (^get 'applic 'applic-args caddr))

(define lambda-simple-params (^get 'lambda-simple 'lambda-simple-params cadr))
(define lambda-simple-body (^get 'lambda-simple 'lambda-simple-body caddr))

(define lambda-opt-params (^get 'lambda-opt 'lambda-opt-params cadr))
(define lambda-opt-opt (^get 'lambda-opt 'lambda-opt-opt caddr))
(define lambda-opt-body (^get 'lambda-opt 'lambda-opt-body cadddr))

(define lambda-var-params (^get 'lambda-var 'lambda-var-params cadr))
(define lambda-var-body (^get 'lambda-var 'lambda-var-body caddr))

(define box-var (^get 'box 'box-var cadr))

(define box-get-var (^get 'box-get 'box-get-var cadr))

(define box-set-var (^get 'box-set 'box-set-var cadr))
(define box-set-expr (^get 'box-set 'box-set-expr caddr))


(define default-cases
  `((const ,(lambda (run) (lambda (c) `(const ,c))))
  (var ,(lambda (run) (lambda (name) `(var ,name))))
  (fvar ,(lambda (run) (lambda (name) `(fvar ,name))))
  (pvar ,(lambda (run) (lambda (name minor) `(pvar ,name ,minor))))
  (bvar ,(lambda (run) (lambda (name major minor) `(bvar ,name ,major ,minor))))
  (if3 ,(lambda (run) (lambda (test dit dif) `(if3 ,(run test) ,(run dit) ,(run dif)))))
  (def ,(lambda (run) (lambda (var expr) `(def ,(run var) ,(run expr)))))
  (set ,(lambda (run) (lambda (var expr) `(set ,(run var) ,(run expr)))))
  (or ,(lambda (run) (lambda (exprs) `(or ,(map run exprs)))))
  (seq ,(lambda (run) (lambda (exprs) `(seq ,(map run exprs)))))
  (applic ,(lambda (run) (lambda (proc args) `(applic ,(run proc) ,(map run args)))))
  (tc-applic ,(lambda (run) (lambda (proc args) `(tc-applic ,(run proc) ,(map run args)))))
  (lambda-simple ,(lambda (run) (lambda (params body) `(lambda-simple ,params ,(run body)))))
  (lambda-opt ,(lambda (run) (lambda (params opt body) `(lambda-opt ,params ,opt ,(run body)))))
  (lambda-var ,(lambda (run) (lambda (params body) `(lambda-var ,params ,(run body)))))
  (box ,(lambda (run) (lambda (var) `(box ,(run var)))))
  (box-get ,(lambda (run) (lambda (var) `(box-get ,(run var)))))
  (box-set ,(lambda (run) (lambda (var expr) `(box-set ,(run var) ,(run expr)))))
  (else ,(lambda (run) (lambda (pe) (error 'default-cases (format "Invalid form: ~s" pe)))))))

(define curry-map
  (lambda (proc curry list)
    (let ((c-proc (lambda (pe) ((proc pe) curry))))
      (map c-proc list))))

(define curry-ormap
  (lambda (proc curry lst)
    (let ((c-proc (lambda (pe) ((proc pe) curry))))
      (ormap c-proc lst))))

(define curry-andmap
  (lambda (proc curry lst)
    (let ((c-proc (lambda (pe) ((proc pe) curry))))
      (andmap c-proc lst))))

(define curry-default-cases
  `((const ,(lambda (run) (lambda (c) (lambda (curry) `(const ,c)))))
    (var ,(lambda (run) (lambda (name) (lambda (curry) `(var ,name)))))
    (fvar ,(lambda (run) (lambda (name) (lambda (curry) `(fvar ,name)))))
    (pvar ,(lambda (run) (lambda (name minor) (lambda (curry) `(pvar ,name ,minor)))))
    (bvar ,(lambda (run) (lambda (name major minor) (lambda (curry) `(bvar ,name ,major ,minor)))))
    (if3 ,(lambda (run) (lambda (test dit dif) (lambda (curry) `(if3 ,((run test) curry) ,((run dit) curry) ,((run dif) curry))))))
    (def ,(lambda (run) (lambda (var expr) (lambda (curry) `(def ,((run var) curry) ,((run expr) curry))))))
    (set ,(lambda (run) (lambda (var expr) (lambda (curry) `(set ,((run var) curry) ,((run expr) curry))))))
    (or ,(lambda (run) (lambda (exprs) (lambda (curry) `(or ,(curry-map run curry exprs))))))
    (seq ,(lambda (run) (lambda (exprs) (lambda (curry) `(seq ,(curry-map run curry exprs))))))
    (applic ,(lambda (run) (lambda (proc args) (lambda (curry) `(applic ,((run proc) curry) ,(curry-map run curry args))))))
    (tc-applic ,(lambda (run) (lambda (proc args) (lambda (curry) `(tc-applic ,((run proc) curry) ,(curry-map run curry args))))))
    (lambda-simple ,(lambda (run) (lambda (params body) (lambda (curry) `(lambda-simple ,params ,((run body) curry))))))
    (lambda-opt ,(lambda (run) (lambda (params opt body) (lambda (curry) `(lambda-opt ,params ,opt ,((run body) curry))))))
    (lambda-var ,(lambda (run) (lambda (params body) (lambda (curry) `(lambda-var ,params ,((run body) curry))))))
    (box ,(lambda (run) (lambda (var) (lambda (curry) `(box ,((run var) curry))))))
    (box-get ,(lambda (run) (lambda (var) (lambda (curry) `(box-get ,((run var) curry))))))
    (box-set ,(lambda (run) (lambda (var expr) (lambda (curry) `(box-set ,((run var) curry) ,((run expr) curry))))))
    (else ,(lambda (run) (lambda (pe) (lambda (curry) (error 'some-curryied-traverse-function (format "Invalid form: ~s" pe))))))))

(define curry-boolean-default-cases
  `((const ,(lambda (run) (lambda (c) (lambda (curry) #f))))
    (var ,(lambda (run) (lambda (name) (lambda (curry) #f))))
    (fvar ,(lambda (run) (lambda (name) (lambda (curry) #f))))
    (pvar ,(lambda (run) (lambda (name minor) (lambda (curry) #f))))
    (bvar ,(lambda (run) (lambda (name major minor) (lambda (curry) #f))))
    (if3 ,(lambda (run) (lambda (test dit dif) (lambda (curry) (curry-ormap run curry `(,test ,dit ,dif))))))
    (def ,(lambda (run) (lambda (var expr) (lambda (curry) (or ((run var) curry) ((run expr) curry))))))
    (set ,(lambda (run) (lambda (var expr) (lambda (curry) (or ((run var) curry) ((run expr) curry))))))
    (or ,(lambda (run) (lambda (exprs) (lambda (curry) (curry-ormap run curry exprs)))))
    (seq ,(lambda (run) (lambda (exprs) (lambda (curry) (curry-ormap run curry exprs)))))
    (applic ,(lambda (run) (lambda (proc args) (lambda (curry) (curry-ormap run curry `(,proc ,@args))))))
    (tc-applic ,(lambda (run) (lambda (proc args) (lambda (curry) (curry-ormap run curry `(,proc ,@args))))))
    (lambda-simple ,(lambda (run) (lambda (params body) (lambda (curry) ((run body) curry)))))
    (lambda-opt ,(lambda (run) (lambda (params opt body) (lambda (curry) ((run body) curry)))))
    (lambda-var ,(lambda (run) (lambda (params body) (lambda (curry) ((run body) curry)))))
    (box ,(lambda (run) (lambda (var) (lambda (curry) ((run var) curry)))))
    (box-get ,(lambda (run) (lambda (var) (lambda (curry) ((run var) curry)))))
    (box-set ,(lambda (run) (lambda (var expr) (lambda (curry) (or ((run var) curry) ((run expr) curry))))))
    (else ,(lambda (run) (lambda (pe) (lambda (curry) (error 'some-curryied-boolean-traverse-function (format "Invalid form: ~s" pe))))))))


(define traverse
  (lambda (default-cases)
      (lambda (cases)
        (let ((cases (append cases default-cases)))
	        (apply
	           (lambda (with-const with-var with-fvar with-pvar with-bvar with-if3 with-def with-set with-or with-seq with-applic with-tc-applic with-lambda-simple with-lambda-opt with-lambda-var with-box with-box-get with-box-set with-else)
	             (letrec ((run
	               (lambda (pe)
	                 (cond
              			((is? 'const pe) (with pe (lambda (tag-const c) ((with-const run) c))))
              			((is? 'var pe) (with pe (lambda (tag-var name) ((with-var run) name))))
                    ((is? 'fvar pe) (with pe (lambda (tag-fvar name) ((with-fvar run) name))))
                    ((is? 'pvar pe) (with pe (lambda (tag-pvar name minor) ((with-pvar run) name minor))))
                    ((is? 'bvar pe) (with pe (lambda (tag-bvar name major minor) ((with-bvar run) name major minor))))
                    ((is? 'if3 pe) (with pe (lambda (tag-if3 test dit dif) ((with-if3 run) test dit dif))))
                    ((is? 'def pe) (with pe (lambda (tag-def var expr) ((with-def run) var expr))))
                    ((is? 'set pe) (with pe (lambda (tag-set var expr) ((with-set run) var expr))))
                    ((is? 'or pe) (with pe (lambda (tag-or exprs) ((with-or run) exprs))))
                    ((is? 'seq pe) (with pe (lambda (tag-seq exprs) ((with-seq run) exprs))))
                    ((is? 'applic pe) (with pe (lambda (tag-applic proc args) ((with-applic run) proc args))))
                    ((is? 'tc-applic pe) (with pe (lambda (tag-tc-applic proc args) ((with-tc-applic run) proc args))))
              			((is? 'lambda-simple pe) (with pe (lambda (tag-lambda-simple params body) ((with-lambda-simple run) params body))))
              			((is? 'lambda-opt pe) (with pe (lambda (tag-lambda-opt params opt body) ((with-lambda-opt run) params opt body))))
              			((is? 'lambda-var pe) (with pe (lambda (tag-lambda-var params body) ((with-lambda-var run) params body))))
                    ((is? 'box pe) (with pe (lambda (tag-box var) ((with-box run) var))))
                    ((is? 'box-get pe) (with pe (lambda (tag-box-get var) ((with-box-get run) var))))
                    ((is? 'box-set pe) (with pe (lambda (tag-box-set var expr) ((with-box-set run) var expr))))
              			(else ((with-else run) pe))))))
                	     run))
                	 (map (lambda (case)
                          (cadr (assoc case cases)))
                	    '(const var fvar pvar bvar if3 def set or seq applic tc-applic lambda-simple lambda-opt lambda-var box box-get box-set else)))))))


(define h-n-d
  (letrec ((extract-defines
              (lambda (pes defined-vars defined-vals)
                  (let ((first (car pes))
                        (rest (cdr pes)))
                    (if (is? 'def first)
                        (extract-defines rest
                          `(,@defined-vars ,(cadr (def-var first)))
                          `(,@defined-vals (set ,(def-var first) ,(eliminate-nested-defines (def-expr first)))))
                        `(applic (lambda-simple ,defined-vars (seq (,@defined-vals ,@(map eliminate-nested-defines pes))))
                                ,(map (lambda (x) '(const #f)) defined-vars)))))))
      (lambda (pes) (extract-defines pes '() '()))))


(define eliminate-nested-defines
  ((traverse default-cases)
    `((lambda-simple ,(lambda (e-n-d) (lambda (params body)
        (if (and (is? 'seq body) (is? 'def (caadr body)))
            `(lambda-simple ,params ,(h-n-d (cadr body)))
            `(lambda-simple ,params ,(e-n-d body))))))
      (lambda-opt ,(lambda (e-n-d) (lambda (params opt body)
        (if (and (is? 'seq body) (is? 'def (caadr body)))
            `(lambda-opt ,params ,opt ,(h-n-d (cadr body)))
            `(lambda-opt ,params ,opt ,(e-n-d body))))))
      (lambda-var ,(lambda (e-n-d) (lambda (params body)
        (if (and (is? 'seq body) (is? 'def (caadr body)))
            `(lambda-var ,params ,(h-n-d (cadr body)))
            `(lambda-var ,params ,(e-n-d body))))))
      (else ,(lambda (e-n-d) (lambda (pe)
        (error 'eliminate-nested-defines (format "Invalid form: ~s" pe))))))))


(define remove-applic-lambda-nil
  ((traverse default-cases)
    `((applic ,(lambda (r-a-l-n) (lambda (proc args)
        (if (and (null? args) (is? 'lambda-simple proc) (null? (lambda-simple-params proc)))
            (r-a-l-n (lambda-simple-body proc))
            `(applic ,(r-a-l-n proc) ,(map r-a-l-n args))))))
      (else ,(lambda (r-a-l-n) (lambda (pe)
        (error 'remove-applic-lambda-nil (format "Invalid form: ~s" pe))))))))


(define get-occ?
  (letrec ((run
    ((traverse curry-boolean-default-cases)
      `((var ,(lambda (get-occ?) (lambda (name) (lambda (v-name)
          (eq? v-name name)))))
      (set ,(lambda (get-occ?) (lambda (var expr) (lambda (v-name)
          ((get-occ? expr) v-name)))))
      (lambda-simple ,(lambda (get-occ?) (lambda (params body) (lambda (v-name)
          (and (not (member v-name params)) ((get-occ? body) v-name))))))
      (lambda-opt ,(lambda (get-occ?) (lambda (params opt body) (lambda (v-name)
          (and (not (member v-name `(,@params ,opt))) ((get-occ? body) v-name))))))
      (lambda-var ,(lambda (get-occ?) (lambda (params body) (lambda (v-name)
          (and (not (eq? v-name params)) ((get-occ? body) v-name))))))
      (box-set ,(lambda (get-occ?) (lambda (var expr) (lambda (curry)
          ((get-occ? expr) curry)))))
      (else ,(lambda (get-occ?) (lambda (pe) (lambda (v-name)
          (error 'get-occ? (format "Invalid form: ~s" pe))))))))))
    (lambda (v-name pe)
      ((run pe) v-name))))


(define set-occ?
  (letrec ((run
    ((traverse curry-boolean-default-cases)
      `((set ,(lambda (set-occ?) (lambda (var expr) (lambda (v-name)
          (or (eq? v-name (var-name var)) ((set-occ? expr) v-name))))))
        (lambda-simple ,(lambda (set-occ?) (lambda (params body) (lambda (v-name)
             (and (not (member v-name params)) ((set-occ? body) v-name))))))
        (lambda-opt ,(lambda (set-occ?) (lambda (params opt body) (lambda (v-name)
            (and (not (member v-name `(,@params ,opt))) ((set-occ? body) v-name))))))
        (lambda-var ,(lambda (set-occ?) (lambda (params body) (lambda (v-name)
            (and (not (eq? v-name params)) ((set-occ? body) v-name))))))
        (else ,(lambda (set-occ?) (lambda (pe) (lambda (v-name)
          (error 'set-occ? (format "Invalid form: ~s" pe))))))))))
    (lambda (v-name pe)
      ((run pe) v-name))))


(define bound-occ?
  (letrec ((run
    ((traverse curry-boolean-default-cases)
      `((lambda-simple ,(lambda (bound-occ?) (lambda (params body) (lambda (v-name)
          (and (not (member v-name params)) (or (get-occ? v-name body) (set-occ? v-name body)))))))
        (lambda-opt ,(lambda (bound-occ?) (lambda (params opt body) (lambda (v-name)
          (and (not (member v-name `(,@params ,opt))) (or (get-occ? v-name body) (set-occ? v-name body)))))))
        (lambda-var ,(lambda (bound-occ?) (lambda (params body) (lambda (v-name)
          (and (not (eq? v-name params)) (or (get-occ? v-name body) (set-occ? v-name body)))))))
        (else ,(lambda (bound-occ?) (lambda (pe) (lambda (v-name)
          (error 'bound-occ? (format "Invalid form: ~s" pe))))))))))
    (lambda (v-name pe)
      ((run pe) v-name))))


(define box-var?
  (lambda (v-name pe)
    (and (get-occ? v-name pe) (set-occ? v-name pe) (bound-occ? v-name pe))))


(define do-box
  (letrec ((run
    ((traverse curry-default-cases)
      `((var ,(lambda (do-box) (lambda (name) (lambda (v-name)
          (if (eq? v-name name) `(box-get (var ,name)) `(var ,name))))))
        (set ,(lambda (do-box) (lambda (var expr) (lambda (v-name)
          (if (eq? v-name (var-name var))
              `(box-set ,var ,((do-box expr) v-name))
              `(set ,var ,((do-box expr) v-name)))))))
        (lambda-simple ,(lambda (do-box) (lambda (params body) (lambda (v-name)
          (if (member v-name params)
              `(lambda-simple ,params ,body)
              `(lambda-simple ,params ,((do-box body) v-name)))))))
        (lambda-opt ,(lambda (do-box) (lambda (params opt body) (lambda (v-name)
          (if (member v-name `(,@params ,opt))
              `(lambda-opt ,params ,opt ,body)
              `(lambda-opt ,params ,opt ,((do-box body) v-name)))))))
        (lambda-var ,(lambda (do-box) (lambda (params body) (lambda (v-name)
          (if (member v-name `(,params))
              `(lambda-var ,params ,body)
              `(lambda-var ,params ,((do-box body) v-name)))))))
        (else ,(lambda (do-box) (lambda (pe) (lambda (v-name) (error 'do-box (format "Invalid form: ~s" pe))))))))))
    (lambda (v-name pe)
      (let ((boxed-pe ((run pe) v-name)))
        (if (is? 'seq boxed-pe)
            `(seq ((set (var ,v-name) (box (var ,v-name))) ,@(seq-exprs boxed-pe)))
            `(seq ((set (var ,v-name) (box (var ,v-name))) ,boxed-pe)))))))


(define box-params
  (lambda (params body)
    (fold-right
      (lambda (v body)
        (if (box-var? v body)
            (do-box v body)
            body))
      body
      params)))


(define box-set
  ((traverse default-cases)
    `((lambda-simple ,(lambda (box-set) (lambda (params body)
       `(lambda-simple ,params ,(box-params params (box-set body))))))
      (lambda-opt ,(lambda (box-set) (lambda (params opt body)
        `(lambda-opt ,params ,opt ,(box-params `(,@params ,opt) (box-set body))))))
      (lambda-var ,(lambda (box-set) (lambda (params body)
            `(lambda-var ,params ,(box-params `(,params) (box-set body)))))))))


(define params->pvar-lst
  (letrec ((run
    (lambda (params pvar-lst count)
      (if (null? params)
          pvar-lst
          (run (cdr params) `(,@pvar-lst (pvar ,(car params) ,count))  (+ count 1))))))
    (lambda (params) (run params '() 0))))


(define deepen-bindings
  (lambda (env)
    (map
      (lambda (lex-v)
        (if (is? 'pvar lex-v)
            `(bvar ,(pvar-name lex-v) 0 ,(pvar-minor lex-v))
            `(bvar ,(bvar-name lex-v) ,(+ (bvar-major lex-v) 1) ,(bvar-minor lex-v))))
      env)))


(define bound?
  (lambda (v env)
    (ormap
      (lambda (lex-v)
        (if (eq? (cadr lex-v) v)
              lex-v
              #f))
      env)))


(define pe->lex-pe
  (letrec ((run
    ((traverse curry-default-cases)
      `((var ,(lambda (pe->lex-pe) (lambda (name) (lambda (env)
          (let ((lex-v (bound? name env))) (if lex-v lex-v `(fvar ,name)))))))
        (lambda-simple ,(lambda (pe->lex-pe) (lambda (params body) (lambda (env)
          `(lambda-simple ,params ,((pe->lex-pe body) (append (params->pvar-lst params) (deepen-bindings env))))))))
        (lambda-opt ,(lambda (pe->lex-pe) (lambda (params opt body) (lambda (env)
          `(lambda-opt ,params ,opt ,((pe->lex-pe body) (append (params->pvar-lst `(,@params ,opt)) (deepen-bindings env))))))))
        (lambda-var ,(lambda (pe->lex-pe) (lambda (params body) (lambda (env)
          `(lambda-var ,params ,((pe->lex-pe body) (append (params->pvar-lst `(,params)) (deepen-bindings env))))))))
        (box ,(lambda (pe->lex-pe) (lambda (var) (lambda (env) `(box ,((pe->lex-pe var) env))))))
        (box-get ,(lambda (pe->lex-pe) (lambda (var) (lambda (env) `(box-get ,((pe->lex-pe var) env))))))
        (box-set ,(lambda (pe->lex-pe) (lambda (var expr) (lambda (env) `(box-set ,((pe->lex-pe var) env) ,((pe->lex-pe expr) env))))))
        (else ,(lambda (pe->lex-pe) (lambda (pe) (lambda (env)
          (error 'pe->lex-pe (format "Invalid form: ~s" pe))))))))))
    (lambda (pe) ((run pe) '()))))


(define tc-map
  (lambda (exprs)
    (let ((reverse-exprs (reverse exprs)))
      `(,@(map annotate-tc (reverse (cdr reverse-exprs))) ,(handle-tc (car reverse-exprs))))))


(define handle-tc
    ((traverse default-cases)
      `((if3 ,(lambda (handle-tc) (lambda (test dit dif)
        `(if3 ,(annotate-tc test) ,(handle-tc dit) ,(handle-tc dif)))))
      (def ,(lambda (handle-tc) (lambda (var expr)
        `(def ,var ,(annotate-tc expr)))))
      (set ,(lambda (handle-tc) (lambda (var expr)
        `(set ,var ,(annotate-tc expr)))))
      (or ,(lambda (handle-tc) (lambda (exprs)
        `(or ,(tc-map exprs)))))
      (seq ,(lambda (handle-tc) (lambda (exprs)
        `(seq ,(tc-map exprs)))))
      (applic ,(lambda (handle-tc) (lambda (proc args)
        `(tc-applic ,(annotate-tc proc) ,(map annotate-tc args)))))
      (box-set ,(lambda (handle-tc) (lambda (var expr) `(box-set ,var ,(annotate-tc expr)))))
      (else ,(lambda (handle-tc) (lambda (pe)
        (error 'handle-tc (format "Invalid form: ~s" pe))))))))


(define annotate-tc
  ((traverse default-cases)
    `((lambda-simple ,(lambda (annotate-tc) (lambda (params body)
        `(lambda-simple ,params ,(handle-tc body)))))
    (lambda-opt ,(lambda (annotate-tc) (lambda (params opt body)
        `(lambda-opt ,params ,opt ,(handle-tc body)))))
    (lambda-var ,(lambda (annotate-tc) (lambda (params body)
        `(lambda-var ,params ,(handle-tc body)))))
    (else ,(lambda (handle-tc) (lambda (pe)
      (error 'annotate-tc (format "Invalid form: ~s" pe))))))))


;;;;;;;;;;;;;;;;;;;;;;;; Final Project  ;;;;;;;;;;;;;;;;;;;;;;;;

;;Auxiliary functions
(define acc-default-cases
        `((const ,(lambda (run) (lambda (c) '())))
        (var ,(lambda (run) (lambda (name) '())))
        (fvar ,(lambda (run) (lambda (name) '())))
        (pvar ,(lambda (run) (lambda (name minor) '())))
        (bvar ,(lambda (run) (lambda (name major minor) '())))
        (if3 ,(lambda (run) (lambda (test dit dif) `(,@(run test) ,@(run dit) ,@(run dif)))))
        (def ,(lambda (run) (lambda (var expr) `(,@(run var) ,@(run expr)))))
        (set ,(lambda (run) (lambda (var expr) `(,@(run var) ,@(run expr)))))
        (or ,(lambda (run) (lambda (exprs) (flat-map run exprs))))
        (seq ,(lambda (run) (lambda (exprs) (flat-map run exprs))))
        (applic ,(lambda (run) (lambda (proc args) `(,@(run proc) ,@(flat-map run args)))))
        (tc-applic ,(lambda (run) (lambda (proc args) `(,@(run proc) ,@(flat-map run args)))))
        (lambda-simple ,(lambda (run) (lambda (params body) (run body))))
        (lambda-opt ,(lambda (run) (lambda (params opt body) (run body))))
        (lambda-var ,(lambda (run) (lambda (params body) (run body))))
        (box ,(lambda (run) (lambda (var) (run var))))
        (box-get ,(lambda (run) (lambda (var) (run var))))
        (box-set ,(lambda (run) (lambda (var expr) `(,@(run var) ,@(run expr)))))
        (else ,(lambda (run) (lambda (pe) (error 'acc-default-cases (format "Invalid form: ~s" pe)))))))

(define file->string
    (lambda (in-file)
      (let ((in-port (open-input-file in-file)))
        (letrec ((run
          (lambda ()
            (let ((ch (read-char in-port)))
              (if (eof-object? ch)
                (begin
                  (close-input-port in-port)
                  '())
                (cons ch (run)))))))
          (list->string
            (run))))))

(define string->file
    (lambda (str out-file)
      (let* ((out-port (open-output-file out-file 'replace)))
             (display str out-port)
             (close-output-port out-port))))

(define list-sexprs
  (lambda (code-string)
    (letrec
      ((acc-sexprs
        (lambda (code-list)
          (<sexpr> code-list
            (lambda (e s) `(,e ,@(acc-sexprs s)))
            (lambda (w) `())))
        ))
        (acc-sexprs (string->list code-string)))))

(define parse-and-optimize
  (lambda (sexpr)
    (annotate-tc
      (pe->lex-pe
        (box-set
          (remove-applic-lambda-nil
            (eliminate-nested-defines
              (parse sexpr))))))))

(define (object->string x)
  (call-with-string-output-port
    (lambda (p) (put-datum p x))))

(define lgen
  (let* ((counter 0)
        (count  (lambda () (set! counter (+ 1 counter)) counter)))
  (lambda (name)
    (string-append "L_" name "_" (object->string (count))))))

(define flat-map
  (lambda (proc list)
    (fold-left (lambda (a x) (append a (proc x))) '() list)))

(define make_primitive_closure
  (lambda (label offset)
    (string-append
      (call "MAKE_SOB_CLOSURE" "555555" (string-append "LABEL(" label ")"))
      "MOV(IND(" (object->string offset) ") , R0);\n"
    )))

;;Creating the constant table

(define list-constants
  (letrec
    ((decompose-const
      (lambda (c)
        (cond ((null? c) c)
              ((symbol? c)  `(,(symbol->string c) ,c))
              ((pair? c) `(,@(decompose-const (car c)) ,@(decompose-const (cdr c)) ,c))
              ((vector? c) `(,@(flat-map decompose-const (vector->list c)) ,c))
              (else `(,c)))))

    (find-constants
      ((traverse acc-default-cases)
        `((const ,(lambda (run) (lambda (c)
            (decompose-const c))))
          (const ,(lambda (run) (lambda (c)
              (decompose-const c)))))
      ))

    (remove-duplicates
      (lambda (lst)
        (if (null? lst)
            lst
            (cons (car lst) (remove-duplicates (remove  (car lst) (cdr lst)))))))

    (create-objects
      (lambda (addr acc lst)
        (if (null? lst)
            (begin (set! fvars-offset addr) acc)
            (let ((first (car lst))
                  (rest (cdr lst)))
              (cond ((eq? (void) first)
                      (create-objects (+ addr 1) `(,@acc (,addr ,first T_VOID)) rest))
                    ((null? first)
                      (create-objects (+ addr 1) `(,@acc (,addr ,first T_NIL)) rest))
                    ((boolean? first)
                      (create-objects (+ addr 2) `(,@acc (,addr ,first T_BOOL ,(if first 1 0))) rest))
                    ((char? first)
                      (create-objects (+ addr 2) `(,@acc (,addr ,first T_CHAR ,first)) rest))
                    ((integer? first)
                      (create-objects (+ addr 2) `(,@acc (,addr ,first T_INTEGER ,first)) rest))
                    ((number? first)
                      (create-objects (+ addr 3) `(,@acc (,addr ,first T_FRACT ,(numerator first) ,(denominator first))) rest))
                    ((string? first)
                      (let* ((chars (string->list first))
                            (len (length chars)))
                        (create-objects (+ addr 2 len) `(,@acc (,addr ,first T_STRING ,len ,@chars)) rest)))
                    ((symbol? first)
                      (let ((rep_string_offset (car (lookup (symbol->string first) acc))))
                      (add_symbol rep_string_offset)
                      (create-objects (+ addr 2) `(,@acc (,addr ,first T_SYMBOL ,rep_string_offset)) rest)))
                    ((pair? first)
                      (create-objects (+ addr 3) `(,@acc (,addr ,first T_PAIR ,(car (lookup (car first) acc)) ,(car (lookup (cdr first) acc)))) rest))
                    ((vector? first)
                      (let* ((entries (map (lambda (x) (car (lookup x acc))) (vector->list first)))
                            (len (length entries)))
                            (create-objects (+ addr 2 len) `(,@acc (,addr ,first T_VECTOR ,len ,@entries)) rest))))
                  )))))
  (lambda (pes)
    (let* ((primitive_consts `(,(void) () #t #f 0 1 #\x0))
          (all_consts (fold-left
                          (lambda (consts pe)
                              `(,@consts ,@(find-constants pe)))
                          primitive_consts
                          pes
                      ))
          (no_dups_consts (remove-duplicates all_consts))
          (const_objects (create-objects 2 '() no_dups_consts)))
      (cons const_objects no_dups_consts)))))

(define lookup
  (lambda (tag lst)
    (cond ((null? lst) '())
          ((equal? (cadar lst) tag) (car lst))
          (else (lookup tag (cdr lst))))))

(define type->string
  (lambda (const)
    (cond ((eq? (void) const) "VOID")
          ((null? const) "NIL")
          ((boolean? const) "BOOL")
          ((char? const) "CHAR")
          ((integer? const) "INTEGER")
          ((number? const) "FRACT")
          ((string? const) "STRING")
          ((symbol? const) "SYMBOL")
          ((pair? const) "PAIR")
          ((vector? const) "VECTOR"))))
(define push_args
  (lambda (const_objects const)
    (letrec ((lookup (lambda (c_table c)
                          (letrec ((find (lambda (table c obj)
                                              (if (equal? (cadr obj) c)
                                                  obj
                                                  (find (cdr table) c (car table))))
                                  ))
                                  (find (cdr c_table) c (car c_table))))))
  (cond ((eq? (void) const)
          "")
        ((null? const)
          "")
        ((boolean? const)
          (string-append
            "\nPUSH("
            (if const "1" "0")
            ")"
          ))
        ((char? const)
          (string-append
          "\nPUSH("
          (object->string (char->integer const))
          ")"))
        ((integer? const)
          (string-append
          "\nPUSH("
          (object->string const)
          ")"))
        ((number? const)
          (string-append
          "\nPUSH(" (object->string (denominator const)) ")"
          "\nPUSH(" (object->string (numerator const)) ")"
          ))
        ((string? const)
          (let ((char_lst (string->list const)))
            (fold-left
              (lambda (push_args_str char)
                (string-append
                  "\nPUSH(IMM(" (object->string (char->integer char)) "))"
                  push_args_str
                ))
              (string-append "\nPUSH(IMM(" (object->string (length char_lst)) "));")
              (reverse char_lst))
              ))
        ((symbol? const)
          (string-append
            "\nPUSH(" (object->string (cadddr (lookup const_objects const)))  ");"
          ))
        ((pair? const)
          (string-append "\n\n"
            "\nPUSH(" (object->string (car (lookup const_objects (cdr const))))  ");"
            "\nPUSH(" (object->string (car (lookup const_objects (car const))))  ");" "\n\n\n\n"))
        ((vector? const)
          (let ((obj_lst (vector->list const)))
            (fold-left
              (lambda (push_args_str obj)
                (string-append
                  "\nPUSH(IMM(" (object->string (car (lookup const_objects obj))) "))"
                  push_args_str
                ))
              (string-append "\nPUSH(IMM(" (object->string (length obj_lst)) "));")
              (reverse obj_lst))
              ))
        ))))

(define drop_args
  (lambda (const)
    (let ((num_of_args
      (cond ((eq? (void) const) 0)
            ((null? const) 0)
            ((boolean? const) 1)
            ((char? const) 1)
            ((integer? const) 1)
            ((number? const) 2);;fract
            ((string? const) (+ 1 (length (string->list const))))
            ((symbol? const) 1)
            ((pair? const) 2)
            ((vector? const) (+ 1 (length (vector->list const)))))))
      (if (zero? num_of_args)
          ""
          (string-append "DROP(" (object->string num_of_args) ");\n")))))

(define allocate_constant_table
  (lambda (lst)
    (let* ((constants_objects (car lst))
            (constants_list (cdr lst))
            (ret (fold-left
                    (lambda (constant_table_string const)
                        (string-append
                          constant_table_string
                          (push_args constants_objects const)
                          "\nCALL(MAKE_SOB_" (type->string const) ");\n"
                          (drop_args const)
                          ))
                      "\nPUSH(IMM(1));\nCALL(MALLOC);\nDROP(1);\nMOV(IND(R0) , -1);\n"
                      constants_list
            )))
            ret)))

;;Creating the fvars table
(define list-fvars
  (letrec
    ((f-primitives  '(
                      apply
                      <
                      =
                      >
                      +
                      -
                      /
                      *
                      boolean?
                      car
                      cdr
                      set-car!
                      set-cdr!
                      cons
                      list
                      append
                      pair?
                      char->integer
                      char?
                      eq?
                      integer?
                      number?
                      integer->char
                      map
                      not
                      null?
                      procedure?
                      rational?
                      denominator
                      numerator
                      remainder
                      make-string
                      string-length
                      string-ref
                      string-set!
                      string->symbol
                      string?
                      symbol?
                      symbol->string
                      vector
                      make-vector
                      vector-length
                      vector-ref
                      vector-set!
                      vector?
                      zero?
                      <
                    ))
    (find-fvars
      ((traverse acc-default-cases)
        `((fvar ,(lambda (run) (lambda (f)
          (list f)))))))

    (remove-duplicates
      (lambda (lst)
        (if (null? lst)
            lst
            (cons (car lst) (remove-duplicates (remove  (car lst) (cdr lst)))))))

    (create-objects
      (lambda (addr acc lst)
        (if (null? lst)
            acc
            (let ((first (car lst))
                  (rest (cdr lst)))
                  (create-objects (+ addr 1) `(,@acc (,addr ,first)) rest))
        ))))

    (lambda (pes)
      (let* ((all_fvars (fold-left
                            (lambda (fvars pe)
                                `(,@fvars ,@(find-fvars pe)))
                            f-primitives
                            pes
                        ))
            (no_dups_fvars (remove-duplicates all_fvars))
            (fvars_objects (create-objects fvars-offset '() no_dups_fvars)))
        (cons fvars_objects no_dups_fvars)))))

(define allocate_fvar_table
  (lambda (lst)
    (let ((ret (fold-left
                  (lambda (fvar_table_string fvar)
                      (string-append
                        fvar_table_string
                        "\nPUSH(IMM(1));"
                        "\nCALL(MALLOC);"
                        "\nDROP(1);"
                        "\nMOV(IND(R0) , IMM(-1));\n"
                        ))
                    ""
                    lst
          )))
          ret)))

(define fvars-offset -1)

;;Creating the symbols table
;;Creating the symbols table
(define allocate_symbol_table
  (lambda (lst)
    (let ((ret
            (fold-left
                      (lambda (symbol_table_string string_offset)
                          (string-append
                            symbol_table_string
                            "MOV(R1 , R0);\n"
                            "INCR(R1);\n"
                            "\nPUSH(IMM(2));\n"
                            "CALL(MALLOC);\n"
                            "DROP(1);\n"
                            "MOV(INDD(R0 , 0) , IMM(" (number->string string_offset) "));\n"
                            "MOV(INDD(R0 , 1) , IMM(0));\n"
                            "MOV(IND(R1) , R0);\n"
                            ))
                        "\nMOV(R0 , IMM(0));\n"
                        lst)))
                ret)))

(define symbols '())

(define add_symbol
  (lambda (str_offset)
      (cond ((null? symbols) (set! symbols (list str_offset)))
            (else (set! symbols (append symbols (list str_offset)))))
      ))

;;Code generation

(define code-gen
  (lambda (c_table f_table sym_table)
    (letrec ((run
      ((traverse curry-default-cases)
      `((const ,(lambda (code-gen) (lambda (c) (lambda (major-acc)
          (string-append
            "MOV(R0 , IMM(" (number->string (lookup-const c_table c)) ")); /* " (object->string `(const ,c)) "*/\n" )))))

        (fvar ,(lambda (code-gen) (lambda (name) (lambda (major-acc)
          (string-append
            "\nMOV(R0 , IND(" (number->string (lookup-fvar f_table name)) ")); /* " (object->string `(fvar ,name)) "*/\n" )))))

        (pvar ,(lambda (code-gen) (lambda (name minor) (lambda (major-acc)
          (string-append
            "MOV(R0 , FPARG(" (number->string (+ minor 2)) ")); /* " (object->string `(pvar ,name ,minor)) "*/\n" )))))

        (bvar ,(lambda (code-gen) (lambda (name major minor) (lambda (major-acc)
          (string-append
            "MOV(R0 , FPARG(0)); // env\n"
            "MOV(R0 , INDD(R0 , " (number->string major) "));\n"
            "MOV(R0 , INDD(R0 , " (number->string minor) ")); /* " (object->string `(bvar ,name ,major ,minor)) "*/\n")))))

        (if3 ,(lambda (code-gen) (lambda (test dit dif) (lambda (major-acc)
          (let* ((l-else (lgen "if3_else"))
                 (l-exit (lgen "if3_exit")))
            (string-append
              "/*if*/\n"
              "/*if-test */"
              ((code-gen test) major-acc)
              "CMP(R0 , IMM(SOB_FALSE));\n"
              "JUMP_EQ(" l-else ");\n"
              "/*if-dit*/"
              ((code-gen dit) major-acc)
              "JUMP(" l-exit ");\n"
              l-else":\n"
              "/*if-dif*/"
              ((code-gen dif) major-acc)
              l-exit":\n"))))))

        (def ,(lambda (code-gen) (lambda (var expr) (lambda (major-acc)
          (string-append
            "/* " (object->string `(define ,(cadr var))) "*/\n"
            ((code-gen expr) major-acc)
            "\nMOV(IND(" (number->string (lookup-fvar f_table (cadr var))) ") , R0);\n"
            "MOV(R0 , IMM(SOB_VOID));\n")))))

        (set ,(lambda (code-gen) (lambda (var expr) (lambda (major-acc)
          (string-append
            "\n/*set*/\n"
            "/*set-expr*/\n"
            ((code-gen expr) major-acc)
            (cond
              ((is? 'fvar var)
                (string-append
                  "/*set-fvar " (object->string (cadr var)) "*/"
                  ((code-gen expr) major-acc)
                  "MOV(IND(" (number->string (lookup-fvar f_table (cadr var))) ") , R0);\n"
                  "MOV(R0 , IMM(SOB_VOID));\n"))
              ((is? 'bvar var)
                (string-append
                  "/*set-bvar " (object->string (cadr var)) "*/"
                  "MOV(R1 , FPARG(0));\n"
                  "MOV(R1 , INDD(R1 , " (number->string (bvar-major var)) "));\n"
                  "MOV(INDD(R1 , " (number->string (bvar-minor var)) ") , R0);\n"))
              ((is? 'pvar var)
                (string-append
                "/*set-pvar " (object->string (cadr var)) "*/\n"
                "MOV(FPARG(" (number->string (+ (pvar-minor var) 2)) ") , R0);\n" ))
              (else (error 'code-gen-set (format "Invalid form: ~s" pe))))
            "MOV(R0 , IMM(SOB_VOID));\n")))))

        (or ,(lambda (code-gen) (lambda (exprs) (lambda (major-acc)
          (if (null? exprs)
              "/* or - no arguments */\nMAKE_SOB_BOOL(0) */;\n"
              (let ((l-exit (lgen "or_exit")))
                (string-append
                  "/*or*/\n"
                  (with (map (lambda (e)
                    (string-append
                      "/* or-operand */\n"
                      ((code-gen e) major-acc)
                      ;"SHOW(\" in or: "  (number->string (lookup-const c_table #f)) " \", R0);\n"
                      ;"INFO;\n"
                      ;"CMP(INDD(R0 , 0) , IMM(T_BOOL));\n"
                      ;"JUMP_NE(" l-exit ");\n"
                      ;"CMP(INDD(R0 , 1) , IMM(0));\n"
                      ;"JUMP_NE(" l-exit ");\n"
                      "CMP(R0 , IMM(" (number->string (lookup-const c_table #f)) "));\n"
                      "JUMP_NE(" l-exit ");\n"))
                          exprs)
                    string-append)
                                l-exit ":\n")))))))

        (seq ,(lambda (code-gen) (lambda (exprs) (lambda (major-acc)
          (string-append "/*sequence*/\n" (with (curry-map code-gen major-acc exprs) string-append))))))

        (applic ,(lambda (code-gen) (lambda (proc args) (lambda (major-acc)
          (string-append
            "/*applic*/\n"
            (with (map (lambda (arg) (string-append ((code-gen arg) major-acc) "PUSH(R0); /*applic- push argument" (object->string arg) "*/\n")) (reverse args)) string-append)
            "PUSH(IMM(" (number->string (length args)) "));\n"
            "/*applic-proc*/"
            ((code-gen proc) major-acc)
            "CMP(INDD(R0 ,0) ,IMM(T_CLOSURE));\n"
            "JUMP_NE(L_error_cannot_apply_non_clos);\n"
            "/*applic-call*/\n"
            (calla "INDD(R0 , 2)" "INDD(R0 , 1)")
            "POP(R1);  /*applic- POP the number of arguments that were pushed into R1*/\n"
            "DROP(R1); /*applic- DROP the number of arguments that were pushed*/\n")))))

        (tc-applic ,(lambda (code-gen) (lambda (proc args) (lambda (major-acc)
          (let ((l-overwrite-old-frame (lgen "tc_applic_overwrite_old_frame")))
            (string-append
              "\n/*tc-applic*/\n"
              (with (map (lambda (arg) (string-append ((code-gen arg) major-acc) "PUSH(R0); /*tc-applic- push argument" (object->string arg) "*/\n")) (reverse args)) string-append)
              "PUSH(IMM(" (number->string (length args)) "));\n"
              "\n/*tc-applic-proc*/"
              ((code-gen proc) major-acc)
              "CMP(INDD(R0 ,0) ,IMM(T_CLOSURE));\n"
              "JUMP_NE(L_error_cannot_apply_non_clos);\n"
              "PUSH(INDD(R0 , 1));\n"
              "PUSH(FPARG(-1));\n"
              "MOV(R1 , FPARG(-2));\n"
              "MOV(R3 , IMM(" (number->string (+ (length args) 3)) "));\n"
              "DROP(R3);\n"
              "MOV(R2 , SP);\n"
              "MOV(SP , FP);\n"
              "DROP(FPARG(1) + 4);\n"
              "/*tc-applic - overwrite old frame*/\n"
              l-overwrite-old-frame ": \n"
              "PUSH(STACK(R2));\n"
              "INCR(R2);\n"
              "DECR(R3);\n"
              "CMP(R3 , 0);\n"
              "JUMP_GT(" l-overwrite-old-frame ");\n"
              "MOV(FP , R1);\n"
              "JUMPA(INDD(R0 , 2)); /*tc-applic- JUMP to function (CALL)*/\n"
              ))))))

        (lambda-simple ,(lambda (code-gen) (lambda (params body) (lambda (major-acc)
          (let* ((l-copy-prev-env-loop (lgen "lambda_simple_copy_prev_env_loop"))
                 (l-copy-prev-env-exit (lgen "lambda_simple_copy_prev_env_exit"))
                 (l-copy-params-loop (lgen "lambda_simple_copy_params_loop"))
                 (l-copy-params-exit (lgen "lambda_simple_copy_params_exit"))
                 (l-clos-body (lgen "lambda_simple_clos_body"))
                 (l-clos-exit (lgen "lambda_simple_clos_exit")))
            (string-append
              ;; allocate space for new env.
              "/*lambda simple" (object->string params) "*/\n"
              "/*allocate space for new env.*/\n"
              "MOV(R1 , FPARG(0)); /*R1 <- old env*/\n" ; R1 <- old env
              (call "MALLOC" (string-append "IMM(" (number->string (+ major-acc 1)) ")" ))
              "MOV(R2 , R0); /*R2 <- new env*/\n" ; R2 <- new env
              ;; copy previous env to new env.
              "/*copy previous env to new env.*/\n"
              "MOV(R3 , IMM(0)); /*R3 == i <- 0*/\n" ; R3 == i <- 0
              "MOV(R4 , IMM(1)); /*R4 == j <- 1*/\n" ; R4 == j <- 1
              l-copy-prev-env-loop ":\n"
              "CMP(R3 , IMM(" (number->string major-acc) "));\n"
              "JUMP_GE(" l-copy-prev-env-exit ");\n"
              "MOV(INDD(R2 , R4) , INDD(R1 , R3)); /*R2[j] <- R1[i]*/\n" ; R2[j] <- R1[i]
              "INCR(R3);\n"
              "INCR(R4);\n"
              "JUMP(" l-copy-prev-env-loop ");\n"
              l-copy-prev-env-exit ":\n"
              ;; allocate space for params (bound 0) in new env.
              "/*allocate space for params (bound 0) in new env.*/\n"
              "MOV(R3 , FPARG(1)); /*R3 <- number of arguments*/\n" ; R3 <- number of arguments
              (call "MALLOC" "R3")
              "MOV(INDD(R2 , 0) , R0);\n"
              ;; copy params to new env.
              "/*copy params to new env.*/\n"
              "MOV(R4 , IMM(0)); /*R4 == i <- 0*/\n" ; R4 == i <- 0
              "MOV(R5 , IMM(2)); /*R5 == j <- 2*/\n" ; R5 == j <- 2
              "MOV(R6  , INDD(R2 , 0)); /*R6<- R2[0]*/\n" ; R6<- R2[0]
              l-copy-params-loop ":\n"
              "CMP(R4 , R3);\n"
              "JUMP_GE(" l-copy-params-exit ");\n"
              "MOV(INDD(R6 , R4) , FPARG(R5)); /*R6[R4] <- FPARG(5) == R2[0][i] <- FPARG[j]*/\n" ; R6[R4] <- FPARG(5) == R2[0][i] <- FPARG[j]
              "INCR(R4);\n"
              "INCR(R5);\n"
              "JUMP(" l-copy-params-loop ");\n"
              l-copy-params-exit ":\n"
              ;; create closure
              "/*create closure*/\n"
              (call "MALLOC" "IMM(3)")
              "MOV(INDD(R0 , 0) , IMM(T_CLOSURE));\n"
              "MOV(INDD(R0 , 1) , R2);\n"
              "MOV(INDD(R0 , 2) , LABEL(" l-clos-body "));\n"
              "JUMP(" l-clos-exit ");\n"
              ;; define the lambdas body
              "/*define the lambdas body*/\n"
              l-clos-body ":\n"
              "PUSH(FP);\n"
              "MOV(FP , SP);\n"

              "CMP(FPARG(1) , IMM(" (number->string (length params)) "));\n"

              "JUMP_NE(L_error_args_count);\n"
              ((code-gen body) (+ major-acc 1))
              "POP(FP);\n"
              "RETURN;\n"
              l-clos-exit ":\n\n"))))))

        (lambda-opt ,(lambda (code-gen) (lambda (params opt body) (lambda (major-acc)
          (let* ((l-copy-prev-env-loop (lgen "lambda_opt_copy_prev_env_loop"))
                 (l-copy-prev-env-exit (lgen "lambda_opt_copy_prev_env_exit"))
                 (l-copy-params-loop (lgen "lambda_opt_copy_params_loop"))
                 (l-copy-params-exit (lgen "lambda_opt_copy_params_exit"))
                 (l-clos-body (lgen "lambda_opt_clos_body"))
                 (l-clos-exit (lgen "lambda_opt_clos_exit"))
                 (l-adjust-stack-up (lgen "lambda_opt_adjust_stack_up"))
                 (l-adjust-stack-up-loop (lgen "lambda_opt_adjust_stack_up_loop"))
                 (l-adjust-stack-down-loop (lgen "lambda_opt_adjust_stack_down_loop"))
                 (l-build-opt-list-loop (lgen "lambda_opt_build_opt_list_loop"))
                 (l-start-body (lgen "lambda_opt_start_body")))
            (string-append
              ;; allocate space for new env.
              "MOV(R1 , FPARG(0));\n" ; R1 <- old env
              (call "MALLOC" (string-append "IMM(" (number->string (+ major-acc 1)) ")" ))
              "MOV(R2 , R0);\n" ; R2 <- new env
              ;; copy previous env to new env.
              "MOV(R3 , IMM(0));\n" ; R3 == i <- 0
              "MOV(R4 , IMM(1));\n" ; R4 == j <- 1
              l-copy-prev-env-loop ":\n"
              "CMP(R3 , IMM(" (number->string major-acc) "));\n"
              "JUMP_GE(" l-copy-prev-env-exit ");\n"
              "MOV(INDD(R2 , R4) , INDD(R1 , R3));\n" ; R2[j] <- R1[i]
              "INCR(R3);\n"
              "INCR(R4);\n"
              "JUMP(" l-copy-prev-env-loop ");\n"
              l-copy-prev-env-exit ":\n"
              ;; allocate space for params (bound 0) in new env.
              "MOV(R3 , FPARG(1));\n" ; R3 <- number of arguments
              (call "MALLOC" "R3")
              "MOV(INDD(R2 , 0) , R0);\n"
              ;; copy params to new env.
              "MOV(R4 , IMM(0));\n" ; R4 == i <- 0
              "MOV(R5 , IMM(2));\n" ; R5 == j <- 2
              "MOV(R6  , INDD(R2 , 0)); /*R6<- R2[0]*/\n" ; R6<- R2[0]
              l-copy-params-loop ":\n"
              "CMP(R4 , R3);\n"
              "JUMP_GE(" l-copy-params-exit ");\n"
              "MOV(INDD(R6 , R4) , FPARG(R5));\n" ; R6[R4] <- FPARG(5) == R2[0][i] <- FPARG[j]
              "INCR(R4);\n"
              "INCR(R5);\n"
              "JUMP(" l-copy-params-loop ");\n"
              l-copy-params-exit ":\n"
              ;; create closure
              (call "MALLOC" "IMM(3)")
              "MOV(INDD(R0 , 0) , IMM(T_CLOSURE));\n"
              "MOV(INDD(R0 , 1) , R2);\n"
              "MOV(INDD(R0 , 2) , LABEL(" l-clos-body "));\n"
              "JUMP(" l-clos-exit ");\n"
              ;; define the lambdas body
              l-clos-body ":\n"
              "PUSH(FP);\n"
              "MOV(FP , SP);\n"
              "PUSH(R1);\n"
              "PUSH(R2);\n"
              "PUSH(R3);\n"
              "PUSH(R4);\n"
              "PUSH(R5);\n"

              "MOV(R3,IMM(SOB_NIL));\n"

              "MOV(R1, FPARG(1));\n"
              "MOV(STACK(FP - 4) , IMM(" (number->string (+ (length params) 1)) "));\n"
              "CMP(R1 , IMM(" (number->string (length params)) "));\n"
              "JUMP_LT(L_error_lambda_opt_insufficient_args_count);\n"
              "JUMP_EQ(" l-adjust-stack-up ");\n"

              "MOV(R5 , IMM(R1));\n"
              "MOV(R2 , SP);\n"
              "SUB(R2 , IMM(9));\n"
              "SUB(R2 , IMM(R5));\n"

              "SUB(R1 , IMM(" (number->string (length params)) "));\n" ; number of optional arguments
              l-build-opt-list-loop ":\n"

              "MOV(R4 , STACK(R2));\n"
              "PUSH(R3);\n"
              "PUSH(R4);\n"
              "CALL(MAKE_SOB_PAIR);\n"
              "DROP(2);\n"
              "MOV(R3 , R0);\n"

              "INCR(R2);\n"
              "DECR(R1);\n"
              "CMP(R1, 0);\n"
              "JUMP_GT(" l-build-opt-list-loop ");\n"

              "MOV(R1 , SP)\n"
              "DROP(R5);\n"
              "DROP(9);\n"

              "PUSH(R3);\n"

              l-adjust-stack-down-loop ":\n"
              "PUSH(STACK(R2));\n"
              "INCR(R2);\n"
              "CMP(R2, R1);\n"
              "JUMP_LT(" l-adjust-stack-down-loop ");\n"

              "MOV(FP , SP);\n"
              "SUB(FP , 5);\n"

              "JUMP(" l-start-body ");\n"
              l-adjust-stack-up ":\n"
              "MOV(R2, FP);\n"
              "INCR(FP);\n"
              "INCR(SP);\n"
              "ADD(R1, 4);\n"
              l-adjust-stack-up-loop ":\n"
              "MOV(STACK(R2), STACK(R2 - 1));\n"
              "DECR(R2);\n"
              "DECR(R1);\n"
              "CMP(R1 , 0);\n"
              "JUMP_GT(" l-adjust-stack-up-loop ");\n"
              "MOV(STACK(R2), R3);\n"
              l-start-body ":\n"
              ((code-gen body) (+ major-acc 1))

              "POP(R5);\n"
              "POP(R4);\n"
              "POP(R3);\n"
              "POP(R2);\n"
              "POP(R1);\n"
              "POP(FP);\n"
              "RETURN;\n"
              l-clos-exit ":\n"))))))

        (lambda-var ,(lambda (code-gen) (lambda (params body) (lambda (major-acc)
          (let* ((l-copy-prev-env-loop (lgen "lambda_var_copy_prev_env_loop"))
                 (l-copy-prev-env-exit (lgen "lambda_var_copy_prev_env_exit"))
                 (l-copy-params-loop (lgen "lambda_var_copy_params_loop"))
                 (l-copy-params-exit (lgen "lambda_var_copy_params_exit"))
                 (l-clos-body (lgen "lambda_var_clos_body"))
                 (l-clos-exit (lgen "lambda_var_clos_exit"))
                 (l-adjust-stack-loop (lgen "lambda_var_adjust_stack_loop"))
                 (l-adjust-stack-exit (lgen "lambda_var_adjust_stack_exit"))
                 (l-adjust-stack-up (lgen "lambda_var_adjust_stack_up"))
                 (l-adjust-stack-up-loop (lgen "lambda_var_adjust_stack_up_loop"))
                 (l-adjust-stack-down-loop (lgen "lambda_var_adjust_stack_down_loop"))
                 (l-build-var-list-loop (lgen "lambda_var_build_var_list_loop"))
                 (l-start-body (lgen "lambda_var_start_body")))
            (string-append
              ;; allocate space for new env.
              "MOV(R1 , FPARG(0));\n" ; R1 <- old env
              (call "MALLOC" (string-append "IMM(" (number->string (+ major-acc 1)) ")" ))
              "MOV(R2 , R0);\n" ; R2 <- new env
              ;; copy previous env to new env.
              "MOV(R3 , IMM(0));\n" ; R3 == i <- 0
              "MOV(R4 , IMM(1));\n" ; R4 == j <- 1
              l-copy-prev-env-loop ":\n"
              "CMP(R3 , IMM(" (number->string major-acc) "));\n"
              "JUMP_GE(" l-copy-prev-env-exit ");\n"
              "MOV(INDD(R2 , R4) , INDD(R1 , R3));\n" ; R2[j] <- R1[i]
              "INCR(R3);\n"
              "INCR(R4);\n"
              "JUMP(" l-copy-prev-env-loop ");\n"
              l-copy-prev-env-exit ":\n"
              ;; allocate space for params (bound 0) in new env.
              "MOV(R3 , FPARG(1));\n" ; R3 <- number of arguments
              (call "MALLOC" "R3")
              "MOV(INDD(R2 , 0) , R0);\n"
              ;; copy params to new env.
              "MOV(R4 , IMM(0));\n" ; R4 == i <- 0
              "MOV(R5 , IMM(2));\n" ; R5 == j <- 2
              "MOV(R6  , INDD(R2 , 0)); /*R6<- R2[0]*/\n" ; R6<- R2[0]
              l-copy-params-loop ":\n"
              "CMP(R4 , R3);\n"
              "JUMP_GE(" l-copy-params-exit ");\n"
              "MOV(INDD(R6 , R4) , FPARG(R5));\n" ; R6[R4] <- FPARG(5) == R2[0][i] <- FPARG[j]
              "INCR(R4);\n"
              "INCR(R5);\n"
              "JUMP(" l-copy-params-loop ");\n"
              l-copy-params-exit ":\n"
              ;; create closure
              (call "MALLOC" "IMM(3)")
              "MOV(INDD(R0 , 0) , IMM(T_CLOSURE));\n"
              "MOV(INDD(R0 , 1) , R2);\n"
              "MOV(INDD(R0 , 2) , LABEL(" l-clos-body "));\n"
              "JUMP(" l-clos-exit ");\n"
              ;; define the lambdas body
              l-clos-body ":\n"
              "PUSH(FP);\n"
              "MOV(FP , SP);\n"
              "PUSH(R1);\n"
              "PUSH(R2);\n"
              "PUSH(R3);\n"
              "PUSH(R4);\n"
              "PUSH(R5);\n"

              ;;the adjust-stack code
              "MOV(R3 , IMM(SOB_NIL));\n"
              "MOV(R1, FPARG(1));\n"
              "MOV(STACK(FP - 4) , IMM(1));\n"
              "CMP(R1 , 0);\n"
              "JUMP_EQ(" l-adjust-stack-up ");\n"

              "MOV(R5 , IMM(R1));\n"
              "MOV(R2, SP);\n"
              "SUB(R2 , IMM(9));\n"
              "SUB(R2 , IMM(R5));\n"

              l-build-var-list-loop ":\n"

              "MOV(R4 , STACK(R2));\n"
              "PUSH(R3);\n"
              "PUSH(R4);\n"
              "CALL(MAKE_SOB_PAIR);\n"
              "DROP(2);\n"
              "MOV(R3 , R0);\n"
              "INCR(R2);\n"
              "DECR(R1);\n"
              "CMP(R1, IMM(0));\n"
              "JUMP_GT(" l-build-var-list-loop ");\n"

              "MOV(R1 , SP);\n"
              "DROP(9);\n"
              "DROP(R5);\n"
              "PUSH(R3);\n"

              l-adjust-stack-down-loop ":\n"
              "PUSH(STACK(R2));\n"
              "INCR(R2);\n"
              "CMP(R2, R1);\n"
              "JUMP_LT(" l-adjust-stack-down-loop ");\n"

              "MOV(FP , SP);\n"
              "SUB(FP , IMM(5));\n"

              "JUMP(" l-start-body ");\n"
              l-adjust-stack-up ":\n"
              "MOV(R2, FP);\n"
              "INCR(FP);\n"
              "INCR(SP);\n"
              "ADD(R1, 4);\n"
              l-adjust-stack-up-loop ":\n"
              "MOV(STACK(R2), STACK(R2 - 1));\n"
              "DECR(R2);\n"
              "DECR(R1);\n"
              "CMP(R1 , 0);\n"
              "JUMP_GT(" l-adjust-stack-up-loop ");\n"
              "MOV(STACK(R2), R3);\n"
              ;;end of the adjust-stack code
              l-start-body ":\n"
              ((code-gen body) (+ major-acc 1))

              "POP(R5);\n"
              "POP(R4);\n"
              "POP(R3);\n"
              "POP(R2);\n"
              "POP(R1);\n"
              "POP(FP);\n"
              "RETURN;\n"
              l-clos-exit ":\n"))))))

        (box ,(lambda (code-gen) (lambda (var) (lambda (major-acc)
          (string-append
            (call "MALLOC" "IMM(1)" )
            "MOV(R1 , R0);\n"
            ((code-gen var) major-acc)
            "MOV(IND(R1) , R0);\n"
            (cond
              ((is? 'bvar var)
                (string-append
                  "MOV(R2 , FPARG(0));\n"
                  "MOV(R2 , INDD(R2 , " (number->string (bvar-major var)) "));\n"
                  "MOV(INDD(R2 , " (number->string (bvar-minor var)) ") , R1);\n"))
              ((is? 'pvar var)
                (string-append
                "MOV(FPARG(" (number->string (+ (pvar-minor var) 2)) ") , R1);\n"))
            (else (error 'code-gen (format "Invalid form: ~s" pe))))
          "MOV(R0 , IMM(SOB_VOID));\n")))))

        (box-get ,(lambda (code-gen) (lambda (var) (lambda (major-acc)
          (string-append
            ((code-gen var) major-acc)
            "MOV(R0 , IND(R0));\n")))))

        (box-set ,(lambda (code-gen) (lambda (var expr) (lambda (major-acc)
          (string-append
            ((code-gen expr) major-acc)
            "MOV(R1 , R0);\n"
            ((code-gen var) major-acc)
            "MOV(IND(R0) , R1);\n"
            "MOV(R0 , IMM(SOB_VOID));\n")))))

        (else ,(lambda (code-gen) (lambda (pe) (lambda (major-acc)
         (error 'code-gen (format "Invalid form: ~s" pe))))))))))
    (lambda (pe) ((run pe) 1)))))

(define call
  (lambda (proc . args)
    (string-append
      (fold-right (lambda (x a) (string-append a "PUSH(" x ");\n" )) "" args)
      "CALL(" proc ");\n"
      "DROP(" (object->string (length args))  ");\n")))

(define calla
  (lambda (addr . args)
    (string-append
      (fold-right (lambda (x a) (string-append a "PUSH(" x ");\n" )) "" args)
      "CALLA(" addr ");\n"
      "DROP(" (object->string (length args))  ");\n")))

(define lookup-const
  (lambda (c_table c)
    (letrec ((find (lambda (table c obj)
                        (if (equal? (cadr obj) c)
                            (car obj)
                            (find (cdr table) c (car table))))
            ))
            (find (cdr c_table) c (car c_table)))
  ))

(define lookup-fvar
  (lambda (f_table f)
    (letrec ((find (lambda (table f obj)
                        (if (not (null? obj))
                              (cond
                                ((equal? (cadr obj) f) (car obj))
                                ((not (null? table)) (find (cdr table) f (car table)))
                                (else (error 'lookup-fvar (format "fvar ~s is not defined" (cadr f)))))))
            ))
            (find (cdr f_table) f (car f_table)))
  ))

;;Compiling
(define compile-scheme-file
  (lambda (src trgt)
    (let* ((code (file->string src))
           (code_with_primitives (string-append runtime_scheme_string code))
           (sexprs (list-sexprs code_with_primitives))
           (parsed-and-optimized-code (map parse-and-optimize sexprs))
           (constants (list-constants parsed-and-optimized-code))
           (constants_objects (car constants))
           (constants_list (cdr constants))
           (runtime_cisc_string
             (let*
               (
                  (i 0)
                  ;;1 apply
                   (l-make-apply-clos (lgen "make_apply_clos"))
                   (l-apply-body (lgen "apply_body"))
                   (l-apply-count-args (lgen "apply_count_args"))
                   (l-apply-build-stack (lgen "apply_build_stack"))
                   (l-apply-push-args-loop (lgen "apply_push_args_loop"))
                   (l-apply-overwrite-old-frame (lgen "apply_overwrite_old_frame"))
                   (l-apply-proc (lgen "apply_proc"))
                   (apply_primitive
                     (string-append
                       "\nJUMP (" l-make-apply-clos ");\n"
                         l-apply-body ":\n"
                         "PUSH(FP);\n"
                         "MOV(FP , SP)\n"
                         "MOV(R10 , FPARG(1));\n"
                         "CMP(R10 , IMM(2));\n"
                         "JUMP_NE(L_error_args_count);\n"
                         "MOV(R11 , FPARG(2)); /* proc */\n"
                         "CMP(INDD(R11 ,0) ,IMM(T_CLOSURE));\n"
                         "JUMP_NE(L_error_cannot_apply_non_clos);\n"
                         "MOV(R12 , FPARG(3)); /* args */\n"

                         "/* count number of arguments */\n"
                         "MOV(R13 , 0);\n"
                         l-apply-count-args ":\n"
                         "CMP(INDD(R12 , 0) , IMM(T_NIL));\n"
                         "JUMP_EQ(" l-apply-build-stack ");\n"
                         "CMP(INDD(R12 , 0) , IMM(T_PAIR));\n"
                         "JUMP_NE(L_error_invalid_args_list);\n"

                         "MOV(R12 , INDD(R12 , 2));\n"
                         "INCR(R13);\n"
                         "JUMP(" l-apply-count-args ");\n"


                         "/* build stack from the top down */\n"
                         l-apply-build-stack ":\n"
                         "MOV(R12 , FPARG(3));\n /* save args list */\n"
                         "MOV(R14 , FPARG(-1)); /* save ret addr */\n"
                         "MOV(R15 , FPARG(-2)); /* save old fp */\n"
                         "ADD(SP , R13); /* move SP up arg count */\n"
                         "SUB(SP , IMM(3));\n"
                         "MOV(FP , SP);\n"
                         ;"DECR(SP);\n"
                         ;"MOV(STACK(SP) , R15); /* push old fp */\n"
                         "DECR(SP);\n"
                         "MOV(STACK(SP) , R14); /* push ret */ \n"
                         "DECR(SP);\n"
                         "MOV(STACK(SP) , INDD(R11 , 1)) /*push env */\n"
                         "DECR(SP);\n"
                         "MOV(STACK(SP) , R13) /* push argc */\n"

                         l-apply-push-args-loop ":\n"
                         "CMP(R13 , 0);\n"
                         "JUMP_EQ(" l-apply-proc ")\n"
                         "DECR(SP);\n"
                         "MOV(STACK(SP) , INDD(R12 , 1)); /* push next arg */\n"
                         "MOV(R12 , INDD(R12 ,2));\n"
                         "DECR(R13);\n"
                         "JUMP(" l-apply-push-args-loop ");\n"

                         l-apply-proc ":\n"
                         "MOV(SP , FP); /* restore SP */\n"
                         "MOV(FP , R15); /* restore old fp*/\n"
                         "JUMPA(INDD(R11 , 2)); /*apply - JUMP to function (CALL)*/\n"

                         l-make-apply-clos ":\n"
                         (make_primitive_closure l-apply-body (+ i fvars-offset))))

                 ;;2 < Less Then
                 (l_make_less_than_clos (lgen "make_less_than_clos"))
                 (l_less_than_body (lgen "less_than_body"))
                 (l_less_than_finish (lgen "less_than_finish"))
                 (l_less_than_two_args (lgen "less_than_two_args"))
                 (l_less_than_first_arg_not_fract (lgen "less_than_first_arg_not_fract"))
                 (l_less_than_second_arg_not_fract (lgen "less_than_second_arg_not_fract"))
                 (l_less_than_false (lgen "less_than_false"))
                 (less_than_primitive
                   (string-append
                  "\nJUMP (" l_make_less_than_clos ");\n"
                    "\n"
                    l_less_than_body
                    ":\n"
                    "PUSH(FP);\n"
                    "MOV(FP,SP);\n"
                    "PUSH(R1);\n"
                    "PUSH(R2);\n"
                    "PUSH(R3);\n"
                    "PUSH(R4);\n"
                    "PUSH(R5);\n"

                    "CMP(FPARG(1) , IMM(1));\n"
                    "JUMP_LT(L_error_args_count);\n"
                    "MOV(R1 , IMM(SOB_TRUE));\n"
                    "JUMP_GT(" l_less_than_two_args ");\n"

                    "\n/*less_then_single_arg*/\n"
                    "MOV(R2 , FPARG(2));\n"
                    "CMP(INDD(R2 , 0) , IMM(T_FRACT));\n"
                    "JUMP_EQ(" l_less_than_finish ");\n"
                    "CMP(INDD(R2 , 0) , IMM(T_INTEGER));\n"
                    "JUMP_EQ(" l_less_than_finish ");\n"
                    "JUMP_NE(L_error_incorr_type);\n"

                    "\n/*less_then_two_arg*/\n"
                    l_less_than_two_args":\n"
                    "\n/*less_then_two_args_type_checks*/\n"
                    "MOV(R2 , FPARG(2));\n"
                    "MOV(R3 , IMM(1));\n"
                    "MOV(R5 , IMM(1));\n"
                    "CMP(INDD(R2 , 0) , IMM(T_INTEGER));\n"
                    "JUMP_EQ(" l_less_than_first_arg_not_fract ");\n"
                    "CMP(INDD(R2 , 0) , IMM(T_FRACT));\n"
                    "JUMP_NE(L_error_incorr_type);\n"
                    "MOV(R3 , INDD(R2 , 2));\n"
                    l_less_than_first_arg_not_fract ":\n"
                    "MOV(R2 , INDD(R2 , 1));\n"

                    "MOV(R4 , FPARG(3));\n"
                    "CMP(INDD(R4 , 0) , IMM(T_INTEGER));\n"
                    "JUMP_EQ(" l_less_than_second_arg_not_fract ");\n"
                    "CMP(INDD(R4 , 0) , IMM(T_FRACT));\n"
                    "JUMP_NE(L_error_incorr_type);\n"
                    "MOV(R5 , INDD(R4 , 2));\n"
                    l_less_than_second_arg_not_fract ":\n"
                    "MOV(R4 , INDD(R4 , 1));"
                    "MUL(R2 , R5);\n"
                    "MUL(R4 , R3);\n"
                    "CMP(R2 , R4);\n"
                    "JUMP_GE(" l_less_than_false ");\n"
                    "JUMP(" l_less_than_finish ");\n"
                    "\n"
                     l_less_than_false
                     ":\n"
                    "MOV(R1 , IMM(SOB_FALSE));\n"
                    "\n"
                     l_less_than_finish
                    ":\n"
                    "MOV(R0 , R1);\n"

                    "POP(R5);\n"
                    "POP(R4);\n"
                    "POP(R3);\n"
                    "POP(R2);\n"
                    "POP(R1);\n"
                    "POP(FP);\n"
                    "RETURN;\n"
                    "\n"
                    l_make_less_than_clos
                    ":\n"
                   (make_primitive_closure l_less_than_body (begin (set! i (+ i 1)) (+ i fvars-offset))))
                    )

                (l_make_equal_clos (lgen "make_equal_clos"))
                (l_equal_body (lgen "equal_body"))
                (l_equal_finish (lgen "equal_finish"))
                (l_equal_two_args (lgen "equal_two_args"))
                (l_equal_first_arg_not_fract (lgen "equal_first_arg_not_fract"))
                (l_equal_second_arg_not_fract (lgen "equal_second_arg_not_fract"))
                (l_equal_false (lgen "equal_false"))
                (equal_primitive
                  (string-append
                 "\nJUMP (" l_make_equal_clos ");\n"
                   "\n"
                   l_equal_body
                   ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP,SP);\n"
                   "PUSH(R1);\n"
                   "PUSH(R2);\n"
                   "PUSH(R3);\n"
                   "PUSH(R4);\n"
                   "PUSH(R5);\n"

                   "CMP(FPARG(1) , IMM(1));\n"
                   "JUMP_LT(L_error_args_count);\n"
                   "MOV(R1 , IMM(SOB_TRUE));\n"
                   "JUMP_GT(" l_equal_two_args ");\n"

                   "\n/*equal_single_arg*/\n"
                   "MOV(R2 , FPARG(2));\n"
                   "CMP(INDD(R2 , 0) , IMM(T_FRACT));\n"
                   "JUMP_EQ(" l_equal_finish ");\n"
                   "CMP(INDD(R2 , 0) , IMM(T_INTEGER));\n"
                   "JUMP_EQ(" l_equal_finish ");\n"
                   "JUMP_NE(L_error_incorr_type);\n"

                   "\n/*equal_two_args*/\n"
                   l_equal_two_args":\n"
                   "\n/*equal_two_args_type_checks*/\n"
                   "MOV(R2 , FPARG(2));\n"
                   "MOV(R3 , IMM(1));\n"
                   "MOV(R5 , IMM(1));\n"
                   "CMP(INDD(R2 , 0) , IMM(T_INTEGER));\n"
                   "JUMP_EQ(" l_equal_first_arg_not_fract ");\n"
                   "CMP(INDD(R2 , 0) , IMM(T_FRACT));\n"
                   "JUMP_NE(L_error_incorr_type);\n"
                   "MOV(R3 , INDD(R2 , 2));\n"
                   l_equal_first_arg_not_fract ":\n"
                   "MOV(R2 , INDD(R2 , 1));\n"

                   "MOV(R4 , FPARG(3));\n"
                   "CMP(INDD(R4 , 0) , IMM(T_INTEGER));\n"
                   "JUMP_EQ(" l_equal_second_arg_not_fract ");\n"
                   "CMP(INDD(R4 , 0) , IMM(T_FRACT));\n"
                   "JUMP_NE(L_error_incorr_type);\n"
                   "MOV(R5 , INDD(R4 , 2));\n"
                   l_equal_second_arg_not_fract ":\n"
                   "MOV(R4 , INDD(R4 , 1));"
                   "MUL(R2 , R5);\n"
                   "MUL(R4 , R3);\n"
                   "CMP(R2 , R4);\n"
                   "JUMP_NE(" l_equal_false ");\n"
                   "JUMP(" l_equal_finish ");\n"
                   "\n"
                    l_equal_false
                    ":\n"
                   "MOV(R1 , IMM(SOB_FALSE));\n"
                   "\n"
                    l_equal_finish
                   ":\n"
                   "MOV(R0 , R1);\n"

                   "POP(R5);\n"
                   "POP(R4);\n"
                   "POP(R3);\n"
                   "POP(R2);\n"
                   "POP(R1);\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   "\n"
                   l_make_equal_clos
                   ":\n"
                  (make_primitive_closure l_equal_body (begin (set! i (+ i 1)) (+ i fvars-offset)))))

                 ;;4 > Greater Then
                 (l_make_greater_than_clos (lgen "make_greater_than_clos"))
                 (l_greater_than_body (lgen "greater_than_body"))
                 (l_greater_than_finish (lgen "greater_than_finish"))
                 (l_greater_than_two_args (lgen "greater_than_two_args"))
                 (l_greater_than_first_arg_not_fract (lgen "greater_than_first_arg_not_fract"))
                 (l_greater_than_second_arg_not_fract (lgen "greater_than_second_arg_not_fract"))
                 (l_greater_than_false (lgen "greater_than_false"))
                 (greater_than_primitive
                   (string-append
                  "\nJUMP (" l_make_greater_than_clos ");\n"
                    "\n"
                    l_greater_than_body
                    ":\n"
                    "PUSH(FP);\n"
                    "MOV(FP,SP);\n"
                    "PUSH(R1);\n"
                    "PUSH(R2);\n"
                    "PUSH(R3);\n"
                    "PUSH(R4);\n"
                    "PUSH(R5);\n"

                    "CMP(FPARG(1) , IMM(1));\n"
                    "JUMP_LT(L_error_args_count);\n"
                    "MOV(R1 , IMM(SOB_TRUE));\n"
                    "JUMP_GT(" l_greater_than_two_args ");\n"

                    "\n/*less_then_single_arg*/\n"
                    "MOV(R2 , FPARG(2));\n"
                    "CMP(INDD(R2 , 0) , IMM(T_FRACT));\n"
                    "JUMP_EQ(" l_greater_than_finish ");\n"
                    "CMP(INDD(R2 , 0) , IMM(T_INTEGER));\n"
                    "JUMP_EQ(" l_greater_than_finish ");\n"
                    "JUMP_NE(L_error_incorr_type);\n"

                    "\n/*less_then_two_arg*/\n"
                    l_greater_than_two_args":\n"
                    "\n/*less_then_two_args_type_checks*/\n"
                    "MOV(R2 , FPARG(2));\n"
                    "MOV(R3 , IMM(1));\n"
                    "MOV(R5 , IMM(1));\n"
                    "CMP(INDD(R2 , 0) , IMM(T_INTEGER));\n"
                    "JUMP_EQ(" l_greater_than_first_arg_not_fract ");\n"
                    "CMP(INDD(R2 , 0) , IMM(T_FRACT));\n"
                    "JUMP_NE(L_error_incorr_type);\n"
                    "MOV(R3 , INDD(R2 , 2));\n"
                    l_greater_than_first_arg_not_fract ":\n"
                    "MOV(R2 , INDD(R2 , 1));\n"

                    "MOV(R4 , FPARG(3));\n"
                    "CMP(INDD(R4 , 0) , IMM(T_INTEGER));\n"
                    "JUMP_EQ(" l_greater_than_second_arg_not_fract ");\n"
                    "CMP(INDD(R4 , 0) , IMM(T_FRACT));\n"
                    "JUMP_NE(L_error_incorr_type);\n"
                    "MOV(R5 , INDD(R4 , 2));\n"
                    l_greater_than_second_arg_not_fract ":\n"
                    "MOV(R4 , INDD(R4 , 1));"
                    "MUL(R2 , R5);\n"
                    "MUL(R4 , R3);\n"
                    "CMP(R2 , R4);\n"
                    "JUMP_LE(" l_greater_than_false ");\n"
                    "JUMP(" l_greater_than_finish ");\n"
                    "\n"
                     l_greater_than_false
                     ":\n"
                    "MOV(R1 , IMM(SOB_FALSE));\n"
                    "\n"
                     l_greater_than_finish
                    ":\n"
                    "MOV(R0 , R1);\n"

                    "POP(R5);\n"
                    "POP(R4);\n"
                    "POP(R3);\n"
                    "POP(R2);\n"
                    "POP(R1);\n"
                    "POP(FP);\n"
                    "RETURN;\n"
                    "\n"
                    l_make_greater_than_clos
                    ":\n"
                   (make_primitive_closure l_greater_than_body (begin (set! i (+ i 1)) (+ i fvars-offset)))))
                 ;;5 + plus
                 (l_make_plus_clos (lgen "make_plus_clos"))
                 (l_plus_body (lgen "plus_body"))
                 (l_plus_finish (lgen "plus_finish"))
                 (l_plus_first_arg_is_fract (lgen "plus_first_arg_is_fract"))
                 (l_plus_second_arg_is_fract (lgen "plus_second_arg_is_fract"))
                 (l_plus_loop (lgen "plus_loop"))
                 (plus_primitive
                   (string-append
                    "\nJUMP (" l_make_plus_clos ");\n"
                      "\n"
                      l_plus_body
                      ":\n"
                      "PUSH(FP);\n"
                      "MOV(FP,SP);\n"
                      ;"INFO;\n"
                      ;"SHOW(\"START OF +\", FPARG(1));\n"
                      "PUSH(R1);\n"
                      "PUSH(R2);\n"
                      "PUSH(R3);\n"
                      "PUSH(R4);\n"
                      "PUSH(R5);\n"
                      "PUSH(R6);\n"

                      "MOV(R1 , FPARG(1));\n"
                      "CMP(R1 , IMM(1));\n"
                      "MOV(R0 , IMM(SOB_INTEGER_ZERO))\n"
                      "JUMP_LT(" l_plus_finish ");\n"
                      "MOV(R0 , FPARG(2));\n"
                      ;"SHOW(\"AFTER CMP FPARG(2) IMM(1)\", FPARG(1));\n"
                      ;"SHOW(\"AFTER CMP FPARG(2) IMM(1)\", R0);\n"
                      "JUMP_EQ(" l_plus_finish ");\n"
                      "ADD(R1 , IMM(2));\n"
                      "MOV(R3 , IMM(3));\n"
                      "MOV(R2 , FPARG(2));\n"

                      ;"SHOW(\"BEFORE CHECK R2 T_FRACT\", R1);\n"
                      ;"SHOW(\"BEFORE CHECK R2 T_FRACT\", R2);\n"
                      ;"SHOW(\"BEFORE CHECK R2 T_FRACT\", R3);\n"

                      "CMP(INDD(R2 , 0) , IMM(T_FRACT));\n"
                      "JUMP_EQ(" l_plus_first_arg_is_fract ");\n"
                      (call "MAKE_SOB_FRACT" "INDD(R2 , 1)" "IMM(1)")
                      "MOV(R2 , R0);\n"
                      l_plus_first_arg_is_fract ":\n"
                      "\n\n/*plus_more_than_one_arg_loop*/\n"
                      l_plus_loop ":\n"
                      "MOV(R4 , FPARG(R3));\n"
                      "CMP(INDD(R4 , 0) , IMM(T_FRACT));\n"
                      "JUMP_EQ(" l_plus_second_arg_is_fract ");\n"
                      (call "MAKE_SOB_FRACT" "INDD(R4 , 1)" "IMM(1)")
                      "MOV(R4 , R0);\n"
                      l_plus_second_arg_is_fract ":\n"
                      "MOV(R5 , INDD(R4 , 2));\n"
                      "MUL(INDD(R2 , 1) , R5);\n"
                      "MOV(R6 , INDD(R2 , 2));\n"
                      "MUL(INDD(R4 , 1) , R6);\n"
                      "MUL(R5 , R6);\n"
                      "MOV(INDD(R2 , 2) , R5);\n"
                      "ADD(INDD(R2 , 1) , INDD(R4 , 1));\n"
                      "INCR(R3);\n"
                      "CMP(R1 , R3);\n"
                      "JUMP_GT(" l_plus_loop ");\n"
                      "\n\n/*plus_more_than_one_arg_after_loop*/\n"
                      "MOV(R6 , INDD(R2 , 2));\n"
                      "MOV(R5 , INDD(R2 , 1));\n"
                      "REM(R5 , R6);\n"
                      "CMP(R5 , IMM(0));\n"
                      "MOV(R0 , R2);\n"
                      "JUMP_NE(" l_plus_finish ");\n"
                      "MOV(R5 , INDD(R2 , 1));\n"
                      "DIV(R5 , R6);\n"
                      (call "MAKE_SOB_INTEGER" "R5")
                      l_plus_finish ":\n"
                      "POP(R6);\n"
                      "POP(R5);\n"
                      "POP(R4);\n"
                      "POP(R3);\n"
                      "POP(R2);\n"
                      "POP(R1);\n"
                      "POP(FP);\n"
                      "RETURN;\n"
                      "\n"
                    l_make_plus_clos ":\n"
                    (make_primitive_closure l_plus_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                  ))
                 ;;6 - minus
                 (l_make_minus_clos (lgen "make_minus_clos"))
                 (l_minus_body (lgen "minus_body"))
                 (l_minus_finish (lgen "minus_finish"))
                 (l_minus_first_arg_is_fract (lgen "minus_first_arg_is_fract"))
                 (l_minus_second_arg_is_fract (lgen "minus_second_arg_is_fract"))
                 (l_minus_loop (lgen "minus_loop"))
                 (minus_primitive
                   (string-append
                    "\nJUMP (" l_make_minus_clos ");\n"
                      "\n"
                      l_minus_body
                      ":\n"
                      "PUSH(FP);\n"
                      "MOV(FP,SP);\n"
                      ;"INFO;\n"
                      ;"SHOW(\"START OF +\", FPARG(1));\n"
                      "PUSH(R1);\n"
                      "PUSH(R2);\n"
                      "PUSH(R3);\n"
                      "PUSH(R4);\n"
                      "PUSH(R5);\n"
                      "PUSH(R6);\n"

                      "MOV(R1 , FPARG(1));\n"
                      "CMP(R1 , IMM(1));\n"
                      "JUMP_LT(L_error_args_count);\n"
                      "MOV(R0 , FPARG(2));\n"
                      ;"SHOW(\"AFTER CMP FPARG(2) IMM(1)\", FPARG(1));\n"
                      ;"SHOW(\"AFTER CMP FPARG(2) IMM(1)\", R0);\n"
                      "JUMP_EQ(" l_minus_finish ");\n"
                      "ADD(R1 , IMM(2));\n"
                      "MOV(R3 , IMM(3));\n"
                      "MOV(R2 , FPARG(2));\n"

                      ;"SHOW(\"BEFORE CHECK R2 T_FRACT\", R1);\n"
                      ;"SHOW(\"BEFORE CHECK R2 T_FRACT\", R2);\n"
                      ;"SHOW(\"BEFORE CHECK R2 T_FRACT\", R3);\n"

                      "CMP(INDD(R2 , 0) , IMM(T_FRACT));\n"
                      "JUMP_EQ(" l_minus_first_arg_is_fract ");\n"
                      (call "MAKE_SOB_FRACT" "INDD(R2 , 1)" "IMM(1)")
                      "MOV(R2 , R0);\n"
                      l_minus_first_arg_is_fract ":\n"
                      "\n\n/*minus_more_than_one_arg_loop*/\n"
                      l_minus_loop ":\n"
                      "MOV(R4 , FPARG(R3));\n"
                      "CMP(INDD(R4 , 0) , IMM(T_FRACT));\n"
                      "JUMP_EQ(" l_minus_second_arg_is_fract ");\n"
                      (call "MAKE_SOB_FRACT" "INDD(R4 , 1)" "IMM(1)")
                      "MOV(R4 , R0);\n"
                      l_minus_second_arg_is_fract ":\n"
                      "MOV(R5 , INDD(R4 , 2));\n"
                      "MUL(INDD(R2 , 1) , R5);\n"
                      "MOV(R6 , INDD(R2 , 2));\n"
                      "MUL(INDD(R4 , 1) , R6);\n"
                      "MUL(R5 , R6);\n"
                      "MOV(INDD(R2 , 2) , R5);\n"
                      "SUB(INDD(R2 , 1) , INDD(R4 , 1));\n"
                      "INCR(R3);\n"
                      "CMP(R1 , R3);\n"
                      "JUMP_GT(" l_minus_loop ");\n"
                      "\n\n/*minus_more_than_one_arg_after_loop*/\n"
                      "MOV(R6 , INDD(R2 , 2));\n"
                      "MOV(R5 , INDD(R2 , 1));\n"
                      "REM(R5 , R6);\n"
                      "CMP(R5 , IMM(0));\n"
                      "MOV(R0 , R2);\n"
                      "JUMP_NE(" l_minus_finish ");\n"
                      "MOV(R5 , INDD(R2 , 1));\n"
                      "DIV(R5 , R6);\n"
                      (call "MAKE_SOB_INTEGER" "R5")
                      l_minus_finish ":\n"
                      "POP(R6);\n"
                      "POP(R5);\n"
                      "POP(R4);\n"
                      "POP(R3);\n"
                      "POP(R2);\n"
                      "POP(R1);\n"
                      "POP(FP);\n"
                      "RETURN;\n"
                      "\n"
                    l_make_minus_clos ":\n"
                    (make_primitive_closure l_minus_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                  ))
                 ;;7 /
                 (l_make_div_clos (lgen "make_div_clos"))
                 (l_div_body (lgen "div_body"))
                 (l_div_finish (lgen "div_finish"))
                 (l_div_one_arg_not_fract (lgen "div_one_arg_not_fract"))
                 (l_div_more_than_one_arg (lgen "div_more_than_one_arg"))
                 (l_div_first_arg_is_fract (lgen "div_first_arg_is_fract"))
                 (l_div_second_arg_is_fract (lgen "div_second_arg_is_fract"))
                 (l_div_second_arg_is_not_fract (lgen "div_second_arg_is_not_fract"))
                 (l_div_GCD_LOOP (lgen "div_GCD_LOOP"))
                 (l_div_GCD_EXIT (lgen "div_GCD_EXIT"))
                 (l_div_loop (lgen "div_loop"))
                 (div_primitive
                   (string-append
                    "\nJUMP (" l_make_div_clos ");\n"
                      "\n"
                      l_div_body
                      ":\n"
                      "PUSH(FP);\n"
                      "MOV(FP,SP);\n"
                      ;"INFO;\n"
                      ;"SHOW(\"START OF +\", FPARG(1));\n"
                      "PUSH(R1);\n"
                      "PUSH(R2);\n"
                      "PUSH(R3);\n"
                      "PUSH(R4);\n"
                      "PUSH(R5);\n"
                      "PUSH(R6);\n"


                      "MOV(R1 , FPARG(1));\n"
                      ;"SHOW(\"BEGINNING CHECKING FPARG(1)\", FPARG(1));\n"
                      "CMP(R1 , IMM(1));\n"
                      "JUMP_LT(L_error_args_count);\n"
                      "MOV(R0 , FPARG(2));\n"

                      ;"SHOW(\"AFTER CMP FPARG(2) IMM(1)\", FPARG(1));\n"
                      ;"SHOW(\"AFTER CMP FPARG(2) IMM(1)\", R0);\n"

                      ;"CMP(R1 , IMM(1));\n"

                      "JUMP_GT(" l_div_more_than_one_arg ");\n"
                      "MOV(R2 , FPARG(2));\n"
                      "CMP(INDD(R2 , 0) , IMM(T_FRACT));\n"
                      "JUMP_NE(" l_div_one_arg_not_fract ");\n"
                      "MOV(R5 , INDD(R2 , 2));\n"
                      "MOV(INDD(R2 , 2) , INDD(R2 , 1));\n"
                      "MOV(INDD(R2 , 1) , R5);\n"
                      "MOV(R0 , R2);\n"
                      "JUMP(" l_div_finish ");\n"
                      l_div_one_arg_not_fract ":\n"
                      (call "MAKE_SOB_FRACT" "IMM(1)" "INDD(R2 , 1)")
                      "JUMP(" l_div_finish ");\n"


                      l_div_more_than_one_arg ":\n"
                      "ADD(R1 , IMM(2));\n"
                      "MOV(R3 , IMM(3));\n"
                      "MOV(R2 , FPARG(2));\n"

                      ;"SHOW(\"BEFORE CHECK R2 T_FRACT\", R1);\n"
                      ;"SHOW(\"BEFORE CHECK R2 T_FRACT\", R2);\n"
                      ;"SHOW(\"BEFORE CHECK R2 T_FRACT\", R3);\n"

                      "CMP(INDD(R2 , 0) , IMM(T_FRACT));\n"
                      "JUMP_EQ(" l_div_first_arg_is_fract ");\n"
                      (call "MAKE_SOB_FRACT" "INDD(R2 , 1)" "IMM(1)")
                      "MOV(R2 , R0);\n"

                      ;"SHOW(\"FIRST ARG IS NOT A FRACT - AFTER CREATING A FRACT\", R2);\n"

                      l_div_first_arg_is_fract ":\n"
                      "\n\n/*div_more_than_one_arg_loop*/\n"


                      l_div_loop ":\n"
                      "MOV(R4 , FPARG(R3));\n"
                      ;"SHOW(\"WHAT IS THE SECOND ARG?\", R4);\n"
                      ;"SHOW(\"WHAT IS THE SECOND ARG?\", R3);\n"
                      ;"SHOW(\"WHAT IS THE SECOND ARG?\", FPARG(R3));\n"
                      "CMP(INDD(R4 , 0) , IMM(T_FRACT));\n"

                      "JUMP_NE(" l_div_second_arg_is_not_fract ");\n"
                      ;
                      ;"INFO;"
                      ;"SHOW(\"BEFORE FLIPPING R4 \", R4);\n"

                      "MOV(R5 , INDD(R4 , 2));\n"
                      "MOV(INDD(R4 , 2) , INDD(R4 , 1));\n"
                      "MOV(INDD(R4 , 1) , R5);\n"
                      ;
                      ;"INFO;"
                      ;"SHOW(\"AFTER FLIPPING R4 \", R4);\n"

                      "JUMP(" l_div_second_arg_is_fract ");\n"

                      l_div_second_arg_is_not_fract ":\n"

                      (call "MAKE_SOB_FRACT" "IMM(1)" "INDD(R4 , 1)")
                      "MOV(R4 , R0);\n"
                      ;"INFO;"
                      ;"SHOW(\"R4 WAS NOT FRACT - NOW IT IS (AND FLIPPED) \", R4);\n"

                      l_div_second_arg_is_fract ":\n"

                      ;"SHOW(\"IN LOOP BEFORE MULT\", INDD(R2 , 1));\n"
                      ;"SHOW(\"IN LOOP BEFORE MULT\", INDD(R2 , 2));\n"
                      ;"SHOW(\"IN LOOP BEFORE MULT\", INDD(R4 , 1));\n"
                      ;"SHOW(\"IN LOOP BEFORE MULT\", INDD(R4 , 2));\n"

                      "MUL(INDD(R2 , 1) , INDD(R4 , 1));\n"
                      "MUL(INDD(R2 , 2) , INDD(R4 , 2));\n"
                      ;
                      ;"SHOW(\"IN LOOP after MULT\", INDD(R2 , 1));\n"
                      ;"SHOW(\"IN LOOP after MULT\", INDD(R2 , 2));\n"
                      ;"SHOW(\"IN LOOP after MULT\", INDD(R4 , 1));\n"
                      ;"SHOW(\"IN LOOP after MULT\", INDD(R4 , 2));\n"


                      "INCR(R3);\n"
                      "CMP(R1 , R3);\n"

                      ;"SHOW(\"CMP R1 R3 IF R1 IS GT LOOP AGAIN\", R1);\n"
                      ;"SHOW(\"CMP R1 R3 IF R1 IS GT LOOP AGAIN\", R3);\n"

                      "JUMP_GT(" l_div_loop ");\n"

                      "\n\n/*div_more_than_one_arg_after_loop*/\n"

                      "MOV(R6 , INDD(R2 , 2));\n"
                      "MOV(R5 , INDD(R2 , 1));\n"

                      ;"INFO;\n"
                      ;"SHOW(\"BEFORE GCD \", R2);\n"

                      l_div_GCD_LOOP ":\n"
                        "CMP(R5 , IMM(0));"
                        "JUMP_EQ(" l_div_GCD_EXIT ");"
                       "MOV(R3 , R5);"
                       "MOV(R4 , R6);"
                       "REM(R4 , R5);"
                       "MOV(R5 , R4);"
                       "MOV(R6 , R3);"
                       "JUMP(" l_div_GCD_LOOP ");\n"
                      l_div_GCD_EXIT ":\n"

                      ;"INFO;\n"
                      ;"SHOW(\"AFTER GCD \", R2);\n"

                      "DIV(INDD(R2 , 1) , IMM(R6));\n"
                      "DIV(INDD(R2 , 2) , IMM(R6));\n"

                      ;"INFO;"
                      ;"SHOW(\"AFTER DIVIDING BY GCD \", R2);\n"

                      "MOV(R6 , INDD(R2 , 2));\n"
                      "MOV(R5 , INDD(R2 , 1));\n"
                      "REM(R5 , R6);\n"
                      "CMP(R5 , IMM(0));\n"
                      ;
                      ;"INFO;"
                      ;"SHOW(\"AFTER REMAINDER \", R5);\n"


                      "MOV(R0 , R2);\n"
                      "JUMP_NE(" l_div_finish ");\n"
                      "MOV(R5 , INDD(R2 , 1));\n"
                      "DIV(R5 , R6);\n"
                      (call "MAKE_SOB_INTEGER" "R5")
                      l_div_finish ":\n"
                      "POP(R6);\n"
                      "POP(R5);\n"
                      "POP(R4);\n"
                      "POP(R3);\n"
                      "POP(R2);\n"
                      "POP(R1);\n"
                      "POP(FP);\n"
                      "RETURN;\n"
                      "\n"
                    l_make_div_clos ":\n"
                    (make_primitive_closure l_div_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                  ))
                 ;;8 *
                (l_make_mul_clos (lgen "make_mul_clos"))
                (l_mul_body (lgen "mul_body"))
                (l_mul_finish (lgen "mul_finish"))
                (l_mul_first_arg_is_fract (lgen "mul_first_arg_is_fract"))
                (l_mul_second_arg_is_fract (lgen "mul_second_arg_is_fract"))
                (l_mul_loop (lgen "mul_loop"))
                (mul_primitive
                  (string-append
                   "\nJUMP (" l_make_mul_clos ");\n"
                     "\n"
                     l_mul_body
                     ":\n"
                     "PUSH(FP);\n"
                     "MOV(FP,SP);\n"
                    ; "INFO;\n"
                    ; "SHOW(\"START OF +\", FPARG(1));\n"
                     "PUSH(R1);\n"
                     "PUSH(R2);\n"
                     "PUSH(R3);\n"
                     "PUSH(R4);\n"
                     "PUSH(R5);\n"
                     "PUSH(R6);\n"

                     "MOV(R1 , FPARG(1));\n"
                     "CMP(R1 , IMM(1));\n"
                     "MOV(R0 , IMM(SOB_INTEGER_ONE))\n"
                     "JUMP_LT(" l_mul_finish ");\n"
                     "MOV(R0 , FPARG(2));\n"
                    ; "SHOW(\"AFTER CMP FPARG(2) IMM(1)\", FPARG(1));\n"
                    ; "SHOW(\"AFTER CMP FPARG(2) IMM(1)\", R0);\n"
                     "JUMP_EQ(" l_mul_finish ");\n"
                     "ADD(R1 , IMM(2));\n"
                     "MOV(R3 , IMM(3));\n"
                     "MOV(R2 , FPARG(2));\n"

                    ; "SHOW(\"BEFORE CHECK R2 T_FRACT\", R1);\n"
                    ; "SHOW(\"BEFORE CHECK R2 T_FRACT\", R2);\n"
                    ; "SHOW(\"BEFORE CHECK R2 T_FRACT\", R3);\n"

                     "CMP(INDD(R2 , 0) , IMM(T_FRACT));\n"
                     "JUMP_EQ(" l_mul_first_arg_is_fract ");\n"
                     (call "MAKE_SOB_FRACT" "INDD(R2 , 1)" "IMM(1)")
                     "MOV(R2 , R0);\n"
                     l_mul_first_arg_is_fract ":\n"
                     "\n\n/*mul_more_than_one_arg_loop*/\n"
                     l_mul_loop ":\n"
                     "MOV(R4 , FPARG(R3));\n"
                     "CMP(INDD(R4 , 0) , IMM(T_FRACT));\n"
                     "JUMP_EQ(" l_mul_second_arg_is_fract ");\n"
                     (call "MAKE_SOB_FRACT" "INDD(R4 , 1)" "IMM(1)")
                     "MOV(R4 , R0);\n"
                     l_mul_second_arg_is_fract ":\n"
                    ; "MOV(R5 , INDD(R4 , 2));\n"
                     "MUL(INDD(R2 , 1) , INDD(R4 , 1));\n"
                    ; "MOV(R6 , INDD(R2 , 2));\n"
                     "MUL(INDD(R2 , 2) , INDD(R4 , 2));\n"
                    ; "MUL(R5 , R6);\n"
                    ; "MOV(INDD(R2 , 2) , R5);\n"
                    ; "ADD(INDD(R2 , 1) , INDD(R4 , 1));\n"
                     "INCR(R3);\n"
                     "CMP(R1 , R3);\n"
                     "JUMP_GT(" l_mul_loop ");\n"
                     "\n\n/*mul_more_than_one_arg_after_loop*/\n"
                     "MOV(R6 , INDD(R2 , 2));\n"
                     "MOV(R5 , INDD(R2 , 1));\n"
                     "REM(R5 , R6);\n"
                     "CMP(R5 , IMM(0));\n"
                     "MOV(R0 , R2);\n"
                     "JUMP_NE(" l_mul_finish ");\n"
                     "MOV(R5 , INDD(R2 , 1));\n"
                     "DIV(R5 , R6);\n"
                     (call "MAKE_SOB_INTEGER" "R5")
                     l_mul_finish ":\n"
                     "POP(R6);\n"
                     "POP(R5);\n"
                     "POP(R4);\n"
                     "POP(R3);\n"
                     "POP(R2);\n"
                     "POP(R1);\n"
                     "POP(FP);\n"
                     "RETURN;\n"
                     "\n"
                   l_make_mul_clos ":\n"
                   (make_primitive_closure l_mul_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                 ))
                ; ;;9 boolean?
                 (l_make_is_boolean_clos (lgen "make_is_boolean_clos"))
                 (l_is_boolean_body (lgen "is_boolean_body"))
                 (l_is_boolean_true (lgen "is_boolean_true"))
                 (l_is_boolean_false (lgen "is_boolean_false"))
                 (is_boolean_primitive
                   (string-append
                   "\nJUMP (" l_make_is_boolean_clos ");\n"
                   l_is_boolean_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP,SP);\n"
                   "PUSH(FPARG(2));\n"
                   "CALL(IS_SOB_BOOL);\n"
                   "DROP(1);\n"
                   "CMP(R0 , IMM(1));\n"
                   "JUMP_EQ(" l_is_boolean_true ");\n"
                   "MOV(R0 , IMM(SOB_FALSE));\n"
                   "JUMP(" l_is_boolean_false ");\n"
                    l_is_boolean_true ":\n"
                   "MOV(R0 , IMM(SOB_TRUE));\n"
                   l_is_boolean_false ":\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_is_boolean_clos ":\n"
                   (make_primitive_closure l_is_boolean_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                  ))
                 ;;10 car
                 (l_make_car_clos (lgen "make_car_clos"))
                 (l_car_body (lgen "car_body"))
                 (car_primitive
                   (string-append
                   "\nJUMP (" l_make_car_clos ");\n"
                   l_car_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP,SP);\n"
                   "PUSH(R1);\n"
                   "CMP(FPARG(1), IMM(1));\n"
                   "JUMP_NE(L_error_args_count);\n"
                   "MOV(R1 , FPARG(2));\n"
                   "CMP(INDD(R1 , 0), IMM(T_PAIR));\n"
                   "JUMP_NE(L_error_incorr_type);\n"
                   "MOV(R0 , INDD(R1 , 1));\n"
                   "POP(R1);\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_car_clos ":\n"
                   (make_primitive_closure l_car_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                  ))
                 ;;11 cdr
                 (l_make_cdr_clos (lgen "make_cdr_clos"))
                 (l_cdr_body (lgen "cdr_body"))
                 (cdr_primitive
                   (string-append
                   "\nJUMP (" l_make_cdr_clos ")\n";
                   l_cdr_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP,SP);\n"
                   "PUSH(R1);\n"
                   "CMP(FPARG(1), IMM(1));\n"
                   "JUMP_NE(L_error_args_count);\n"
                   "MOV(R1 , FPARG(2));\n"
                   "CMP(INDD(R1 , 0), IMM(T_PAIR));\n"
                   "JUMP_NE(L_error_incorr_type);\n"
                   "MOV(R0 , INDD(R1 , 2));\n"
                   "POP(R1);\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_cdr_clos ":\n"
                   (make_primitive_closure l_cdr_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                  ))
                 ;;12 set-car!
                 (l_make_set_car_clos (lgen "make_set_car_clos"))
                 (l_set_car_body (lgen "set_car_body"))
                 (set_car_primitive
                   (string-append
                      "\nJUMP (" l_make_set_car_clos ");\n"
                      l_set_car_body ":\n"
                      "PUSH(FP);\n"
                      "MOV(FP,SP);\n"
                      "PUSH(R2);\n"
                      "MOV(R0,FPARG(2));\n" ;pair
                      "MOV(R2,FPARG(3));\n" ;
                      "MOV(INDD(R0,1),R2);\n"
                      "MOV(R0,SOB_VOID);\n"
                      "POP(R2);\n"
                      "POP(FP);\n"
                      "RETURN;\n"
                      l_make_set_car_clos ":\n"
                      (make_primitive_closure l_set_car_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                     ))
                 ;;13 set-cdr!
                 (l_make_set_cdr_clos (lgen "make_set_cdr_clos"))
                 (l_set_cdr_body (lgen "set_cdr_body"))
                 (set_cdr_primitive
                   (string-append
                      "\nJUMP (" l_make_set_cdr_clos ");\n"
                      l_set_cdr_body ":\n"
                      "PUSH(FP);\n"
                      "MOV(FP,SP);\n"
                      "PUSH(R2);\n"
                      "MOV(R0,FPARG(2));\n"
                      "MOV(R2,FPARG(3));\n"
                      "MOV(INDD(R0,2),R2);\n"
                      "MOV(R0,SOB_VOID);\n"

                      "POP(R2);\n"
                      "POP(FP);\n"
                      "RETURN;\n"
                      l_make_set_cdr_clos ":\n"
                      (make_primitive_closure l_set_cdr_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                     ))
                 ;;14 cons
                 (l_make_cons_clos (lgen "make_cons_clos"))
                 (l_cons_body (lgen "cons_body"))
                 (cons_primitive
                   (string-append
                   "\nJUMP (" l_make_cons_clos ");\n"
                   l_cons_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP,SP);\n"
                   "PUSH(FPARG(3));\n"
                   "PUSH(FPARG(2));\n"
                   "CALL(MAKE_SOB_PAIR);\n"
                   "DROP(2);\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_cons_clos ":\n"
                   (make_primitive_closure l_cons_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                  ))
                 ;;15 list
                 (Replace5 (begin (set! i (+ 1 i))))
                 ;;16 append
                 (Replace6 (begin (set! i (+ 1 i))))
                 ;;17 pair?
                 (l_make_is_pair_clos (lgen "make_is_pair_clos"))
                 (l_is_pair_body (lgen "is_pair_body"))
                 (l_is_pair_true (lgen "is_pair_true"))
                 (l_is_pair_false (lgen "is_pair_false"))
                 (is_pair_primitive
                   (string-append
                   "\nJUMP (" l_make_is_pair_clos ");\n"
                   l_is_pair_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP,SP);\n"
                   "PUSH(FPARG(2));\n"
                   "CALL(IS_SOB_PAIR);\n"
                   "DROP(1);\n"
                   "CMP(R0 , IMM(1));\n"
                   "JUMP_EQ(" l_is_pair_true ");\n"
                   "MOV(R0 , IMM(SOB_FALSE));\n"
                   "JUMP(" l_is_pair_false ");\n"
                    l_is_pair_true ":\n"
                   "MOV(R0 , IMM(SOB_TRUE));\n"
                   l_is_pair_false ":\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_is_pair_clos ":\n"
                   (make_primitive_closure l_is_pair_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                  ))
                 ;;18 char->integer
                 (l_make_char_to_int_clos (lgen "make_char_to_int_clos"))
                 (l_char_to_int_body (lgen "is_char_to_int_body"))
                 (char_to_int_primitive
                   (string-append
                   "\nJUMP (" l_make_char_to_int_clos ");\n"
                   l_char_to_int_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP,SP);\n"
                   "MOV(R0,FPARG(2));\n"
                   "PUSH(INDD(R0,1));\n"
                   "CALL(MAKE_SOB_INTEGER);\n"
                   "DROP(1);\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_char_to_int_clos ":\n"
                   (make_primitive_closure l_char_to_int_body (begin (set! i (+ i 1)) (+ i fvars-offset)))))
                 ;;19 char?
                 (l_make_is_char_clos (lgen "make_is_char_clos"))
                 (l_is_char_body (lgen "is_char_body"))
                 (l_is_char_true (lgen "is_char_true"))
                 (l_is_char_false (lgen "is_char_false"))
                 (is_char_primitive
                   (string-append
                   "\nJUMP (" l_make_is_char_clos ");\n"
                   l_is_char_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP,SP);\n"
                   "PUSH(FPARG(2));\n"
                   "CALL(IS_SOB_CHAR);\n"
                   "DROP(1);\n"
                   "CMP(R0 , IMM(1));\n"
                   "JUMP_EQ(" l_is_char_true ");\n"
                   "MOV(R0 , IMM(SOB_FALSE));\n"
                   "JUMP(" l_is_char_false ");\n"
                    l_is_char_true ":\n"
                   "MOV(R0 , IMM(SOB_TRUE));\n"
                   l_is_char_false ":\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_is_char_clos ":\n"
                   (make_primitive_closure l_is_char_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                  ))
                 ;;20 eq_prim?
                 (l_make_is_eq_clos (lgen "make_is_eq_clos"))
                 (l_is_eq_body (lgen "is_eq_body"))
                 (l_is_eq_exit (lgen "is_eq_exit"))
                 (l_is_eq_true (lgen "is_eq_true"))
                 (l_is_eq_false (lgen "is_eq_false"))
                 (l_is_eq_compare_vals (lgen "is_eq_compare_vals"))
                 (is_eq_primitive
                   (string-append
                   "\nJUMP (" l_make_is_eq_clos ");\n"
                   l_is_eq_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP, SP);\n"
                   "PUSH(R1);\n"
                   "PUSH(R2);\n"

                   "MOV(R1 , FPARG(2));\n"
                   "MOV(R2 , FPARG(3));\n"
                   "CMP(INDD(R1 , 0) , INDD(R2 , 0));\n"
                   "MOV(R0 , IMM(SOB_FALSE));\n"
                   "JUMP_NE(" l_is_eq_exit ");\n"

                   "CMP(R1 , R2);\n"
                   "MOV(R0 , IMM(SOB_TRUE));\n"
                   "JUMP_EQ(" l_is_eq_exit ");\n"

                   "CMP(IND(R1) , T_INTEGER);\n"
                   "MOV(R0 , IMM(SOB_FALSE));\n"
                   "JUMP_EQ(" l_is_eq_compare_vals ");\n"

                   "CMP(IND(R1) , T_CHAR);\n"
                   "MOV(R0 , IMM(SOB_FALSE));\n"
                   "JUMP_EQ(" l_is_eq_compare_vals ");\n"

                   "CMP(IND(R1) , T_SYMBOL);\n"
                   "MOV(R0 , IMM(SOB_FALSE));\n"
                   "JUMP_EQ(" l_is_eq_compare_vals ");\n"

                   "CMP(IND(R1) , T_FRACT);\n"
                   "MOV(R0 , IMM(SOB_FALSE));\n"
                   "JUMP_NE(" l_is_eq_exit ");\n"
                   "PUSH(R1);\n"
                   "PUSH(R2);\n"
                   "CALL(" l_equal_body ");\n"
                   "DROP(2);\n"
                   "JUMP(" l_is_eq_exit ");\n"

                   l_is_eq_compare_vals ":\n"
                   "CMP(INDD(R1 , 1) , INDD(R2 , 1));\n"
                   "JUMP_NE(" l_is_eq_exit ");\n"
                   "MOV(R0, IMM(SOB_TRUE));\n"
                   l_is_eq_exit ":\n"

                   "POP(R2);\n"
                   "POP(R1);\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_is_eq_clos ":\n"
                   (make_primitive_closure l_is_eq_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                   ))
                 ;;21 integer?
                 (l_make_is_integer_clos (lgen "make_is_integer_clos"))
                 (l_is_integer_body (lgen "is_integer_body"))
                 (l_is_integer_true (lgen "is_integer_true"))
                 (l_is_integer_false (lgen "is_integer_false"))
                 (is_integer_primitive
                   (string-append
                   "\nJUMP (" l_make_is_integer_clos ");\n"
                   l_is_integer_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP,SP);\n"
                   "PUSH(FPARG(2));\n"
                   "CALL(IS_SOB_INTEGER);\n"
                   "DROP(1);\n"
                   "CMP(R0 , IMM(1));\n"
                   "JUMP_EQ(" l_is_integer_true ");\n"
                   "MOV(R0 , IMM(SOB_FALSE));\n"
                   "JUMP(" l_is_integer_false ");\n"
                    l_is_integer_true ":\n"
                   "MOV(R0 , IMM(SOB_TRUE));\n"
                   l_is_integer_false ":\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_is_integer_clos ":\n"
                   (make_primitive_closure l_is_integer_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                  ))
                 ;;22 number?
                 (Replace9 (begin (set! i (+ 1 i))))
                 ;;23 integer->char
                 (l_make_int_to_char_clos (lgen "make_int_to_char_clos"))
                 (l_int_to_char_body (lgen "is_int_to_char_body"))
                 (int_to_char_primitive
                   (string-append
                   "\nJUMP ("  l_make_int_to_char_clos ");\n"
                   l_int_to_char_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP,SP);\n"
                   "MOV(R0,FPARG(2));\n"
                   "PUSH(INDD(R0,1));\n"
                   "CALL(MAKE_SOB_CHAR);\n"
                   "DROP(1);\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_int_to_char_clos ":\n"
                   (make_primitive_closure l_int_to_char_body (begin (set! i (+ i 1)) (+ i fvars-offset)))))
                 ;;24 map
                 (Replace11 (begin (set! i (+ 1 i))))
                 ;;25 not
                 (Replace12 (begin (set! i (+ 1 i))))
                 ;;26 null?
                 (l_make_is_null_clos (lgen "make_is_null_clos"))
                 (l_is_null_body (lgen "is_null_body"))
                 (l_is_null_true (lgen "is_null_true"))
                 (l_is_null_false (lgen "is_null_false"))
                 (is_null_primitive
                   (string-append
                   "\nJUMP (" l_make_is_null_clos ");\n"
                   l_is_null_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP,SP);\n"
                   "PUSH(FPARG(2));\n"
                   "CALL(IS_SOB_NIL);\n"
                   "DROP(1);\n"
                   "CMP(R0 , IMM(1));\n"
                   "JUMP_EQ(" l_is_null_true ");\n"
                   "MOV(R0 , IMM(SOB_FALSE));\n"
                   "JUMP(" l_is_null_false ");\n"
                    l_is_null_true ":\n"
                   "MOV(R0 , IMM(SOB_TRUE));\n"
                   l_is_null_false ":\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_is_null_clos ":\n"
                   (make_primitive_closure l_is_null_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                  ))
                 ;;27 procedure?
                 (l_make_is_procedure_clos (lgen "make_is_procedure_clos"))
                 (l_is_procedure_body (lgen "is_procedure_body"))
                 (l_is_procedure_true (lgen "is_procedure_true"))
                 (l_is_procedure_false (lgen "is_procedure_false"))
                 (is_procedure_primitive
                   (string-append
                   "\nJUMP (" l_make_is_procedure_clos ");\n"
                   l_is_procedure_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP,SP);\n"
                   "PUSH(FPARG(2));\n"
                   "CALL(IS_SOB_CLOSURE);\n"
                   "DROP(1);\n"
                   "CMP(R0 , IMM(1));\n"
                   "JUMP_EQ(" l_is_procedure_true ");\n"
                   "MOV(R0 , IMM(SOB_FALSE));\n"
                   "JUMP(" l_is_procedure_false ");\n"
                    l_is_procedure_true ":\n"
                   "MOV(R0 , IMM(SOB_TRUE));\n"
                   l_is_procedure_false ":\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_is_procedure_clos ":\n"
                   (make_primitive_closure l_is_procedure_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                  ))
                 ;;28 rational?
                 (l_make_is_rational_clos (lgen "make_is_rational_clos"))
                 (l_is_rational_body (lgen "is_rational_body"))
                 (l_is_rational_exit (lgen "is_rational_exit"))
                 (l_is_rational_true (lgen "is_rational_true"))
                 (is_rational_primitive
                   (string-append
                   "\nJUMP (" l_make_is_rational_clos ");\n"
                   l_is_rational_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP, SP);\n"
                   "MOV(R0, FPARG(2));\n"
                   "CMP(IND(R0), T_INTEGER);\n"
                   "JUMP_EQ(" l_is_rational_true ");\n"
                   "CMP(IND(R0), T_FRACT);\n"
                   "JUMP_EQ(" l_is_rational_true ");\n"
                   "MOV(R0, IMM(SOB_FALSE));\n"
                   "JUMP(" l_is_rational_exit ");\n"
                   l_is_rational_true ":\n"
                   "MOV(R0, IMM(SOB_TRUE));\n"
                   l_is_rational_exit ":\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_is_rational_clos ":\n"
                   (make_primitive_closure l_is_rational_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                   ))
                 ;;29 denominator
                 (l_make_denominator_clos (lgen "make_denominator_clos"))
                 (l_denominator_body (lgen "is_denominator_body"))
                 (l_denominator_finish (lgen "denominator_finish"))
                 (l_denominator_int (lgen "denominator_int"))
                 (denominator_primitive
                   (string-append
                   "\nJUMP (" l_make_denominator_clos ");\n"
                   l_denominator_body ":\n"
                    "PUSH(FP);\n"
                    "MOV(FP , SP);\n"
                    "MOV(R0 , FPARG(2));\n"
                    "CMP(INDD(R0, 0) , T_INTEGER);\n"
                    "\nJUMP_EQ(" l_denominator_int ");\n"
                    "MOV(R0 , INDD(R0 , 2));\n"
                    "PUSH(R0);\n"
                    "CALL(MAKE_SOB_INTEGER);\n"
                    "DROP(1);\n"
                    "\nJUMP(" l_denominator_finish ");\n"
                    l_denominator_int ":\n"
                    "PUSH(IMM(1));\n"
                    "CALL(MAKE_SOB_INTEGER);\n"
                    "DROP(1);\n"
                    l_denominator_finish ":\n"
                    "POP(FP);\n"
                    "RETURN;\n"
                    l_make_denominator_clos ":\n"
                    (make_primitive_closure l_denominator_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                    ))
                 ;;30 numerator
                 (l_make_numerator_clos (lgen "make_numerator_clos"))
                 (l_numerator_body (lgen "is_numerator_body"))
                 (numerator_primitive
                   (string-append
                   "\nJUMP (" l_make_numerator_clos ");\n"
                   l_numerator_body ":\n"
                    "PUSH(FP);\n"
                    "MOV(FP,SP);\n"
                    "MOV(R0,FPARG(2));\n"
                    "MOV(R0,INDD(R0,IMM(1)));\n"
                    "PUSH(R0);\n"
                    "CALL(MAKE_SOB_INTEGER);\n"
                    "DROP(1);\n"
                    "POP(FP);\n"
                    "RETURN;\n"
                    l_make_numerator_clos ":\n"
                    (make_primitive_closure l_numerator_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                    ))
                 ;;31 remainder
                 (l_make_remainder_clos (lgen "make_remainder_clos"))
                 (l_remainder_body (lgen "remainder_body"))
                 (remainder_primitive
                   (string-append
                   "\nJUMP (" l_make_remainder_clos ");\n"
                   l_remainder_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP,SP);\n"
                   "PUSH(R1);\n"
                   "PUSH(R2);\n"
                   "MOV(R0,FPARG(2));\n"
                   "MOV(R1,INDD(R0,IMM(1)));\n"
                   "MOV(R0,FPARG(3));\n"
                   "MOV(R2,INDD(R0,IMM(1)));\n"
                   "REM(R1,R2);\n"
                   "MOV(R0,R1);\n"
                   "PUSH(R0);\n"
                   "CALL(MAKE_SOB_INTEGER);\n"
                   "DROP(1);\n"
                   "POP(R2);\n"
                   "POP(R1);\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_remainder_clos ":\n"
                   (make_primitive_closure l_remainder_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                   ))
                 ;;32 make-string
                 (l_make_make_string_clos (lgen "make_make_string_clos"))
                 (l_make_string_body (lgen "make_string_body"))
                 (l_make_string_loop_to_make_new_char (lgen "make_string_loop_to_make_new_char"))
                 (l_make_string_with_one_arg (lgen "make_string_with_one_arg"))
                 (l_make_string_make_str (lgen "make_string_make_str"))
                 (make_string_primitive
                   (string-append
                     "\nJUMP (" l_make_make_string_clos ");\n"
                       l_make_string_body ":\n"
                       "PUSH(FP);\n"
                       "MOV(FP,SP);\n"
                       "MOV(R0,FPARG(1));\n"
                       "CMP(R0,IMM(1));\n"
                       "JUMP_EQ(" l_make_string_with_one_arg ");\n"
                       "MOV(R0,FPARG(2));\n"
                       "MOV(R0,INDD(R0,1));\n"
                       "MOV(R2,R0);\n"
                       "MOV(R1, FPARG(3));\n"
                       "MOV(R1,INDD(R1,1));\n"
                       "JUMP("l_make_string_loop_to_make_new_char");\n"
                       l_make_string_with_one_arg ":\n"
                       "MOV(R0,FPARG(2));\n"
                       "MOV(R0,INDD(R0,1));\n"
                       "MOV(R2,R0);\n"
                       "MOV(R1, IMM(0));\n"
                       l_make_string_loop_to_make_new_char ":\n"
                       "CMP(R0, IMM(0));\n"
                       "JUMP_EQ(" l_make_string_make_str ");\n"
                       "PUSH(R1);\n"
                       "SUB(R0,IMM(1));\n"
                       "JUMP(" l_make_string_loop_to_make_new_char ");\n"
                       l_make_string_make_str ":\n"
                       "PUSH(R2);\n"
                       "CALL(MAKE_SOB_STRING);\n"
                       "DROP(R2+1);\n"
                       "POP(FP);\n"
                       "RETURN;\n"
                       l_make_make_string_clos ":\n"
                       (make_primitive_closure l_make_string_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                       ))

                 ;;33 string-length
                 (l_make_string_length_clos (lgen "make_string_length_clos"))
                 (l_string_length_body (lgen "string_length_body"))
                 (string_length_primitive
                   (string-append
                   "\nJUMP (" l_make_string_length_clos ");\n"
                   l_string_length_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP,SP);\n"
                   "MOV(R0 , FPARG(2));\n"
                   "PUSH(INDD(R0 , 1));\n"
                   "CALL(MAKE_SOB_INTEGER);\n"
                   "DROP(1);\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_string_length_clos ":\n"
                   (make_primitive_closure l_string_length_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                   ))
                 ;;34 string-ref
                 (l_make_string_ref_clos (lgen "make_string_ref_clos"))
                 (l_string_ref_body (lgen "string_ref_body"))
                 (string_ref_primitive
                   (string-append
                     "\nJUMP (" l_make_string_ref_clos ");\n"
                      l_string_ref_body ":\n"
                      "PUSH(FP);\n"
                      "MOV(FP,SP);\n"
                      "PUSH(R1);\n"
                      "MOV(R1,FPARG(3));\n"
                      "MOV(R1,INDD(R1,1));\n"
                      "ADD(R1,IMM(2));\n"
                      "MOV(R0,FPARG(2));\n"
                      "MOV(R0,INDD(R0,R1));\n"
                      "PUSH(R0);\n"
                      "CALL(MAKE_SOB_CHAR);\n"
                      "DROP(1);\n"
                      "POP(R1);\n"
                      "POP(FP);\n"
                      "RETURN;\n"
                      l_make_string_ref_clos ":\n"
                      (make_primitive_closure l_string_ref_body (begin (set! i (+ i 1)) (+ i fvars-offset)))))
                 ;;35 string-set!
                 (l_make_string_set_clos (lgen "make_string_set_clos"))
                 (l_string_set_body (lgen "string_set_body"))
                 (string_set_primitive
                   (string-append
                      "\nJUMP (" l_make_string_set_clos ");\n"
                      l_string_set_body ":\n"
                      "PUSH(FP);\n"
                      "MOV(FP,SP);\n"
                      "PUSH(R1);\n"
                      "PUSH(R2);\n"
                      "MOV(R1,FPARG(3));\n"
                      "MOV(R1,INDD(R1,1));\n"
                      "ADD(R1,IMM(2));\n"
                      "MOV(R0,FPARG(2));\n"
                      "MOV(R2,FPARG(4));\n"
                      "MOV(R2,INDD(R2,1));\n"
                      "MOV(INDD(R0,R1),R2);\n"
                      "MOV(R0,SOB_VOID);\n"
                      "POP(R2);\n"
                      "POP(R1);\n"
                      "POP(FP);\n"
                      "RETURN;\n"
                      l_make_string_set_clos ":\n"
                      (make_primitive_closure l_string_set_body (begin (set! i (+ i 1)) (+ i fvars-offset)))))
                 ;;36 string->symbol
                 (l_make_string_to_symbol_clos (lgen "make_string_to_symbol_clos"))
                 (l_string_to_symbol_body (lgen "string_to_symbol_body"))
                 (l_string_to_symbol_finish (lgen "string_to_symbol_finish"))
                 (l_string_to_symbol_find_symbol_loop (lgen "string_to_symbol_find_symbol_loop"))
                 (l_string_to_symbol_add_symbol (lgen "string_to_symbol_add_symbol"))
                 (l_string_to_symbol_found_symbol (lgen "string_to_symbol_found_symbol"))
                 (string_to_symbol_primitive
                   (string-append
                   "\nJUMP (" l_make_string_to_symbol_clos ");\n"
                   l_string_to_symbol_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP, SP);\n"
                   "PUSH(R1);\n"
                   "PUSH(R2);\n"
                   "PUSH(R3);\n"
                   "MOV(R2 , FPARG(2));\n"
                   "MOV(R1 , IND(1));\n"
                   "CMP(R1 , IMM(-1));\n"
                   "JUMP_NE(" l_string_to_symbol_find_symbol_loop ");\n"
                   "PUSH(R2);\n"
                   "CALL(MAKE_SOB_SYMBOL)\n"
                   "DROP(1);\n"
                   "MOV(R1 , R0);\n"
                   "MOV(IND(1) , INDD(R1 , 1));\n"
                   "JUMP(" l_string_to_symbol_finish ");\n"
                   l_string_to_symbol_find_symbol_loop ":\n"
                   "CMP(IND(R1) , IMM(R2));\n"
                   "JUMP_EQ(" l_string_to_symbol_found_symbol ");\n"
                   "INCR(R1);\n"
                   "CMP(IND(R1) , IMM(0));\n"
                   "JUMP_EQ(" l_string_to_symbol_add_symbol ");\n"
                   "MOV(R1 , IND(R1));\n"
                   "JUMP(" l_string_to_symbol_find_symbol_loop ");\n"
                   l_string_to_symbol_add_symbol ":\n"
                   "PUSH(R2);\n"
                   "CALL(MAKE_SOB_SYMBOL);\n"
                   "DROP(1);\n"
                   "INCR(IND(0));\n"
                   "MOV(R3 , R0);\n"
                   "INCR(R3);\n"
                   "MOV(IND(R1) , R3);\n"
                   "JUMP(" l_string_to_symbol_finish ");\n"
                   l_string_to_symbol_found_symbol ":\n"
                   "PUSH(R2);\n"
                   "CALL(MAKE_SOB_SYMBOL);\n"
                   "DROP(1);\n"
                   l_string_to_symbol_finish ":\n"
                   "POP(R3);\n"
                   "POP(R2);\n"
                   "POP(R1);\n"
                   "POP(FP);\n"
                   "RETURN;\n"

                   l_make_string_to_symbol_clos ":\n"
                   (make_primitive_closure l_string_to_symbol_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                   ))
                 ;;37 string?
                 (l_make_is_string_clos (lgen "make_is_string_clos"))
                 (l_is_string_body (lgen "is_string_body"))
                 (l_is_string_exit (lgen "is_string_exit"))
                 (l_is_string_true (lgen "is_string_true"))
                 (is_string_primitive
                   (string-append
                   "\nJUMP (" l_make_is_string_clos ");\n"
                   l_is_string_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP, SP);\n"
                   "MOV(R0, FPARG(2));\n"
                   "CMP(IND(R0), T_STRING);\n"
                   "JUMP_EQ(" l_is_string_true ");\n"
                   "MOV(R0, IMM(SOB_FALSE));\n"
                   "JUMP(" l_is_string_exit ");\n"
                   l_is_string_true ":\n"
                   "MOV(R0, IMM(SOB_TRUE));\n"
                   l_is_string_exit ":\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_is_string_clos ":\n"
                   (make_primitive_closure l_is_string_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                   ))
                 ;;38 symbol?
                 (l_make_is_symbol_clos (lgen "make_is_symbol_clos"))
                 (l_is_symbol_body (lgen "is_symbol_body"))
                 (l_is_symbol_exit (lgen "is_symbol_exit"))
                 (l_is_symbol_true (lgen "is_symbol_true"))
                 (is_symbol_primitive
                   (string-append
                   "\nJUMP (" l_make_is_symbol_clos ");\n"
                   l_is_symbol_body ":\n"
                   "PUSH(FP);\n"
                   "MOV(FP, SP);\n"
                   "MOV(R0, FPARG(2));\n"
                   "CMP(IND(R0), T_SYMBOL);\n"
                   "JUMP_EQ(" l_is_symbol_true ");\n"
                   "MOV(R0, IMM(SOB_FALSE));\n"
                   "JUMP(" l_is_symbol_exit ");\n"
                   l_is_symbol_true ":\n"
                   "MOV(R0, IMM(SOB_TRUE));\n"
                   l_is_symbol_exit ":\n"
                   "POP(FP);\n"
                   "RETURN;\n"
                   l_make_is_symbol_clos ":\n"
                   (make_primitive_closure l_is_symbol_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                   ))
                  ;;39 symbol->string
                  (l_make_symbol_to_string_clos (lgen "make_symbol_to_string_clos"))
                  (l_symbol_to_string_body (lgen "symbol_to_string_body"))
                  (symbol_to_string_primitive
                    (string-append
                    "\nJUMP (" l_make_symbol_to_string_clos ");\n"
                    l_symbol_to_string_body ":\n"
                    "PUSH(FP);\n"
                    "MOV(FP, SP);\n"
                    "MOV(R0 , FPARG(2));\n"
                    "MOV(R0 , INDD(R0 , 1));\n"
                    "POP(FP);\n"
                    "RETURN;\n"
                    l_make_symbol_to_string_clos ":\n"
                    (make_primitive_closure l_symbol_to_string_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                    ))
                  ;;40 vector
                  (Replace40 (begin (set! i (+ 1 i))))
                  ;;41 make-vector
                  (l_make_make_vector_clos (lgen "make_make_vector_clos"))
                  (l_make_vector_body (lgen "make_vector_body"))
                  (l_make_vector_loop_to_make_new_item (lgen "make_vector_loop_to_make_new_item"))
                  (l_make_vector_with_one_arg (lgen "make_vector_with_one_arg"))
                  (l_make_vector_make_vec (lgen "make_vector_make_vec"))
                  (make_vector_primitive
                    (string-append
                        "\nJUMP (" l_make_make_vector_clos ");\n"
                        l_make_vector_body  ":\n"
                        "PUSH(FP);\n"
                        "MOV(FP,SP);\n"
                        "PUSH(R1);\n"
                        "PUSH(R2);\n"
                        "MOV(R0 , FPARG(1));\n"
                        "CMP(INDD(R0,1) , IMM(1));\n"
                        "JUMP_EQ(" l_make_vector_with_one_arg ");\n"
                        "MOV(R0,FPARG(2));\n"
                        "MOV(R0,INDD(R0,1));\n"
                        "MOV(R2,R0);\n"
                        "MOV(R1, SOB_INTEGER_ZERO);\n"
                        "JUMP(" l_make_vector_loop_to_make_new_item ");\n"
                        l_make_vector_with_one_arg ":\n"
                        "MOV(R0 , FPARG(2));\n"
                        "MOV(R0 , INDD(R0 , 1));\n"
                        "MOV(R2 , R0);\n"
                        "PUSH(IMM(0));\n"
                        "CALL(MAKE_SOB_INTEGER);\n"
                        "MOV(R1,R0);\n"
                        "MOV(R0,R2);\n"
                        l_make_vector_loop_to_make_new_item ":\n"
                        "CMP(R0, IMM(0));\n"
                        "JUMP_EQ(" l_make_vector_make_vec ");\n"
                        "PUSH(R1);\n"
                        "SUB(R0,IMM(1));\n"
                        "JUMP(" l_make_vector_loop_to_make_new_item ");\n"
                        l_make_vector_make_vec ":\n"
                        "PUSH(R2);\n"
                        "CALL(MAKE_SOB_VECTOR);\n"
                        "INCR(R2);\n"
                        "DROP(R2);\n"

                        "POP(R2);\n"
                        "POP(R1);\n"
                        "POP(FP);\n"
                        "RETURN;\n"
                        l_make_make_vector_clos ":\n"
                        (make_primitive_closure l_make_vector_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                        ))
                  ;;42 vector-length
                  (l_make_vector_length_clos (lgen "make_vector_length_clos"))
                  (l_vector_length_body (lgen "vector_length_body"))
                  (vector_length_primitive
                    (string-append
                      "\nJUMP (" l_make_vector_length_clos ");\n"
                      l_vector_length_body ":\n"
                      "PUSH(FP);\n"
                      "MOV(FP,SP);\n"
                      "MOV(R0,FPARG(2));\n"
                      "MOV(R0,INDD(R0,IMM(1)));\n"
                      "PUSH(R0);\n"
                      "CALL(MAKE_SOB_INTEGER);\n"
                      "DROP(1);\n"
                      "POP(FP);\n"
                      "RETURN;\n"
                      l_make_vector_length_clos ":\n"
                      (make_primitive_closure l_vector_length_body (begin (set! i (+ i 1)) (+ i fvars-offset)))))
                  ;;43 vector-ref
                  (l_make_vector_ref_clos (lgen "make_vector_ref_clos"))
                  (l_vector_ref_body (lgen "vector_ref_body"))
                  (vector_ref_primitive
                    (string-append
                      "\nJUMP (" l_make_vector_ref_clos ");\n"
                      l_vector_ref_body ":\n"
                      "PUSH(FP);\n"
                      "MOV(FP,SP);\n"
                      "MOV(R1,FPARG(3));\n"
                      "MOV(R1,INDD(R1,1));\n"
                      "ADD(R1,IMM(2));\n"
                      "MOV(R0,FPARG(2));\n"
                      "MOV(R0,INDD(R0,R1));\n"
                      "POP(FP);\n"
                      "RETURN;\n"
                      l_make_vector_ref_clos ":\n"
                      (make_primitive_closure l_vector_ref_body (begin (set! i (+ i 1)) (+ i fvars-offset)))))
                  ;;44 vector-set!
                  (l_make_vector_set_clos (lgen "make_vector_set_clos"))
                  (l_vector_set_body (lgen "vector_set_body"))
                  (vector_set_primitive
                    (string-append
                      "\nJUMP (" l_make_vector_set_clos ");\n"
                      l_vector_set_body ":\n"
                       "PUSH(FP);\n"
                       "MOV(FP,SP);\n"
                       "MOV(R1,FPARG(3));\n"
                       "MOV(R1,INDD(R1,1));\n"
                       "ADD(R1,IMM(2));\n"
                       "MOV(R0,FPARG(2));\n"
                       "MOV(R2,FPARG(4));\n"
                       "MOV(INDD(R0,R1),R2);\n"
                       "MOV(R0,SOB_VOID);\n"
                       "POP(FP);\n"
                       "RETURN;\n"
                       l_make_vector_set_clos ":\n"
                       (make_primitive_closure l_vector_set_body (begin (set! i (+ i 1)) (+ i fvars-offset)))))
                  ;;45 vector?
                  (l_make_is_vector_clos (lgen "make_is_vector_clos"))
                  (l_is_vector_body (lgen "is_vector_body"))
                  (l_is_vector_false (lgen "is_vector_false"))
                  (is_vector_primitive
                    (string-append
                      "\nJUMP (" l_make_is_vector_clos ");\n"
                      l_is_vector_body ":\n"
                      "PUSH(FP);\n"
                      "MOV(FP,SP);\n"
                      "MOV(R0,FPARG(2));\n"
                      "MOV(R0,INDD(R0,IMM(0)));\n"
                      "CMP(R0, IMM(T_VECTOR));\n"
                      "JUMP_NE(" l_is_vector_false ");\n"
                      "MOV(R0,SOB_TRUE);\n"
                      "POP(FP);\n"
                      "RETURN;\n"
                      l_is_vector_false ":\n"
                      "MOV(R0, SOB_FALSE);\n"
                      "POP(FP);\n"
                      "RETURN;\n"
                    l_make_is_vector_clos ":\n"
                    (make_primitive_closure l_is_vector_body (begin (set! i (+ i 1)) (+ i fvars-offset)))
                    ))
                  ;;46 zero?
                  (Replace46 (begin (set! i (+ 1 i))))
               )
             (string-append
              apply_primitive
              less_than_primitive
              equal_primitive
              greater_than_primitive
              plus_primitive
              minus_primitive
              div_primitive
              mul_primitive
              is_boolean_primitive
              car_primitive
              cdr_primitive
              set_car_primitive
              set_cdr_primitive
              cons_primitive
              is_pair_primitive
              char_to_int_primitive
              is_char_primitive
              is_eq_primitive
              is_integer_primitive
              ;;number?
              int_to_char_primitive
              ;;map
              ;;not
              is_null_primitive
              is_procedure_primitive
              is_rational_primitive
              denominator_primitive
              numerator_primitive
              remainder_primitive
              make_string_primitive
              string_length_primitive
              string_ref_primitive
              string_set_primitive
              string_to_symbol_primitive
              is_string_primitive
              is_symbol_primitive
              symbol_to_string_primitive
              make_vector_primitive
              vector_length_primitive
              vector_ref_primitive
              vector_set_primitive
              is_vector_primitive
              ;zero?
             )))
           (fvars (list-fvars parsed-and-optimized-code))
           (fvars_objects (car fvars))
           (fvars_list (cdr fvars))
           (set_up_stack
              (string-append
                "\n/* ---COMPILED CODE--- */\n"
                "PUSH(IMM(-99999999)); /* fake arg1 */\n"
                "PUSH(IMM(1)); /* fake argc */\n"
                "/* create fake env */\n"
                (call "MALLOC" "IMM(1)")
                "MOV(IND(R0) , IMM(-666));\n"
                "MOV(R1 , R0);\n"
                (call "MALLOC" "IMM(1)")
                "MOV(IND(R0) , R1);\n"
                "PUSH(R0); /* fake env */\n"
                "PUSH(IMM(496351)); /* fake return addr */\n"
                "PUSH(FP); /*fake old fp */\n"
                "MOV(FP , SP);\n"
                "INFO;\n"
                "SHOW(\"after stack setup\" , R0);\n"
              )
             )
           (generated-code (fold-left
                                 (lambda (gend-code pe)
                                     (let* ((l-skip-printing (lgen "skip_printing"))
                                            (printer_string
                                              (string-append
                                                "CMP(R0 , IMM(SOB_VOID));\n"
                                                "JUMP_EQ(" l-skip-printing ");\n"
                                                "PUSH(R0);\nCALL(WRITE_SOB); //Printing [[E]] as requested \nDROP(1);\nCALL(NEWLINE);\n"
                                                l-skip-printing ":\n\n"
                                              )))
                                     (string-append "\n" gend-code ((code-gen constants_objects fvars_objects symbols) pe) printer_string)))
                                 ""
                                 parsed-and-optimized-code
                             ))
           (output (string-append
                      prologue
                      (allocate_constant_table constants)
                      (allocate_fvar_table fvars_list)
                      (allocate_symbol_table symbols)
                      runtime_cisc_string
                      set_up_stack
                      generated-code
                      epilogue
                    )))
            (string->file output trgt)
          )))

;;Run-Time support and primitive functions
(define prologue
  (string-append
  "#include <stdio.h>\n"
  "#include <stdlib.h>\n"
  "#include <string.h>\n"
  "\n/* change to 0 for no debug info to be printed: */\n"
  "#define DO_SHOW 0\n"
  "\n#include \"arch/cisc.h\"\n"
  "#define SOB_VOID 2\n"
  "#define SOB_NIL 3\n"
  "#define SOB_TRUE 4\n"
  "#define SOB_FALSE 6\n"
  "#define SOB_INTEGER_ZERO 8\n"
  "#define SOB_INTEGER_ONE 10\n"
  "#define SOB_CHAR_x0 12\n"
  "\nint main()\n"
  "{\n"
  "START_MACHINE;\n"
  "JUMP(CONTINUE);\n"
    "#include \"arch/char.lib\"\n"
    "#include \"arch/io.lib\"\n"
    "#include \"arch/math.lib\"\n"
    "#include \"arch/string.lib\"\n"
    "#include \"arch/system.lib\"\n"
    "#include \"arch/scheme.lib\"\n"
  "CONTINUE:\n"))


(define epilogue
  (string-append

  "JUMP(L_return);\n"
  "L_error_invalid_args_list:\n"
  (string-append
    ;"SHOW(\"ERROR INVALID ARGS LIST \" , FPARG(1));\n"
    ;"INFO;\n"
    "JUMP(L_return);\n"
  )
  "L_error_args_count:\n"
  (string-append
    ;"SHOW(\"ERROR INVALID ARGS COUNT \" , FPARG(1));\n"
    ;"INFO;\n"
    "JUMP(L_return);\n"
  )
  "L_error_incorr_type:\n"
  (string-append
    ;"SHOW(\"ERROR INCORRECT TYPE \" , R0);\n"
    ;"INFO;\n"
    "JUMP(L_return);\n"
  )
  "L_error_cannot_apply_non_clos:\n"
  (string-append
    ;"SHOW(\"ERROR APPLY NON CLOS \" , R0);\n"
    ;"INFO;\n"
    "JUMP(L_return);\n"
  )
  "L_error_lambda_opt_insufficient_args_count:\n"
  (string-append
    ;"SHOW(\"ERROR LAMBDA OPT INSUFFICIENT ARGS \" , FPARG(1));\n"
    ;"INFO;\n"
    "JUMP(L_return);\n"
  )
  "L_return:\n"
  "\nSTOP_MACHINE;\n"
  "return 0;\n"
  "}\n"
  )
)
(define runtime_scheme_string
  (string-append

    "\n\n(define cadr (lambda (z) (car (cdr z))))
    (define cdar (lambda (z) (cdr (car z))))
    (define cddr (lambda (z) (cdr (cdr z))))
    (define caar (lambda (z) (car (car z))))
    (define cadar (lambda (z) (car (cdr (car z)))))
    (define cdaar (lambda (z) (cdr (car (car z)))))
    (define cddar (lambda (z) (cdr (cdr (car z)))))
    (define caaar (lambda (z) (car (car (car z)))))
    (define caddr (lambda (z) (car (cdr (cdr z)))))
    (define cdadr (lambda (z) (cdr (car (cdr z)))))
    (define cdddr (lambda (z) (cdr (cdr (cdr z)))))
    (define caadr (lambda (z) (car (car (cdr z)))))
    (define cadaar (lambda (z) (car (cdr (car (car z))))))
    (define cdaaar (lambda (z) (cdr (car (car (car z))))))
    (define cddaar (lambda (z) (cdr (cdr (car (car z))))))
    (define caaaar (lambda (z) (car (car (car (car z))))))
    (define caddar (lambda (z) (car (cdr (cdr (car z))))))
    (define cdadar (lambda (z) (cdr (car (cdr (car z))))))
    (define cdddar (lambda (z) (cdr (cdr (cdr (car z))))))
    (define caadar (lambda (z) (car (car (cdr (car z))))))
    (define cadadr (lambda (z) (car (cdr (car (cdr z))))))
    (define cdaadr (lambda (z) (cdr (car (car (cdr z))))))
    (define cddadr (lambda (z) (cdr (cdr (car (cdr z))))))
    (define caaadr (lambda (z) (car (car (car (cdr z))))))
    (define cadddr (lambda (z) (car (cdr (cdr (cdr z))))))
    (define cdaddr (lambda (z) (cdr (car (cdr (cdr z))))))
    (define cddddr (lambda (z) (cdr (cdr (cdr (cdr z))))))
    (define caaddr (lambda (z) (car (car (cdr (cdr z))))))\n\n"

  ;;vector
  "(define vector
      (lambda args
        (let ((length 0))
        (map (lambda (x) (set! length (+ 1 length))) args)
          (letrec ((vec (make-vector length))
                  (rec_set
                    (lambda (i vals)
                      (if (< i length)
                          (begin (vector-set! vec i (car vals))
                                 (rec_set (+ i 1) (cdr vals)))
                          vec
                      ))))
          (rec_set 0 args)
    ))))"

  ;;map
  ;"(define map
  ;  (lambda (func lst)
  ;      (letrec ((rec (lambda (lst1)
  ;                          (if (null? lst1)
  ;                              lst1
  ;                              `(,(func (car lst1)) ,@(rec (cdr lst1)))
  ;                          ))
  ;              ))
  ;              (rec lst)
  ;  )))"
  "(define map
    (lambda (func lst)
        (if (null? lst)
            lst
            `(,(func (car lst)) ,@(map func (cdr lst)))
          )))"

  ;;list
  "(define list
    (lambda args args))"

  ;;zero?
  "(define zero?
    (lambda (val)
        (if (number? val)
            (= 0 val))))"

  ;;number?
  "(define number? rational?)"

  ;;append
  "(define append
      (lambda args
          (letrec ((f (lambda(ls args)
                        (if (null? args)
                          ls
                          (letrec ((g (lambda(ls)
                                        (if (null? ls)
                                            (f (car args) (cdr args))
                                            (cons (car ls) (g (cdr ls)))))))
                                            (g ls)
                                        )))))
                        (f '() args))))"

  ;not
  "(define not
    (lambda (val)
      (if val #f #t)))"


  ))
