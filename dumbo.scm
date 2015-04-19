(define-macro (at-expand-time #!rest exprs)
  (eval `(begin ,@exprs))
  `(begin #f))

(define-macro (at-expand-time-and-run-time #!rest exprs)
  `(begin 
	 (at-expand-time ,@exprs)
	 (begin ,@exprs)))

(define-type -members-of-dumbo-object 
  extender: define-type-of--members-of-dumbo-object)
(define-type -methods-of-dumbo-object
  extender: define-type-of--methods-of-dumbo-object
  (dumbo-init 
   -methods-of-dumbo-object-dumbo-init
   -methods-of-dumbo-object-dumbo-init-set!
   init: #f))
(define-type dumbo-object 
  extender: define-type-of-dumbo-object
  constructor: -make-dumbo-object
  (members unprintable:) 
  (methods unprintable:)
  (member-indirection unprintable:)
  (method-indirection unprintable:))

(define (make-dumbo-object)
  (-make-dumbo-object
   (make--members-of-dumbo-object)
   (make--methods-of-dumbo-object)
   (make-table test: eq?)
   (make-table test: eq?)))

(at-expand-time-and-run-time 
 (define (concat-symbols delim #!rest args)
   (let ((delim (symbol->string delim))) 
	 (string->symbol
	  (reduce (lambda (symbol acc-string)
				(string-append acc-string delim (symbol->string symbol)))
			  (cdr args)
			  (symbol->string (car args))))))
 (define (create-global-setter-name field-name)
   (concat-symbols (string->symbol "") 
				   (concat-symbols '- 'set field-name)
				   '!)))

(define (-dumbo-object-dumbo-init instance #!rest args)
  (let ((member-indirection (dumbo-object-member-indirection instance))
		(members (dumbo-object-members instance))) 
	(let loop 
		((args args)
		 (args-count (length args)))
	  (cond 
	   ((= 0 args-count) instance)
	   ((= 1 args-count) (error '(create-constructor-name class) "Odd number of init args should be key/value pairs."))
	   (else 
		(let ((key (create-global-setter-name (dumbo-thing->symbol (car args))))
			  (val (cadr args))
			  (args (cddr args)))
		  ((table-ref member-indirection key) members val)
		  (loop args (- args-count 2)))))))
  instance)

(define (-init-dumbo-object-method-table table)  
  (-methods-of-dumbo-object-dumbo-init-set! table -dumbo-object-dumbo-init)
  table)
(define (-init-dumbo-object-member-table table)
  table)

(define (dumbo-init instance #!rest args)
  (apply (-methods-of-dumbo-object-dumbo-init (dumbo-object-methods instance))
		 instance args))



(at-expand-time

 (define (merge-tables #!rest tables)
   (define (helper remaining out)
	 (cond ((eq? remaining '())
			out)
		   (else 
			(table-merge out (car remaining))
			(helper (cdr remaining) out))))
   (helper tables (make-table test: eq?)))

 (define (filter f lst #!optional (acc (list)))
   (cond 
	((eq? lst '()) (reverse acc))
	(else 
	 (let ((head (car lst))
		   (tail (cdr lst)))
	   (cond
		((f head) (filter f tail (cons head acc)))
		(else 
		 (filter f tail acc)))))))

 (define (nth-from-list lst n or-val)
   (cond 
	((eq? lst '()) or-val)
	((= n 0) (car lst))
	(else (nth-from-list (cdr lst) (- n 1) or-val))))

 (define (empty? lst)
   (eq? lst '()))

 (define (reduce f lst init)
   (cond 
	((empty? lst) init)
	(else 
	 (reduce f (cdr lst) (f (car lst) init)))))

  (define (symbol-list->table lst)
	(define (helper tbl lst)
	  (cond ((empty? lst)
			 tbl)
			(else 
			 (table-set! tbl (car lst) #t)
			 (helper tbl (cdr lst)))))
	(helper (make-table test: eq? init: #f)
			lst))

  (define (table->symbol-list tbl)
	(let ((output '()))
	  (table-for-each (lambda (k v)
						(set! output (cons k output)))
					  tbl)
	  output))


 (define dumbo-class-info (make-table test: eq?))
 (define dumbo-method-info (make-table test: eq?))
 (define dumbo-member-info (make-table test: eq?))

 (table-set! dumbo-method-info 'dumbo-init #t)

 (define (global-method-exists? method-name)
   (table-ref dumbo-method-info method-name))

 (define (global-accessors-exist? member-name)
   (table-ref dumbo-member-info member-name))

 (define-type dumbo-class-info constructor: make-dumbo-class-info* super-class member-names method-names)
 (define (make-dumbo-class-info super-class member-names method-names)
   (make-dumbo-class-info* super-class 
						   (symbol-list->table member-names)
						   (symbol-list->table method-names)))



 (define (dumbo-class-direct-members class-name)
   (dumbo-class-info-member-names (table-ref dumbo-class-info class-name)))

 (define (dumbo-class-direct-methods class-name)
   (dumbo-class-info-method-names (table-ref dumbo-class-info class-name)))

 (define (dumbo-class-has-method-directly? class-name method-name)
   (table-ref (dumbo-class-direct-methods class-name) method-name))

 (define (dumbo-class-has-member-directly? class-name member-name)
   (table-ref (dumbo-class-direct-members class-name) member-name))

 (define (dumbo-class-direct-superclass class-name)
   (dumbo-class-info-super-class (table-ref dumbo-class-info class-name)))

 (define (calculate-super-method class-name method-name)
   (define (helper class method-name)
	 (cond 
	  ((eq? class #f) '#f)
	  ((dumbo-class-has-method-directly? class method-name)
	   (create-obfuscated-method-name class method-name))
	  (else 
	   (helper (dumbo-class-direct-superclass class) method-name))))
   (let ((result (helper (dumbo-class-direct-superclass class-name) method-name)))
	 result))

 (define (dumbo-class-which-declared-method class-name method)
   (define (helper class-name method class-so-far)
	 (cond ((eq? #f class-name) class-so-far)
		   ((dumbo-class-has-method-directly? class-name method)
			(helper (dumbo-class-direct-superclass class-name) method class-name))
		   (else 
			(helper (dumbo-class-direct-superclass class-name) method class-so-far))))
   (helper class-name method #f))

 (define (dumbo-class-which-declared-member class-name member)
   (define (helper class-name member class-so-far)
	 (cond ((eq? #f class-name) class-so-far)
		   ((dumbo-class-has-member-directly? class-name member)
			(helper (dumbo-class-direct-superclass class-name) member class-name))
		   (else 
			(helper (dumbo-class-direct-superclass class-name) member class-so-far))))
   (helper class-name member #f))

 (define (calculate-method-slot-name class-name method)
   (let ((declaring-class (dumbo-class-which-declared-method class-name method)))
	 (cond 
	  ((eq? declaring-class #f) (error (string-append 
										"Tried to construct a method slot name for class "
										(symbol->string class-name) " method "
										(symbol->string method)
										" but no class or super class implements that method.")))
	  (else 
	   (create-method-getter declaring-class method)))))

 (define (dumbo-class-defining-method class-name method-name)
   (cond 
	((eq? class-name #f) #f)
	((dumbo-class-has-method-directly? class-name method-name)
	 class-name)
	(else 
	 (dumbo-class-defining-method (dumbo-class-direct-superclass class-name) method-name))))

 (define (dumbo-class-members class-name #!optional (names '()))
   (cond ((eq? class-name #f)
		  names)
		 (else 
		  (dumbo-class-members
		   (dumbo-class-direct-superclass class-name)
		   (append names (table->symbol-list (dumbo-class-direct-members class-name)))))))

 (define (dumbo-class-methods class-name #!optional (names '()))
   (cond ((eq? class-name #f)
		  names)
		 (else 
		  (dumbo-class-methods
		   (dumbo-class-direct-superclass class-name)
		   (append names (table->symbol-list (dumbo-class-direct-methods class-name)))))))

 (define (dumbo-class-members-with-class class-name #!optional (names '()))
   (define (add-class-to-symbol-list class lst)
	 (map (lambda (s)
			(list class s))
		  lst))
   (cond ((eq? class-name #f)
		  names)
		 (else 
		  (dumbo-class-members-with-class
		   (dumbo-class-direct-superclass class-name)
		   (append names 
				   (add-class-to-symbol-list class (table->symbol-list (dumbo-class-direct-members class-name))))))))

 (define (dumbo-class-methods-with-class class-name #!optional (names '()))
   (define (add-class-to-symbol-list class lst)
	 (map (lambda (s)
			(list class s))
		  lst))
   (cond ((eq? class-name #f)
		  names)
		 (else 
		  (dumbo-class-methods-with-class
		   (dumbo-class-direct-superclass class-name)
		   (append names 
				   (add-class-to-symbol-list (table->symbol-list (dumbo-class-direct-methods class-name))))))))


 (define (dumbo-class-member-getters-and-setters class-name #!optional (names '()))
   (define (make-getter-and-setter symbol)
	 (list symbol (create-member-getter class-name symbol)
		   (create-member-setter class-name symbol)))
   (cond ((eq? class-name #f)
		  names)
		 (else 
		  (dumbo-class-member-getters-and-setters
		   (dumbo-class-direct-superclass class-name)
		   (append names 
				   (map make-getter-and-setter (table->symbol-list (dumbo-class-direct-members class-name))))))))

 (define (dumbo-class-method-getters-and-setters class-name #!optional (names '()))
   (define (make-getter-and-setter symbol)
	 (list symbol (create-method-getter class-name symbol)
		   (create-method-setter class-name symbol)))
   (cond ((eq? class-name #f)
		  names)
		 (else 
		  (dumbo-class-method-getters-and-setters
		   (dumbo-class-direct-superclass class-name)
		   (append names 
				   (map make-getter-and-setter (table->symbol-list (dumbo-class-direct-methods class-name))))))))

 (define (create-member-indirection-table-builder-name class-name)
   (concat-symbols '- the-empty-symbol class-name 'member-indirection-builder))

 (define (create-method-indirection-table-builder-name class-name)
   (concat-symbols '- the-empty-symbol class-name 'method-indirection-builder))

 ;; (define (create-global-setter-name field-name)
 ;;   (concat-symbols the-empty-symbol 
 ;; 				   (concat-symbols '- 'set field-name)
 ;; 				   '!))

 (define (create-global-getter-name field-name)
   (concat-symbols '- 'get field-name))

 (define (create-global-method-name method-name)
   method-name)

 (define (create-member-indirection-table-builder class-name)
   (let ((information (dumbo-class-member-getters-and-setters class-name))
		 (tbl (gensym 'tbl)))
	 `(define (,(create-member-indirection-table-builder-name class-name))
		(let ((,tbl (make-table test: eq?)))
			   ,@(let loop ((getters-and-setters information)
							(forms '()))
				   (if (empty? getters-and-setters)
					   (reverse forms)
					   (let* ((first (car getters-and-setters))
							  (rest (cdr getters-and-setters))
							  (field-name (car first))
							  (global-setter (create-global-setter-name field-name))
							  (global-getter (create-global-getter-name field-name))
							  (local-setter (create-member-setter (dumbo-class-which-declared-member class-name field-name) field-name))
							  (local-getter (create-member-getter (dumbo-class-which-declared-member class-name field-name) field-name)))
						 (loop rest
							   (cons `(table-set! ,tbl ',global-setter ,local-setter)
									 (cons `(table-set! ,tbl ',global-getter ,local-getter) forms))))))
			   ,tbl))))

 (define (create-method-indirection-table-builder class-name)
   (let ((tbl (gensym 'tbl))
		 (methods-implemented (dumbo-class-methods class-name)))
	 `(define (,(create-method-indirection-table-builder-name class-name))
		(let ((,tbl (make-table test: eq?)))
		  ,@(let loop ((rest-methods-implemented methods-implemented)
					   (forms '()))
			  (cond 
			   ((empty? rest-methods-implemented)
				(reverse forms))
			   (else 
				(let* ((method-name (car rest-methods-implemented))
					   (rest (cdr rest-methods-implemented))
					   (global-getter (create-global-method-name method-name))
					   (local-getter (create-method-getter (dumbo-class-which-declared-method class-name method-name)
														   method-name)))
				  (loop rest 
						(cons `(table-set! ,tbl ',global-getter ,local-getter) forms))))))
			   ,tbl))))

;; ,@(let loop ((getters-and-setters information)
;; 							(forms '()))
;; 				   (if (empty? getters-and-setters)
;; 					   (reverse forms)
;; 					   (let* ((first (car getters-and-setters))
;; 							  (rest (cdr getters-and-setters))
;; 							  (field-name (car first))
;; 							  (global-getter (create-global-method-name field-name))
;; 							  (local-getter (create-method-getter class-name field-name)))
;; 						 (loop rest
;; 							   (cons `(table-set! ,tbl ',global-getter ,local-getter) forms)))))

 (define (dumbo-class-has-member? class-name member-name)
   (cond 
	((eq? class-name #f) #f)
	(else 
	 (if (table-ref (dumbo-class-direct-members class-name) member-name)
		 #t
		 (dumbo-class-has-member? (dumbo-class-direct-superclass class-name) member-name)))))

 (define (dumbo-class-has-method? class-name method-name)
   (cond 
	((eq? class-name #f) #f)
	(else 
	 (if (table-ref (dumbo-class-direct-methods class-name) method-name)
		 #t
		 (dumbo-class-has-method? (dumbo-class-direct-superclass class-name) method-name)))))

 (define (dumbo-class-super-classes class-name #!optional (names '()))
   (cond ((eq? class-name #f)
		  (reverse names))
		 (else
		  (let ((super-class (dumbo-class-direct-superclass class-name))) 
			(dumbo-class-super-classes super-class
									   (cons super-class names))))))

 (define (add-dumbo-class-info class-name super-class members methods)
   (define (not-in-table table)
	 (lambda (k) (not (table-ref table k #f))))
   (let* ((super-class-methods (symbol-list->table (dumbo-class-methods super-class)))
		  (super-class-members (symbol-list->table (dumbo-class-members super-class)))
		  ;; (methods (filter (not-in-table super-class-methods) methods))
		  ;; (members (filter (not-in-table super-class-members) members))
		  ) 
	 (if (table-ref dumbo-class-info class-name #f)
		 (error (string-append "A dumbo class called " (symbol->string class-name) " already exists."))
		 (table-set! dumbo-class-info class-name (make-dumbo-class-info super-class members methods)))))
 
 (add-dumbo-class-info 'dumbo-object #f (list) (list 'dumbo-init))

 ;; (define (concat-symbols delim #!rest args)
 ;;   (let ((delim (symbol->string delim))) 
 ;; 	 (string->symbol
 ;; 	  (reduce (lambda (symbol acc-string)
 ;; 				(string-append acc-string delim (symbol->string symbol)))
 ;; 			  (cdr args)
 ;; 			  (symbol->string (car args))))))

 (define (make-members-struct-name class-name)
   (concat-symbols '- '-members-of class-name))
 
 (define (make-methods-struct-name class-name)
   (concat-symbols '- '-methods-of class-name))

 (define (make-methods-struct-maker-name class-name)
   (concat-symbols the-empty-symbol 'make (make-methods-struct-name class-name)))

 (define (make-members-struct-maker-name class-name)
   (concat-symbols the-empty-symbol 'make (make-members-struct-name class-name)))

 (define (create-members-define-type-name class-name super-class)
   (concat-symbols '- 'define-type-of (make-members-struct-name super-class)))

 (define (create-members-table-constructor class-name)
   (concat-symbols '- 'make (make-members-struct-name class-name)))

 (define (create-methods-define-type-name class-name super-class)
   (concat-symbols the-empty-symbol 'define-type-of (make-methods-struct-name super-class)))

 ;; (define (create-methods-table-subtype-declaration class-name super-class)
 ;;   (,(create-methods-define-type-name super-class) (make-methods-struct-name class-name)
 ;; 	extender: (create-methods-define-type-name class-name)
 ;; 	,@()))

 (define (create-methods-table-constructor class-name)
   (concat-symbols '- 'make (make-methods-struct-name class-name)))

 (define (create-extender-name class-name)
   (concat-symbols '- 'define-type-of class-name))

 (define (create-method-table-extender-name class-name)
   (create-extender-name (concat-symbols '- '-methods-of class-name)))

 (define (create-member-table-extender-name class-name)
   (create-extender-name (concat-symbols '- '-members-of class-name)))


 (define (create-method-getter class-name method)
   (concat-symbols '- (make-methods-struct-name (dumbo-class-defining-method class-name method)) method))

 (define (create-method-setter class-name method)
   (concat-symbols '- (make-methods-struct-name class-name) method 'set!))

 (define (create-member-getter class-name member)
   (concat-symbols '- (make-members-struct-name class-name) member))

 (define (create-member-setter class-name member)
   (concat-symbols '- (make-members-struct-name class-name) member 'set!))

 (define (create-obfuscated-method-name class-name method-name)
   (concat-symbols '- the-empty-symbol class-name method-name))

 (define (create-appropriate-obfuscated-method-name class-name method-name)
   (let ((actual-class-name (class-defining-method class-name method-name)))
	 (if actual-class-name
		 (create-obfuscated-method-name actual-class-name method-name)
		 (error (string-append> 
				 "Couldn't find class implementing method "
				 (symbol->string method-name)
				 " for base class "
				 (symbol->string class-name)
				 ".")))))

 (define (create-obfuscated-member-init-name class-name member-name)
   (concat-symbols '- '- class-name member-name 'init))

 (define (create-appropriate-obfuscated-member-init-name class-name member-name)
   (let ((actual-class-name (class-defining-member class-name member-name)))
	 (if actual-class-name
		 (create-obfuscated-member-init-name actual-class-name member-name)
		 (error (string-append> 
				 "Couldn't find class implementing member "
				 (symbol->string member-name)
				 " for base class "
				 (symbol->string class-name)
				 ".")))))

 (define (create-method-table-name class-name)
   (concat-symbols '- '-methods-of class-name))

 (define (create-member-table-name class-name)
   (concat-symbols '- '-members-of class-name))


 (define (create-method-table-definition class-name super-class-name methods)
   `(,(create-method-table-extender-name super-class-name) ,(make-methods-struct-name class-name)
	 extender: ,(create-method-table-extender-name class-name)
	 ,@(let loop ((slot-defs '())
				  (methods methods))
		 (cond 
		  ((empty? methods) (reverse slot-defs))
		  ;; skip methods which already have slots
		  ((dumbo-class-has-method? super-class-name (car methods))			 
		   (loop slot-defs (cdr methods)))
		  (else 
		   (let ((method (car methods))
				 (methods (cdr methods))) 
			 (loop (cons `(,method ,(create-method-getter class-name method)
								   ,(create-method-setter class-name method)
								   init: #f)
						 slot-defs)
				   methods)))))))
 


 (define the-empty-symbol (string->symbol ""))

 (define (create-method-table-initializer-name class-name)
   (concat-symbols '- '-init class-name 'method-table))

 (define (create-method-table-name class-name)
   (concat-symbols '- the-empty-symbol class-name 'method-table))

 ;; method tables are shared between all objects of the same class
 ;; and should only be built once
 (define (create-method-table-initializer class-name super-class-name methods) 
   `(define (,(create-method-table-initializer-name class-name) table)
	  (,(create-method-table-initializer-name super-class-name) table)
	  ,@(let loop ((slot-setups '())
				   (methods methods))
		  (cond 
		   ((empty? methods) (reverse slot-setups))
		   (else 
			(let ((method (car methods))
				  (methods (cdr methods))) 
			  (loop (cons `(,(create-method-setter (dumbo-class-which-declared-method class-name method) method)
							table ,(create-obfuscated-method-name (dumbo-class-defining-method class-name method) method))
						  slot-setups)
					methods)))))))

 (define (create-member-table-definition class-name super-class-name members)
   `(,(create-member-table-extender-name super-class-name) ,(make-members-struct-name class-name)
	 extender: ,(create-member-table-extender-name class-name)
	 ,@(let loop ((slot-defs '())
				  (members members))
		 (cond 
		  ((empty? members) (reverse slot-defs))
		  ;; skip members which already have slots
		  ((dumbo-class-has-member? super-class-name (car members))
		   (loop slot-defs (cdr members)))
		  (else 
		   (let ((member (car members))
				 (members (cdr members))) 
			 (loop (cons `(,member ,(create-member-getter (dumbo-class-which-declared-member class-name member) member)
								   ,(create-member-setter (dumbo-class-which-declared-member class-name member) member)
								   init: #f)
						 slot-defs)
				   members)))))))
 
 (define (create-member-table-initializer-name class-name)
   (concat-symbols '- '-init class-name 'member-table))

 (define (create-member-table-name class-name)
   (concat-symbols '- the-empty-symbol class-name 'member-table))

 ;; each instance has a member table
 (define (create-member-table-initializer class-name super-class-name members)
   `(define (,(create-member-table-initializer-name class-name) table)
	  ;; init the table with the super class table
	  (,(create-member-table-initializer-name super-class-name) table)
	  ,@(let loop ((slot-setups '())
				   (members members))
		  (cond 
		   ((empty? members) (reverse slot-setups))
		   (else 
			(let ((member (car members))
				  (members (cdr members))) 
			  (loop (cons `(,(create-member-setter (dumbo-class-which-declared-member class-name member) member) table (,(create-obfuscated-member-init-name class-name member))) slot-setups)
					members)))))))

 (define (extract-method-names method-forms)
   (map (lambda (define-form)
		  (car (cadr define-form)))
		method-forms))

 (define (extract-member-names member-forms)
   (map (lambda (define-form)
		  (cadr define-form))
		member-forms))

 (define (create-hidden-constructor-name class-name)
   (concat-symbols '- '-make class-name))

 (define (create-constructor-name class-name)
   (concat-symbols '- 'make class-name))

 (define (create-wrapper-type-definition class super-class)
   `(,(create-extender-name super-class) ,class
	 constructor: ,(create-hidden-constructor-name class)
	 extender: ,(create-extender-name class)))

 (define (create-wrapper-maker-definition class)
   (let ((methods (gensym 'methods))
		 (members (gensym 'members))
		 (method-indirection (gensym 'method-indirection))
		 (member-indirection (gensym 'member-indirection))
		 (instance (gensym 'instance))
		 (args (gensym 'args))
		 (loop (gensym 'loop))
		 (args-count (gensym 'args-count))
		 (key (gensym 'key))
		 (val (gensym 'val)))
	 `(define ,(create-constructor-name class) 
		(let* ((,methods (,(create-methods-table-constructor class)))
			   (,method-indirection (,(create-method-indirection-table-builder-name class)))
			   (,member-indirection (,(create-member-indirection-table-builder-name class)))) 
		  (,(create-method-table-initializer-name class) ,methods)
		  (lambda (#!rest ,args)
			(let ((,members (,(create-members-table-constructor class))))
			  (,(create-member-table-initializer-name class) ,members)			   
			  (let ((,instance  (,(create-hidden-constructor-name class)
								,members
								,methods
								,member-indirection
								,method-indirection)))
				;; (let ,loop ((,args ,args)
				;; 			(,args-count (length ,args)))
				;; 	 (cond 
				;; 	  ((= 0 ,args-count) ,instance)
				;; 	  ((= 1 ,args-count) (error ',(create-constructor-name class) "Odd number of init args, should be key/value pairs."))
				;; 	  (else 
				;; 	   (let ((,key (create-global-setter-name (dumbo-thing->symbol (car ,args))))
				;; 			 (,val (cadr ,args))
				;; 			 (,args (cddr ,args)))
				;; 		 ((table-ref ,member-indirection ,key) ,members ,val)
				;; 		 (,loop ,args (- ,args-count 2))))))
				(apply dumbo-init ,instance ,args)
				,instance)))))))

 (define (create-getter-form class-name field-name)
   `(define (,(create-global-getter-name field-name) instance)
	  ((table-ref (dumbo-object-member-indirection instance) ',(create-global-getter-name field-name))
	   (dumbo-object-members instance))))

 (define (create-setter-form class-name field-name)
   `(define (,(create-global-setter-name field-name) instance new-value)
	  ((table-ref (dumbo-object-member-indirection instance) ',(create-global-setter-name field-name))
	   (dumbo-object-members instance)
	   new-value)))


 (define (create-accessor-forms class-name field-name)
   (if (eq? #f (table-ref dumbo-member-info field-name #f))
	   (begin
		 (table-set! dumbo-member-info field-name #t)
		 `(,(create-getter-form class-name field-name)
		   ,(create-setter-form class-name field-name)))
	   (list)))

 (define (create-all-accessor-forms class-name)
   (let ((member-names (table->symbol-list (dumbo-class-direct-members class-name))))
	 (let loop ((rest member-names)
				(forms (list)))
	   (if (empty? rest) forms
		   (loop (cdr rest)
				 (append (create-accessor-forms class-name (car rest))
						 forms))))))

 (define (create-global-method-definition method-name)
   (let ((args (gensym 'args))
		 (instance (gensym 'instance)))
	 `(define (,method-name ,instance #!rest ,args)
		(apply ((table-ref (dumbo-object-method-indirection ,instance) ',method-name)
				(dumbo-object-methods ,instance))
			   ,instance ,args))))

 (define (create-all-method-forms class-name)
   (let ((method-names (table->symbol-list (dumbo-class-direct-methods class-name))))
	 (let loop ((rest method-names)
				(forms '()))
		  (if (empty? rest)
			  forms
			  (let ((method-name (car rest))
					(rest (cdr rest))) 
				(cond 
				 ((eq? (table-ref dumbo-method-info method-name #f) #f)
				  (table-set! dumbo-method-info method-name #t)
				  (loop rest
						(cons (create-global-method-definition method-name)
							  forms)))
				 (else (loop rest forms))))))))

 (define (create-local-method-form class-name method)
   (let* ((head (car method))
		  (method-name (car (cadr method)))
		  (local-method-name (create-obfuscated-method-name class-name method-name))
		  (method-args (cdr (cadr method)))
		  (method-body (cddr method))
		  (super-class-name (dumbo-class-direct-superclass class-name))
		  (super-method-name (calculate-super-method class-name method-name)))
	 (if (not (eq? head 'define))
		 (error "Method definitions must follow `define (name arg ...) body`"))
	 `(define ,local-method-name 
		(let ((super-method ,super-method-name)) 
		  (lambda ,method-args ,@method-body)))))

 (define (create-all-local-method-forms class-name methods)
   (let loop ((rest methods)
			  (forms '()))
	 (if (empty? rest) forms
		 (loop (cdr rest)
			   (cons (create-local-method-form class-name (car rest))
					 forms)))))

 (define (create-local-member-init-form class-name form)
   (let* ((head (car form))
		  (name (cadr form))
		  (value-form (nth-from-list form 2 '#f))
		  (initer-name (create-obfuscated-member-init-name class-name name)))
	 `(define (,initer-name) ,value-form)))

 (define (create-all-local-member-init-forms class-name members)
   (let loop ((rest members)
			  (forms '())) 
	 (if (empty? rest)
		 forms
		 (loop (cdr rest)
			   (cons (create-local-member-init-form class-name (car rest))
					 forms)))))


)

(define (dumbo-thing->symbol thing)
  (cond 
   ((symbol? thing) thing)
   ((string? thing) (string->symbol thing))
   ((keyword? thing) (string->symbol (keyword->string thing)))))

(define-macro (define-class class-name #!key 
				(super-class 'dumbo-object)
				(members '()) 
				(methods '()))
  (let ((new-member-names (extract-member-names members))
		(new-method-names (extract-method-names methods))
		(old-method-names (dumbo-class-methods super-class)))
	(add-dumbo-class-info class-name super-class new-member-names new-method-names)
	(let ((expansion `(begin
			   ,@(create-all-accessor-forms class-name)
			   ,@(create-all-method-forms class-name)
			   ,@(create-all-local-method-forms class-name methods)
			   ,@(create-all-local-member-init-forms class-name members)
			   ,(create-method-table-definition class-name super-class (table->symbol-list (dumbo-class-direct-methods class-name)))
			   ,(create-method-indirection-table-builder class-name)
			   ,(create-method-table-initializer class-name super-class new-method-names)
			   ,(create-member-table-definition class-name super-class (table->symbol-list (dumbo-class-direct-members class-name)))
			   ,(create-member-indirection-table-builder class-name)
			   ,(create-member-table-initializer class-name super-class new-member-names)
			   ,(create-wrapper-type-definition class-name super-class)
			   ,(create-wrapper-maker-definition class-name)
			   )))
	  (pretty-print expansion) (newline)
	  expansion)))






