DUMBO
-----

A dumb, fast, single inheritance, single dispatch object system.

Motivation
----------

I needed a simple, fast, hackable object system for lambdanative and decided to write my own.

Examples
--------


    (include "./modules/dumbo/dumbo.scm")

    (define (log #!rest args)
      (display args)
      (newline))

    (define testdo (make-dumbo-object))

    (log "Dumbo object: " (make-dumbo-object))

    (define-class person 
           super-class: dumbo-object
           members: 
           ((define first-name "")
            (define last-name ""))
           methods:
           ((define (get-name person)
              (string-append 
               (get-first-name person)
               " "
               (get-last-name person)))
            (define (get-designator person)
              (get-name person))))


    (define a-person (make-person first-name: "Vincent" last-name:     "Toups"))
    (log "instance" a-person)
    ;; (set-first-name! a-person "Vincent")
    ;; (set-last-name! a-person "Toups")
    (newline)
    (log (get-name a-person))
    (log "Person Designator: " (get-designator a-person))

    (define-class employee
      super-class: person
      members:
      ((define id "000000"))
      methods:
      ((define (get-designator employee)
         (string-append
          (get-id employee)
          " : "
          (super-method employee)))))

    (define an-employee (make-employee))
    (set-first-name! an-employee "John")
    (set-last-name! an-employee "Suits")
    (set-id! an-employee "ABC123")

    (log "Employee Designator: " (get-designator an-employee)) 

    (log "employee is person? "(person? an-employee))
    (log "person is employee? "(employee? a-person))

    (define-class hero super-class: person
      members:
      ((define kenning ""))
      methods:
      ((define (get-designator hero)
         (string-append (get-name hero) " " (get-kenning hero)))))

    (define a-hero (make-hero first-name: "Hrothgar" last-name: "Bornhold"  kenning: "The Kingslayer"))

    (log "hero desig " (get-designator a-hero))




