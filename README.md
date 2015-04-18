DUMBO
-----

A dumb, fast, single inheritance, single dispatch object system.

Motivation
----------

I needed a simple, fast, hackable object system for lambdanative and decided to write my own.

Information
-----------

Dumbo represents instances as a method and member table wrapped in a gambit `define-type` style type. A dumbo class then, is a `define-type` type and instances are `define-type` instances.  Methods are generic and global, and consult the instance's method table for dispatch (constant time, regardless of object hierarchy depth).  The method table is shared by all instances of a class.

Member tables are created on a per instance basis to store object state, but member look up and mutation is also constant time.  Getters and setters are global and generic.

Be careful not to stomp on other definitions that may be floating around globally.  Classes must appear in order if they depend on one-another.

Initial values for member fields are evaluated at instance creation time (and therefore may be evaluated repeatedly, so should be pure or non-pure as the case warrants) and can be overridden in subclasses. 

Inheritance is single but since setters, getters and methods are global, you can duck type.

Examples
--------


    ;; One must "include" dumbo because it is mostly compile-time
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




