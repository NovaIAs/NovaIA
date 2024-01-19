```scheme

(define-syntax make-struct
  (syntax-rules
    ((make-struct struct-name
                  struct-fields)
     (define (make-instance &rest args)
       (cons struct-name
             (if (null? args)
                 '()
                 args))))
    ((make-struct struct-name
                  struct-fields-list)
     (define (make-instance values)
       (cons struct-name
             (if (null? values)
                 '()
                 values))))))

(define-syntax def-struct
  (syntax-rules
    ((def-struct struct-name struct-constructors)
     (make-struct struct-name
                  (make-field-list struct-constructors)))))

(define-syntax make-field-list
  (syntax-rules
    (((_ . _ _) ..)
     (_))))

(define-syntax struct-constructor
  (syntax-rules
    (((_ _ ...) . _ _)
     (_ _ ...))))

(def-struct color
  ((red integer) (green integer) (blue integer)))

(define color-white
  (make-instance 255 255 255))

(define color-red
  (make-instance 255 0 0))

(define color-green
  (make-instance 0 255 0))

(define color-blue
  (make-instance 0 0 255))

```

This code defines a syntax for creating structs in Scheme. A struct is a data structure that consists of a fixed number of fields, each of which has a name and a type.

The first syntax rule defines a macro called `make-struct` that can be used to create a struct type. The `make-struct` macro takes two arguments: the name of the struct type and a list of field names. The `make-struct` macro creates a constructor function for the struct type. The constructor function takes a list of values as arguments, one for each field in the struct. The constructor function returns a new instance of the struct type.

The second syntax rule defines a macro called `def-struct` that can be used to define a struct type. The `def-struct` macro takes two arguments: the name of the struct type and a list of constructor functions. The `def-struct` macro creates a new struct type and a constructor function for each of the struct's constructors.

The third syntax rule defines a macro called `make-field-list` that can be used to create a list of field names. The `make-field-list` macro takes a list of field names and returns a list of symbols.

The fourth syntax rule defines a macro called `struct-constructor` that can be used to create a constructor function for a struct type. The `struct-constructor` macro takes a list of field names and a constructor body. The `struct-constructor` macro creates a new constructor function that takes a list of values as arguments, one for each field in the struct. The constructor function returns a new instance of the struct type.

The last few lines of code define four instances of the `color` struct type: `color-white`, `color-red`, `color-green`, and `color-blue`.