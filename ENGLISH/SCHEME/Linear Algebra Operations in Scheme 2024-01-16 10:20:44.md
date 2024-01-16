```scheme
(define (mult-matrix matrix1 matrix2)
  (cond ((null? matrix1) '())
        ((null? (car matrix1)) '())
        (else (cons (map (lambda (row) (dot-product row (car matrix2)))
                       (car matrix1))
                   (mult-matrix (cdr matrix1) (cdr matrix2))))))

(define (dot-product vec1 vec2)
  (cond ((null? vec1) 0)
        ((null? vec2) 0)
        (else (+ (* (car vec1) (car vec2))
                 (dot-product (cdr vec1) (cdr vec2))))))

(define (transpose matrix)
  (cond ((null? matrix) '())
        ((null? (car matrix)) '())
        (else (cons (map (lambda (row) (list-ref row 0))
                       matrix)
                   (transpose (map (lambda (row) (cdr row))
                                 matrix))))))

(define (inverse matrix)
  (cond ((null? matrix) '())
        ((null? (car matrix)) '())
        (else (/ 1 (det matrix))
              (transpose (cofactor matrix))))))

(define (cofactor matrix)
  (cond ((null? matrix) '())
        ((null? (car matrix)) '())
        (else (cons (map (lambda (row) (minor matrix row 0))
                       (car matrix))
                   (cofactor (cdr matrix))))))

(define (minor matrix row col)
  (cond ((null? matrix) '())
        ((null? (car matrix)) '())
        (else (map (lambda (row) (remove-at row col))
                 (cdr matrix))))))

(define (det matrix)
  (cond ((null? matrix) 0)
        ((null? (car matrix)) 0)
        (else (+ (map (lambda (row) (* (car row) (det (minor matrix 0 row))))
                   (car matrix))))))

(define (remove-at vec index)
  (cond ((null? vec) '())
        ((= index 0) (cdr vec))
        (else (cons (car vec) (remove-at (cdr vec) (- index 1))))))

(define (list-ref vec index)
  (cond ((null? vec) '())
        ((= index 0) (car vec))
        (else (list-ref (cdr vec) (- index 1)))))
```

This code implements a number of linear algebra operations in Scheme, including matrix multiplication, dot product, transpose, inverse, cofactor, minor, and determinant.

The `mult-matrix` function multiplies two matrices together. It uses a recursive algorithm to compute the dot product of each row of the first matrix with each column of the second matrix.

The `dot-product` function computes the dot product of two vectors. It uses a recursive algorithm to add the products of the corresponding elements of the vectors.

The `transpose` function transposes a matrix. It uses a recursive algorithm to construct a new matrix where the rows and columns of the original matrix are swapped.

The `inverse` function computes the inverse of a matrix. It uses the cofactor expansion formula to compute the adjoint of the matrix, and then divides the adjoint by the determinant of the matrix.

The `cofactor` function computes the cofactor of a matrix. It uses a recursive algorithm to construct a new matrix where the row and column of the element to be cofactored are removed.

The `minor` function computes the minor of a matrix. It uses a recursive algorithm to construct a new matrix where the specified row and column are removed.

The `det` function computes the determinant of a matrix. It uses a recursive algorithm to expand the determinant along the first row of the matrix.

The `remove-at` function removes the element at the specified index from a vector. It uses a recursive algorithm to construct a new vector with the element removed.

The `list-ref` function gets the element at the specified index from a vector. It uses a recursive algorithm to traverse the vector until it reaches the specified index.