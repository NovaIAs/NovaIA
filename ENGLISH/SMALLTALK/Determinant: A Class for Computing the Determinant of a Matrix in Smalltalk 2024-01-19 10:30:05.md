```smalltalk

{ Determine the determinant of a given matrix. }
Determinant := Class new.
Determinant classVarList: { }

Determinant>>initialize: aMatrix
[
    self matrix: aMatrix.
    self cachedDeterminant: nil
]

Determinant>>matrix: aMatrix
[
    matrix := aMatrix
]

Determinant>>matrix
[
    ^matrix
]

Determinant>>isSquareMatrix
[
    ^ self matrix size = self matrix first size
]

Determinant>>cachedDeterminant
[
    ^ cachedDeterminant
]

Determinant>>cachedDeterminant: det
[
    cachedDeterminant := det
]

Determinant>>value
[
    "If the matrix is not square, return an error message."
    self isSquareMatrix ifFalse: [
        ^ error('Matrix must be square to calculate the determinant')
    ].

    "If the matrix is 1x1, return the single value."
    (self matrix size = 1) ifTrue: [
        ^ self matrix first first
    ].

    "If the matrix is 2x2, return the determinant."
    (self matrix size = 2) ifTrue: [
        ^ (self matrix first first * self matrix second second) -
          (self matrix first second * self matrix second first)
    ].

    "If the cached determinant is available, return it."
    cachedDeterminant ifNotNil: [
        ^ cachedDeterminant
    ].

    "Calculate the determinant using the Laplace expansion."
    cachedDeterminant := 0.
    self matrix first do: [ :row |
        cachedDeterminant := cachedDeterminant +
            (row first * (self submatrixWithoutRow: row column: 1) value)
    ].

    ^ cachedDeterminant
]

Determinant>>submatrixWithoutRow: row column: column
[
    "Create a new matrix without the given row and column."
    Submatrix := Class new.
    Submatrix classVarList: { }

    Submatrix>>initialize: oldMatrix
    [
        self matrix: oldMatrix
    ]

    Submatrix>>matrix: aMatrix
    [
        matrix := aMatrix
    ]

    Submatrix>>matrix
    [
        ^ matrix
    ]

    Submatrix>>value
    [
        "Create a new matrix without the given row and column."
        newMatrix := OrderedCollection new.
        matrix do: [ :oldRow |
            oldRow do: [ :value |
                (oldRow != row and: [ value != column ]) ifTrue: [
                    newMatrix add: value
                ]
            ]
        ].

        ^ newMatrix asMatrix
    ]

    ^ Submatrix new: self matrix
]

```