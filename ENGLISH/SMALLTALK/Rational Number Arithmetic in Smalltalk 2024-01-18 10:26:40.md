```smalltalk

class Rational

    class Variable
        "A variable in an expression. The variable name is represented as
    a string."

        initialize: name
            "Create a new variable with the given name."

            name := name asString.
            super initialize.
            "Store the name of the variable, as a string."
            name := name.

        name
            "Answer the name of this variable."

            ^name
    "Class methods for Variable."

    contentsCopy: other
        "Answer a copy of the receiver, with the copy containing
    contents that are copied from other."

        ^Variable new initialize: (other name).

    objectCopy: other
        "Answer a copy of the receiver, with the copy containing
    contents that are copies of the contents of other."

        ^Variable new initialize: (other name).

    variableWithString: aString
        "Answer a new variable whose name is the given string."

        ^Variable new initialize: aString
    "Instance methods for Variable."

    isVariable
        "Answer true."

        ^true

    print
        "Print the name of this variable."

        Transcript show: (self name)
    "End of class Variable."

"Class methods for Rational."

dot: integer numerator denominator
        "Answer the value integer.numerator/denominator.
    (This method implements the 'dot' syntax for fixed-point
    numbers.)"

        ^Rational new initialize: numerator denominator * integer
"End of class methods for Rational."

"Instance methods for Rational."

   * aRational
        "Answer the product of the receiver and aRational."

        ^Rational new initialize: (numerator * aRational numerator)
        (denominator * aRational denominator)

        ^self

    + aRational
        "Answer the sum of the receiver and aRational."

        ^Rational new initialize: (numerator * aRational denominator) +
        (denominator * aRational numerator)
        denominator

    - aRational
        "Answer the receiver minus aRational."

        ^Rational new initialize: (numerator * aRational denominator) -
        (denominator * aRational numerator)
        denominator

    / aRational
        "Answer the receiver divided by aRational."

        ^Rational new initialize: numerator * aRational denominator
        denominator * aRational numerator

    % aRational
        "Answer the remainder of the receiver divided by aRational."

        ^(numerator * aRational denominator) % (denominator * aRational numerator)

    abs
        "Answer the absolute value of the receiver."

        ^(self < 0) ifTrue: [^self negated] ifFalse: [^self]

    asFloat
        "Answer the receiver converted to a float."

        ^numerator / denominator asFloat

    compare: aRational
        "Answer 1 if the receiver is greater than aRational; 0 if
    they are equal; -1 if the receiver is less."

        | result |
        result := (numerator * aRational denominator) -
        (denominator * aRational numerator).
        result > 0 ifTrue: [^1].
        result < 0 ifTrue: [^-1].
        ^0

    denominator
        "Answer the denominator of this rational number."

        ^denominator

    display
        "Print the receiver."

        Transcript show: numerator.
        Transcript show: '/'.
        Transcript show: denominator.

    floor
        "Answer the floor of the receiver."

        ^trunc

    isInteger
        "Answer true if the receiver is an integer, false otherwise."

        ^denominator = 1

    isNonNegative
        "Answer true if the receiver is non-negative, false otherwise."

        ^numerator >= 0

    isZero
        "Answer true if the receiver is zero, false otherwise."

        ^numerator = 0

    negated
        "Answer the receiver negated."

        ^Rational new initialize: -numerator denominator

    numerator
        "Answer the numerator of this rational number."

        ^numerator

    < aRational
        "Answer true if the receiver is less than aRational, false
    otherwise."

        ^self compare: aRational = -1

    <= aRational
        "Answer true if the receiver is less than or equal to aRational,
    false otherwise."

        ^(self compare: aRational) <= 0

    >= aRational
        "Answer true if the receiver is greater than or equal to aRational,
    false otherwise."

        ^(self compare: aRational) >= 0

    > aRational
        "Answer true if the receiver is greater than aRational, false
    otherwise."

        ^self compare: aRational = 1

    integerDivide: aRational
        "Answer the integer quotient of the receiver divided by aRational."

        ^trunc / (aRational trunc)

    print
        "Print the receiver."

        self display

    round: precision
        "Answer the receiver rounded to the given precision, if the
    precision is a multiple of 10. Otherwise, raise an error."

        ^precision // 10 = 0
            ifTrue: [self roundToMultipleOf: precision]
            ifFalse: [self error: 'Precision must be a multiple of 10']

    roundToMultipleOf: precision
        "Answer the receiver rounded to the nearest multiple of precision."

        | adjuster |
        adjuster := 1.0 / precision.
        ^self multiplyBy: adjuster.
        round.
        multiplyBy: adjuster reciprocal.

    roundUpToMultipleOf: precision
        "Answer the receiver rounded up to the nearest multiple of precision."

        | adjuster |
        adjuster := 1.0 / precision.
        ^self multiplyBy: adjuster.
        ceil.
        multiplyBy: adjuster reciprocal.

    roundDownToMultipleOf: precision
        "Answer the receiver rounded down to the nearest multiple of precision."

        | adjuster |
        adjuster := 1.0 / precision.
        ^self multiplyBy: adjuster.
        floor.
        multiplyBy: adjuster reciprocal.

    squared
        "Answer the square of the receiver."

        ^self * self

    sqrt
        "Answer the square root of the receiver. Returns a float if the
    result is not an integer."

        | guess prevGuess |
        guess := self.
        prevGuess := 0.0.
        [guess ~= prevGuess] whileTrue: [
            prevGuess := guess.
            guess := guess / 2 + guess / (self asFloat)
        ].
        ^guess asInteger asFloat

    toString
        "Answer the string representation of the receiver."

        ^numerator asString , '/' , denominator asString

    trunc
        "Answer the integer part of the receiver."

        ^numerator // denominator

    "End of instance methods for Rational."
end

"Demonstration of the Rational class."

Rational new initialize: 2 5.
Rational new initialize: 3 7.

"Print the two rational numbers."

Transcript spcr; show: 'Rational numbers:'; cr.
Transcript show: (Rational new initialize: 2 5).
Transcript cr.
Transcript show: (Rational new initialize: 3 7).
Transcript spcr.

"Perform some operations on the two rational numbers and print the results."

Transcript spcr; show: 'Sum:'; cr.
Transcript show: (Rational new initialize: 2 5) + (Rational new initialize: 3 7).
Transcript spcr.

Transcript show: 'Difference:'; cr.
Transcript show: (Rational new initialize: 2 5) - (Rational new initialize: 3 7).
Transcript spcr.

Transcript show: 'Product:'; cr.
Transcript show: (Rational new initialize: 2 5) * (Rational new initialize: 3 7).
Transcript spcr.

Transcript show: 'Division:'; cr.
Transcript show: (Rational new initialize: 2 5) / (Rational new initialize: 3 7).
Transcript spcr.

Transcript show: 'Modulo:'; cr.
Transcript show: (Rational new initialize: 2 5) % (Rational new initialize: 3 7).
Transcript spcr.

Transcript show: 'Negated:'; cr.
Transcript show: (Rational new initialize: 2 5) negated.
Transcript spcr.

Transcript show: 'Floor:'; cr.
Transcript show: (Rational new initialize: 2 5) floor.
Transcript spcr.

Transcript show: 'Approximation:'; cr.
Transcript show: (Rational new initialize: 2 5) asFloat.
Transcript spcr.

```

This code implements a `Rational` class, which represents rational numbers as a pair of integers, the numerator and denominator. It provides various operations on rational numbers, such as addition, subtraction, multiplication, and division, as well as methods to access the numerator and denominator, compare rational numbers, and convert them to strings or floats.

The `Variable` class represents a variable in an expression. It has a name, which is stored as a string, and methods to print the name and check if the variable is a variable.

The `Rational` class provides a number of methods for working with rational numbers. These include methods for adding, subtracting, multiplying, and dividing rational numbers, as well as methods for finding the absolute value, negating a rational number, finding the floor and ceiling of a rational number, and converting a rational number to a string or float.

The `Variable` class provides methods for printing the name of the variable and checking if the variable is a variable.

The code also includes a demonstration of the `Rational` class, which creates two rational numbers, performs various operations on them, and prints the results.