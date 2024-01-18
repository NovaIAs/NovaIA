```smalltalk
"! Basic Definitions"
Object subclass: #VeryDifferentAndLarge [
    instanceVariableNames: 'size'
]

VeryDifferentAndLarge class methodsFor: 'creating' [
    :aSize -> [ self new size: aSize ]
]

VeryDifferentAndLarge >> size [ ^ size ]

VeryDifferentAndLarge >> size: aSize [
    size := aSize + (10 random: 100)
]

"! Random Definitions"
Object >> rangeRandom: aBegin to: aLast [
    ^ Random new next: (aLast - aBegin + 1) + aBegin
]

Object >> limitedRandom: aLowerBound to: anUpperBound [
    ^ self rangeRandom: aLowerBound to: anUpperBound + 1
]

"! 'word' Definitions"
String >> first [ ^ self substring: 1 to: 1 ]
String >> rest [ ^ self substring: 2 to: self size ]

String >> find: aString startingAt: aPos [
    "Return the position of the first occurrence of aString in self, starting
        the search at position aPos. If the substring is not found, return 0."
    [ self indexOf: aString after: aPos - 1 ] on: Error do: [ -> 0 ]
]

String >> capitalize [
    "Return a copy of self with its first letter capitalized and the rest in
        lowercase."
    (self first uppercase) , (self rest lowercase)
]

String >> capitalizeAll [
    "Return a copy of self with each word in lowercase and each first letter
        capitalized."
    (self split: ' ') collect: [ :word -> word capitalize ] join: ' '
]

String >> isCapitalized [
    "Return true if and only if self starts with a capital letter."
    self first isUpperCase
]

String >> isLowerCase [
    "Return true if and only if self starts with a lowercase letter."
    self first isLowerCase
]

String >> isLowercaseAll [
    "Return true if and only if all the letters in self are lowercase."
    self allSatisfy: [ :char -> char isLowerCase ]
]

String >> isUppercaseAll [
    "Return true if and only if all the letters in self are uppercase."
    self allSatisfy: [ :char -> char isUpperCase ]
]

String >> surroundWith: aString [ aString , self , aString ]
String >> surroundWith: aString and: anotherString [ aString , self , anotherString ]

String >> toNumber [
    "Return the number that self represents. If self does not represent a number,
        return 0."
    [ Number new value: self ] on: Error do: [ -> 0 ]
]

String >> isNumber [
    "Return true if and only if self represents a number."
    [ self toNumber ] on: Error do: [ -> false ]
]

String >> isAlpha [
    "Return true if and only if self contains only letters."
    self allSatisfy: [ :char -> char isLetter ]
]

String >> isAlphaNumeric [
    "Return true if and only if self contains only letters or digits."
    self allSatisfy: [ :char -> char isLetter or: [ char isDigit ] ]
]

String >> isDigit [
    "Return true if and only if self contains only digits."
    self allSatisfy: [ :char -> char isDigit ]
]

String >> isHexString [
    "Return true if and only if self contains only hexadecimal digits."
    self allSatisfy: [ :char -> char isDigit or: [ char isHexDigit ] ]
]

String >> isLatin1 [
    "Return true if and only if self contains only Latin-1 characters."
    self allSatisfy: [ :char -> char <= 255 ]
]

String >> isAscii [
    "Return true if and only if self contains only ASCII characters."
    self allSatisfy: [ :char -> char <= 127 ]
]

String >> isPrintable [
    "Return true if and only if self contains only printable characters."
    self allSatisfy: [ :char -> char isPrint ]
]

String >> isSpace [
    "Return true if and only if self contains only whitespace characters."
    self allSatisfy: [ :char -> char isWhitespace ]
]

String >> isTab [
    "Return true if and only if self contains only tab characters."
    self allSatisfy: [ :char -> char isTab ]
]

String >> isNewLine [
    "Return true if and only if self contains only newline characters."
    self allSatisfy: [ :char -> char isNewline ]
]

String >> toUpper [ (self collect: [ :char -> char uppercase ]) join: '' ]
String >> toLower [ (self collect: [ :char -> char lowercase ]) join: '' ]

String >> hexString [
    "Return an ASCII string representation of self in hexadecimal notation, with
        each byte represented by two hexadecimal digits."
    (self collect: [ :char -> char asByte hexString ]) join: ''
]

"! List Definitions"
Collection >> last [
    "Return the last element of self, or nil if self is empty."
    (self size > 0) ifTrue: [ self at: self size ]
]

Collection >> take: aNumber [
    "Return a new collection containing the first aNumber elements of self."
    (0 to: aNumber - 1) collect: [ |i| self at: i + 1 ]
]

Collection >> drop: aNumber [
    "Return a new collection containing all but the first aNumber elements of self."
    aNumber to: self size collect: [ |i| self at: i + aNumber ]
]

Collection >> exclusiveOr: anotherCollection [
    "Return a new collection containing the elements of self that are not in
        anotherCollection, and the elements of anotherCollection that are not in
        self."
    self asSet exclusiveOr: anotherCollection asSet asArray
]

Collection >> includes: anotherCollection [
    "Return true if and only if self contains all the elements of anotherCollection."
    anotherCollection allSatisfy: [ :element -> self includes: element ]
]

Collection >> includesAll: anotherCollection [ self includes: anotherCollection ]

Collection >> includesAny: anotherCollection [
    "Return true if and only if self contains any of the elements of anotherCollection."
    anotherCollection anySatisfy: [ :element -> self includes: element ]
]

Collection >> includesOnly: anotherCollection [
    "Return true if and only if self contains exactly the same elements as
        anotherCollection."
    self includes: anotherCollection and: [ anotherCollection includes: self ]
]

Collection >> exclusiveIntersection: anotherCollection [
    "Return a new collection containing the elements of self that are not in
        anotherCollection."
    self select: [ :element -> not: [ anotherCollection includes: element ] ]
]

Collection >> exclusiveUnion: anotherCollection [
    "Return a new collection containing the elements of self that are not in
        anotherCollection, and the elements of anotherCollection that are not in
        self."
    self union: anotherCollection exclusiveOr: anotherCollection
]

Collection >> intersection: anotherCollection [
    "Return a new collection containing the elements that are in both self and
        anotherCollection."
    self select: [ :element -> anotherCollection includes: element ]
]

Collection >> union: anotherCollection [
    "Return a new collection containing all the elements of self and anotherCollection,
        with duplicate elements removed."
    self asSet union: anotherCollection asSet asArray
]

Collection >> sort [
    "Return a new collection containing the elements of self sorted in ascending
        order."
    self collect: [ :element -> element ] sortedCollection
]

Collection >> sort: aComparator [
    "Return a new collection containing the elements of self sorted in ascending
        order according to the comparator aComparator."
    self collect: [ :element -> element ] sortedCollection: aComparator
]

Collection >> sortWith: aBlock [
    "Return a new collection containing the elements of self sorted in ascending
        order according to the block aBlock."
    self collect: [ :element -> element ] sortedCollection: [ |a b| aBlock valueWith: a with: b ]
]

Collection >> shuffle [
    "Return a new collection containing the elements of self in a random order."
    (self size) timesRepeat: [ :i |
        [ :j | j := Random new next: self size. j ~= i ] whileTrue: [
            |temp| temp := self at: i. self at: i put: self at: j. self at: j put: temp
        ]
    ]
]

Collection >> shuffleWith: aRandomGenerator [
    (self size) timesRepeat: [ :i |
        [ :j | j := aRandomGenerator next: self size. j ~= i ] whileTrue: [
            |temp| temp := self at: i. self at: i put: self at: j. self at: j put: temp
        ]
    ]
]

Collection >> shuffleWith: aBlock [
    (self size) timesRepeat: [ :i |
        [ :j | j := aBlock valueWith: i. j ~= i ] whileTrue: [
            |temp| temp := self at: i. self at: i put: self at: j. self at: j put: temp
        ]
    ]
]

"! OrderedCollection Definitions"
OrderedCollection >> exclusiveOr: anotherCollection [
    "Return a new ordered collection containing the elements of self that are not in
        anotherCollection, and the elements of anotherCollection that are not in
        self."
    OrderedCollection with: (self select: [ :element -> not: [ anotherCollection includes: element ] ])
        withAll: (anotherCollection select: [ :element -> not: [ self includes: element ] ])
]

OrderedCollection >> exclusiveIntersection: anotherCollection [
    "Return a new ordered collection containing the elements of self that are not in
        anotherCollection."
    self select: [ :element -> not: [ anotherCollection includes: element ] ]
]

OrderedCollection >> exclusiveUnion: anotherCollection [
    "Return a new ordered collection containing the elements of self that are not in
        anotherCollection, and the elements of anotherCollection that are not in
        self."
    self exclusiveOr: anotherCollection
]

OrderedCollection >> intersection: anotherCollection [
    "Return a new ordered collection containing the elements that are in both self and
        anotherCollection."
    self select: [ :element -> anotherCollection includes: element ]
]

OrderedCollection >> union: anotherCollection [
    "Return a new ordered collection containing all the elements of self and anotherCollection,
        with duplicate elements removed."
    OrderedCollection with: ((self asArray) union: (anotherCollection asArray))
]

OrderedCollection >> swapElementsAt: index1 and: index2 [
    |temp| temp := self at: index1. self at: index1 put: self at: index2. self at: index2 put: temp
]

OrderedCollection >> sortBy: aComparator [
    "Return a new ordered collection containing the elements of self sorted in ascending
        order according to the comparator aComparator."
    (0 to: self size - 1) do: [ |i|
        (i + 1 to: self size - 1) do: [ |j|
            self at: i > self at: j ifTrue: [ self swapElementsAt: i and: j ]
        ]
    ]
]

OrderedCollection >> sortBy: aBlock [
    "Return a new ordered collection containing the elements of self sorted in ascending
        order according to the block aBlock."
    (0 to: self size - 1) do: [ |i|
        (i + 1 to: self size - 1) do: [ |j|
            aBlock valueWith: self at: i > aBlock valueWith: self at: j ifTrue: [ self swapElementsAt: i and: j ]
        ]
    ]
]

"! Stack Definitions"
Stack >> push: anObject [ self add: anObject ]
Stack >> pop [ self removeLast ]
Stack >> peek [ self last ]

"! Queue Definitions"
Queue >> enqueue: anObject [ self add: anObject ]
Queue >> dequeue [ self removeFirst ]
Queue >> peek [ self first ]

"! Dictionary Definitions"
Dictionary >> at: aKey ifAbsent: anObject [ (self at: aKey) ifNil: [ self at: aKey put: anObject ]. self at: aKey ]
Dictionary >> atPut: aKey with: anObject [ self at: aKey put: anObject ]
Dictionary >> keys [ self keys asArray ]
Dictionary >> values [ self values asArray ]
Dictionary >> size [ self size ]
Dictionary >> isEmpty [ self size = 0 ]

"! FileStream Definitions"
FileStream >> write: aString [ self nextPutAll: aString ]
FileStream >> writeLine: aString [ self cr. self nextPutAll: aString. self cr ]
FileStream >> readLine [ self upTo: Character cr ]
FileStream >> seekToStart [ self reset ]
FileStream >> seekToEnd