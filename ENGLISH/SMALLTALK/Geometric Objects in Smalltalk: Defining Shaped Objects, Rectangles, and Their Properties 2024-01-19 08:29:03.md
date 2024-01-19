```smalltalk
Object subclass: ShapedObject [
    "A specialized kind of object that has a shape. The shape can be any shape that can be represented as a polygon, including circles, rectangles, and triangles."

    instanceVariableNames: 'shape'

    classVariableNames: ''

    poolDictionaries: ''

    category: 'MyFramework-Graphics'
]

ShapedObject methodsFor: 'Copying' [
    copy: aShape [
        "Return a copy of the receiver with the given shape."

        ^ ShapedObject new shape: aShape
    ]
]

ShapedObject methodsFor: 'Geometry' [
    area [
        "Return the area of the receiver's shape."

        | area |
        area := 0.0.
        shape do: [:line |
            area := area + (line at: 1) * (line at: 2) - (line at: 2) * (line at: 1)
        ].
        ^ area / 2.0
    ]

    perimeter [
        "Return the perimeter of the receiver's shape."

        | perimeter |
        perimeter := 0.0.
        shape do: [:line |
            perimeter := perimeter + line length
        ].
        ^ perimeter
    ]

    containsPoint: aPoint [
        "Return true if the receiver's shape contains the given point."

        | ray |
        ray := Point new x: 1000000 y: aPoint y.
        ^ self intersectsLineSegmentFrom: aPoint to: ray count: 1
    ]

    intersectsLineSegmentFrom: aPoint to: bPoint [
        "Return true if the receiver's shape intersects the given line segment."

        | line |
        line := LineSegment new start: aPoint end: bPoint.
        ^ self intersectsShape: line
    ]

    intersectsLineSegmentFrom: aPoint to: bPoint count: count [
        "Return true if the receiver's shape intersects the given line segment, counting the number of intersections."

        | line count |
        line := LineSegment new start: aPoint end: bPoint.
        count := 0.
        shape do: [:lineSegment |
            count := count + lineSegment intersectsLineSegment: line
        ].
        ^ count > 0
    ]

    intersectsShape: aShape [
        "Return true if the receiver's shape intersects the given shape."

        | lineSegment1 lineSegment2 |
        shape do: [:lineSegment1 |
            aShape shape do: [:lineSegment2 |
                if [lineSegment1 intersectsLineSegment: lineSegment2] then [^ true]
            ]
        ].
        ^ false
    ]
]

ShapedObject methodsFor: 'Initialization' [
    initialize: aShape [
        "Create a new shaped object with the given shape."

        super initialize.
        shape := aShape
    ]
]

ShapedObject methodsFor: 'Printing' [
    printOn: aStream [
        "Print the receiver's shape on the given stream."

        super printOn: aStream.
        aStream nextPutAll: 'shape: '.
        shape printOn: aStream
    ]
]

LineSegment subclass: Rectangle [
    "A specialized kind of line segment that represents a rectangle."

    instanceVariableNames: 'width height'

    classVariableNames: ''

    poolDictionaries: ''

    category: 'MyFramework-Graphics'
]

Rectangle methodsFor: 'Copying' [
    copy: aRectangle [
        "Return a copy of the receiver with the given width and height."

        ^ Rectangle new width: aRectangle width height: aRectangle height
    ]
]

Rectangle methodsFor: 'Geometry' [
    area [
        "Return the area of the receiver."

        ^ width * height
    ]

    perimeter [
        "Return the perimeter of the receiver."

        ^ 2 * (width + height)
    ]

    containsPoint: aPoint [
        "Return true if the receiver contains the given point."

        ^ (aPoint x >= 0 and: [aPoint x <= width]) and: [
            (aPoint y >= 0