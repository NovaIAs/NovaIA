Here is a SMALLTALK code that generates a random maze:

```smalltalk
Object subclass: Maze [
    slots: {
        width  "Width of the maze."
        height "Height of the maze."
        cells  "Array of cells that make up the maze."
        start  "Cell that represents the starting point."
        end    "Cell that represents the ending point."
    }

    class methods: [
        generate: aWidth anHeight -> [
            "Generate a random maze of the given width and height."
            | maze |
            maze := self new setWidth: aWidth setHeight: anHeight.
            maze generateCells.
            maze generateMaze.
            maze
        ]
    ]

    instance methods: [
        setWidth: aWidth -> [
            "Set the width of the maze."
            width := aWidth
        ]

        setHeight: aHeight -> [
            "Set the height of the maze."
            height := aHeight
        ]

        generateCells -> [
            "Generate the cells that make up the maze."
            cells := Array new: width * height.
            1 to: cells size do: [ :i | cells at: i put: Cell new ]
        ]

        generateMaze -> [
            "Generate the maze."
            start := cells atRandom.
            end := cells atRandom.
            generateMazeFromCell: start
        ]

        generateMazeFromCell: aCell -> [
            "Generate the maze from the given cell."
            4 to: 1 by: -1 do: [ :direction |
                | neighbor |
                neighbor := aCell neighborInDirection: direction.
                neighbor notNil and: [neighbor notVisited] ifTrue: [
                    aCell setVisited.
                    neighbor setVisited.
                    generateMazeFromCell: neighbor
                ]
            ]
        ]

        string -> [
            "Return a string representation of the maze."
            | result |
            result := String streamContents: [ :stream |
                1 to: height do: [ :y |
                    stream cr.
                    1 to: width do: [ :x |
                        cells at: (y * width) + x ifNil: [stream print: ' '] ifNotNil: [stream print: cells at: (y * width) + x string]
                    ]
                ]
            ].
            result
        ]
    ]
]

Cell subclass: [
    slots: {
        visited "Flag indicating whether the cell has been visited."
    }

    instance methods: [
        setVisited -> [
            "Set the visited flag to true."
            visited := true
        ]

        visited -> [
            "Return the visited flag."
            visited
        ]

        string -> [
            "Return a string representation of the cell."
            visited ifTrue: [ 'X' ] ifFalse: [ ' ' ]
        ]
    ]
]

Cell subclass: Exit [
    instance methods: [
        string -> [
            "Return a string representation of the cell."
            'E'
        ]
    ]
]

Cell subclass: Start [
    instance methods: [
        string -> [
            "Return a string representation of the cell."
            'S'
        ]
    ]
]

```

To use this code, you can do the following:

```smalltalk
maze := Maze generate: 20 20.
Transcript show: maze string.
```

This will generate a random maze of size 20x20 and print it to the Transcript window.

The `Maze` class has a number of methods for generating and manipulating mazes. The `generate` class method generates a new maze of the given width and height. The `generateCells` method generates the cells that make up the maze. The `generateMaze` method generates the maze itself, starting from a random cell. The `string` method returns a string representation of the maze.

The `Cell` class is the superclass of all cells in the maze. It has a `visited` slot that indicates whether the cell has been visited. The `setVisited` method sets the `visited` flag to true. The `visited` method returns the `visited` flag. The `string` method returns a string representation of the cell.

The `Exit` and `Start` classes are subclasses of `Cell` that represent the exit and start points of the maze, respectively. They override the `string` method to return a different string representation for the exit and start points.

The code also includes a Transcript object that is used to print the maze to the screen.