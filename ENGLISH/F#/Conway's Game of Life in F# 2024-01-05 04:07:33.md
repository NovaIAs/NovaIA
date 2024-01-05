```f#

// Define a module to encapsulate the code
module ConwayGameOfLife =

    // Define a type to represent the state of a cell
    type CellState =
        | Alive
        | Dead

    // Define a type to represent a cell
    type Cell =
        {
            State : CellState
        }

    // Define a type to represent a grid of cells
    type Grid =
        {
            Cells : Cell[][]
        }

    // Create a new grid with the specified number of rows and columns
    let createGrid (rows : int) (cols : int) =
        {
            Cells = Array.init rows (fun _ -> Array.init cols (fun _ -> { State = Dead }))
        }

    // Set the state of a cell at the specified row and column
    let setCellState (grid : Grid) (row : int) (col : int) (state : CellState) =
        grid.Cells.[row].[col].State <- state

    // Get the state of a cell at the specified row and column
    let getCellState (grid : Grid) (row : int) (col : int) =
        grid.Cells.[row].[col].State

    // Count the number of alive neighbors of a cell at the specified row and column
    let countAliveNeighbors (grid : Grid) (row : int) (col : int) =
        let aliveNeighbors =
            [
                getCellState grid (row - 1) (col - 1);
                getCellState grid (row - 1) (col);
                getCellState grid (row - 1) (col + 1);
                getCellState grid (row) (col - 1);
                getCellState grid (row) (col + 1);
                getCellState grid (row + 1) (col - 1);
                getCellState grid (row + 1) (col);
                getCellState grid (row + 1) (col + 1)
            ]
            |> Seq.filter (fun state -> state = Alive)
            |> Seq.length
        aliveNeighbors

    // Update the state of a cell based on the number of alive neighbors
    let updateCellState (grid : Grid) (row : int) (col : int) =
        let aliveNeighbors = countAliveNeighbors grid row col
        let currentState = getCellState grid row col
        let newState =
            match (currentState, aliveNeighbors) with
            | (Alive, 2) | (Alive, 3) -> Alive
            | (Dead, 3) -> Alive
            | _ -> Dead
        setCellState grid row col newState

    // Update the state of all cells in the grid
    let updateGrid (grid : Grid) =
        for row in 0 .. grid.Cells.Length - 1 do
            for col in 0 .. grid.Cells.[row].Length - 1 do
                updateCellState grid row col
            done
        done

    // Print the grid to the console
    let printGrid (grid : Grid) =
        for row in 0 .. grid.Cells.Length - 1 do
            for col in 0 .. grid.Cells.[row].Length - 1 do
                let state = getCellState grid row col
                printfn "%c" (if state = Alive then '*' else ' ')
            done
            printfn ""
        done

// Create a new grid with the specified number of rows and columns
let grid = createGrid 20 20

// Set the initial state of the grid
setCellState grid 10 10 Alive
setCellState grid 10 11 Alive
setCellState grid 10 12 Alive
setCellState grid 11 12 Alive
setCellState grid 12 12 Alive

// Update the state of the grid 10 times
for i in 1 .. 10 do
    updateGrid grid
done

// Print the final state of the grid
printGrid grid

```

This code implements Conway's Game of Life in F#. The Game of Life is a cellular automaton that consists of a grid of cells, each of which can be in one of two states: alive or dead. The state of each cell changes at each generation based on the number of alive neighbors it has.

The code starts by defining a module called ConwayGameOfLife, which encapsulates all of the code for the game. The module defines several types, including CellState, Cell, and Grid, which represent the state of a single cell, a single cell, and a grid of cells, respectively.

The module also defines several functions, including createGrid, setCellState, getCellState, countAliveNeighbors, updateCellState, updateGrid, and printGrid. These functions are used to create a new grid, set the state of a cell, get the state of a cell, count the number of alive neighbors of a cell, update the state of a cell based on the number of alive neighbors it has, update the state of all cells in the grid, and print the grid to the console, respectively.

The main body of the code creates a new grid with 20 rows and 20 columns. It then sets the initial state of the grid by setting the state of several cells to Alive. The code then updates the state of the grid 10 times using the updateGrid function. Finally, the code prints the final state of the grid to the console using the printGrid function.