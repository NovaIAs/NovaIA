```tcl
package require Tcl 8.5
package require Tk 8.5
package require BLT::Graph

proc main {} {
    # Create a new toplevel window
    toplevel .main

    # Create a new graph widget
    graph .main.graph -height 400 -width 400

    # Create a new data set
    data .main.graph.data1 -type xy

    # Add some data to the data set
    foreach i [range 10] {
        data .main.graph.data1 add $i [expr {sin($i * 0.1)}]
    }

    # Create a new plot
    plot .main.graph.plot1 -data .main.graph.data1 -style lines

    pack .main.graph
}

main
```

This code creates a simple line graph using the BLT::Graph package. The code first creates a new toplevel window, then creates a new graph widget inside the window. Next, it creates a new data set and adds some data to it. Finally, it creates a new plot and assigns the data set to it. The plot is then packed into the graph widget.

Here is a breakdown of the code:

* `package require Tcl 8.5`: This line loads the Tcl 8.5 package.
* `package require Tk 8.5`: This line loads the Tk 8.5 package.
* `package require BLT::Graph`: This line loads the BLT::Graph package.
* `proc main {}`: This line defines a new procedure named `main`.
* `toplevel .main`: This line creates a new toplevel window named `.main`.
* `graph .main.graph -height 400 -width 400`: This line creates a new graph widget named `.main.graph` inside the `.main` window. The `-height` and `-width` options specify the height and width of the graph widget.
* `data .main.graph.data1 -type xy`: This line creates a new data set named `.main.graph.data1`. The `-type xy` option specifies that the data set will contain x-y data.
* `foreach i [range 10] { ... }`: This line creates a loop that iterates over the numbers from 1 to 10.
* `data .main.graph.data1 add $i [expr {sin($i * 0.1)}]`: This line adds a new data point to the `.main.graph.data1` data set. The `$i` variable contains the current value of the loop variable, and the `[expr {sin($i * 0.1)}]` expression calculates the sine of the current value of the loop variable multiplied by 0.1.
* `plot .main.graph.plot1 -data .main.graph.data1 -style lines`: This line creates a new plot named `.main.graph.plot1`. The `-data` option specifies the data set that the plot will use, and the `-style lines` option specifies that the plot will be a line graph.
* `pack .main.graph`: This line packs the `.main.graph` graph widget into the `.main` window.
* `main`: This line calls the `main` procedure, which creates the graph and displays it in the window.