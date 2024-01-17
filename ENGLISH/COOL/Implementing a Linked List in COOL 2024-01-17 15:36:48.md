```cool
module IntList {
    class IntList {
	attribute first : Int;
	attribute rest  : IntList;

	method init(i : Int, r : IntList) {
	    first <- i;
	    rest  <- r;
	}

	method len : Int {
	    if (rest = IntList.nil) then
		1
	    else
		1 + rest.len
	    fi
	}

	method find(i : Int) : Boolean {
	    if (first = i) then
		true
	    else
		if (rest = IntList.nil) then
		    false
		else
		    rest.find(i)
		fi
	    fi
	}

	method append(i : Int) {
	    self <- new IntList init(i, self)
	}

	method index(i : Int) : Int {
	    if (i = 0) then
		first
	    else
		rest.index(i - 1)
	    fi
	}
    }

    class Nil extends IntList {
	method init() {
	    first <- 0;
	    rest  <- self;
	}

	method len : Int {
	    0
	}

	method find(i : Int) : Boolean {
	    false
	}

	method append(i : Int) {
	    self
	}

	method index(i : Int) : Int {
	    error("index: index out of bounds")
	}
    }

    class TestIntList {
	method main() : Void {
	    var l : IntList <- new IntList init(1, new IntList init(2, new IntList init(3, IntList.nil)));
	    var i : Int <- 0;

	    l.append(4);
	    l.append(5);

	    while (i < l.len) loop
		io.print_int(l.index(i));
		io.print_string(" ");
		i <- i + 1
	    pool;

	    io.print_string("\n");

	    if (l.find(4)) then
		io.print_string("4 was found\n")
	    else
		io.print_string("4 was not found\n")
	    fi;

	    if (l.find(6)) then
		io.print_string("6 was found\n")
	    else
		io.print_string("6 was not found\n")
	    fi
	}
    }
}
```

This code defines a class `IntList` that represents a list of integers. The class has four methods: `init`, `len`, `find`, and `append`. The `init` method initializes the list with a given integer and a reference to the rest of the list. The `len` method returns the length of the list. The `find` method returns true if the given integer is in the list, and false otherwise. The `append` method appends the given integer to the end of the list.

The code also defines a class `Nil` that represents an empty list. The `Nil` class has four methods: `init`, `len`, `find`, and `append`. The `init` method initializes the empty list. The `len` method returns 0. The `find` method always returns false. The `append` method returns the same list.

Finally, the code defines a class `TestIntList` that contains a `main` method that tests the `IntList` class. The `main` method creates an `IntList` object, adds some integers to it, and then prints the list. The `main` method also tests the `find` method to see if it can find the integer 4 in the list.

This code is a good example of how to use object-oriented programming to represent a data structure. The `IntList` class encapsulates the data and operations that can be performed on the data, making it easy to use and maintain.