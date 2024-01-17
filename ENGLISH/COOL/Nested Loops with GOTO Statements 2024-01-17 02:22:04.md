```cool
class Main {
	main() : Object {
		let i : Int <- 0;
		let j : Int <- 1;
		loop:
			if (i < 10) then
				let j : Int <- j + 1;
				let i : Int <- i + 1;
				goto loop;
			else
				let i : Int <- 0;
				let j : Int <- 1;
				loop2:
					if (i < 15) then
						let j : Int <- j + 1;
						let i : Int <- i + 1;
						goto loop2;
					fi;
			fi;
	}
}
```

Explanation:

1. The class `Main` is defined, which contains the main function.
2. In the main function, two integer variables `i` and `j` are declared and initialized to 0 and 1, respectively.
3. A labeled loop `loop` is created.
4. Inside the loop, there is an `if` statement that checks if `i` is less than 10.
5. If the condition is true, `j` is incremented by 1, `i` is incremented by 1, and the program jumps back to the beginning of the loop using the `goto` statement.
6. If the condition is false, the program exits the loop and proceeds to the next statement.
7. After exiting the loop, `i` and `j` are reset to 0 and 1, respectively.
8. Another labeled loop `loop2` is created.
9. Inside the loop, there is an `if` statement that checks if `i` is less than 15.
10. If the condition is true, `j` is incremented by 1, `i` is incremented by 1, and the program jumps back to the beginning of the loop using the `goto` statement.
11. If the condition is false, the program exits the loop and the main function ends.