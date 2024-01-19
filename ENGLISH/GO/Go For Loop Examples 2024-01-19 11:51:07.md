```go
// https://golang.org/ref/spec#For_statements

package main

import "fmt"

func main() {
	// The basic for loop has three components: the initialization statement,
	// the condition, and the post statement. The initialization initializes
	// a variable; the condition specifies a boolean expression that controls
	// whether the loop continues to execute; and the post statement executes
	// after each iteration of the loop.
	//
	// In this example, the loop variable 'i' is initialized to 0, the
	// condition checks whether 'i' is less than 5, and the post statement
	// increments 'i' by 1.
	for i := 0; i < 5; i++ {
		fmt.Println(i)
	}

	// The for loop can also be used to iterate over a range of values.
	// In this example, the loop variable 'j' ranges over the values in the
	// slice 'numbers'.
	numbers := []int{0, 1, 2, 3, 4}
	for j, number := range numbers {
		fmt.Println(j, number)
	}

	// The for loop can also be used to iterate over a map.
	// In this example, the loop variable 'key' ranges over the keys in the
	// map 'fruits', and the loop variable 'value' ranges over the values
	// in the map 'fruits'.
	fruits := map[string]string{
		"apple":  "red",
		"banana": "yellow",
		"orange": "orange",
	}
	for key, value := range fruits {
		fmt.Println(key, value)
	}

	// The for loop can also be used to iterate over a channel.
	// In this example, the loop variable 'message' ranges over the messages
	// in the channel 'messages'.
	messages := make(chan string)
	go func() {
		messages <- "hello"
		messages <- "world"
		close(messages)
	}()
	for message := range messages {
		fmt.Println(message)
	}

	// The for loop can also be used to iterate over a string.
	// In this example, the loop variable 'rune' ranges over the runes in the
	// string 's'.
	s := "hello"
	for _, rune := range s {
		fmt.Println(rune)
	}
}
```

This code demonstrates various ways to use the `for` loop in Go. It includes loops with initialization statements, conditions, and post statements, loops that iterate over a range of values, and loops that iterate over maps, channels, and strings. The code also uses anonymous functions and goroutines to create a more complex and interesting program.