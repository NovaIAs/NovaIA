```smalltalk
"This code creates a dynamic list of numbers from 1 to 100, and then prints each number in the list."

[1 to: 100] do: [ :each | Transcript show: each printString ]

"This code creates a dictionary of names and ages, and then prints each name and age in the dictionary."

ages := Dictionary new.
ages at: 'Alice' put: 20.
ages at: 'Bob' put: 25.
ages at: 'Carol' put: 30.

ages keysAndValuesDo: [ :name :age | Transcript show: name printString , ' is ' printString , age printString ]

"This code creates a block that increments a counter, and then uses the block as an argument to the times: method to increment the counter 10 times."

counter := 0.
[counter := counter + 1] timesRepeat: 10.

Transcript show: counter printString.

"This code creates a stack of numbers from 1 to 10, and then pops and prints each number from the stack."

stack := Stack new.
[1 to: 10] do: [ :each | stack push: each ].

[stack isEmpty not] whileTrue: [ Transcript show: stack pop printString ].

"This code creates a queue of numbers from 1 to 10, and then dequeues and prints each number from the queue."

queue := Queue new.
[1 to: 10] do: [ :each | queue enqueue: each ].

[queue isEmpty not] whileTrue: [ Transcript show: queue dequeue printString ].

"This code creates a set of numbers from 1 to 10, and then prints each number in the set."

set := Set new.
[1 to: 10] do: [ :each | set add: each ].

set do: [ :each | Transcript show: each printString ]

```