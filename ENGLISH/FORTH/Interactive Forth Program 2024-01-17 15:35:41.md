```forth
: MESSAGE "FORTH IS FUN!" ; Store the message in a variable
: DISPLAY "CR ." ; Define a function to display a line of text
: ASK "YOU WANT TO HEAR IT AGAIN ? TYPE Y OR N." ; Ask the user a question

: AGAIN ; Define a loop that will run until the user says "N"
  DISPLAY MESSAGE ; Display the message
  ASK ; Ask the user the question
  KEY ; Get the user's response
  "Y" = IF ; If the user typed "Y",
    AGAIN ; Call the AGAIN function again
  ELSE ; Otherwise,
    DROP ; Discard the user's response
  THEN ; End of the IF statement
; End of the AGAIN function

AGAIN ; Start the loop
```

This code is a bit more complex than the previous examples, but it shows how Forth can be used to create interactive programs. The code defines a function called `MESSAGE` that stores the string "FORTH IS FUN!" in a variable. It then defines a function called `DISPLAY` that takes a string as an argument and displays it on the screen. The `ASK` function displays a question on the screen and waits for the user to type a response.

The `AGAIN` function is defined using a loop construct. The loop will run until the user types "N" in response to the question "YOU WANT TO HEAR IT AGAIN ? TYPE Y OR N.". Inside the loop, the `DISPLAY` function is used to display the message "FORTH IS FUN!" on the screen. The `ASK` function is then used to ask the user a question. If the user types "Y", the `AGAIN` function is called again, causing the loop to repeat. If the user types "N", the loop is exited and the program ends.

This code demonstrates how Forth can be used to create interactive programs that can respond to user input. This is a powerful capability that makes Forth a versatile programming language.