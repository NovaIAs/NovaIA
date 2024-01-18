```modula-2

MODULE Modula2CodeExample;
FROM InOut IMPORT WriteString, WriteCard, WriteLn;

PROCEDURE Greeting(name: STRING);
BEGIN
  WriteString("Hello, ");
  WriteString(name);
  WriteLn("!");
END Greeting;

PROCEDURE Main;
VAR
  name: ARRAY 20 OF CHAR;
BEGIN
  WriteString("What is your name? ");
  ReadString(name);
  Greeting(name);
END Main;

```

This code is a simple program written in Modula-2 that greets the user by name. It consists of two procedures:

- **Greeting:** This procedure takes a string as an argument and prints a greeting message to the console.
- **Main:** This is the main procedure of the program. It reads the user's name from the console and then calls the Greeting procedure to display a greeting message.

Here's a step-by-step explanation of the code:

1. **MODULE Modula2CodeExample:** This line declares the start of a module named Modula2CodeExample. A module is a collection of related procedures and data types.
2. **FROM InOut IMPORT WriteString, WriteCard, WriteLn:** This line imports three procedures (WriteString, WriteCard, and WriteLn) from the InOut module. These procedures are used for writing text and values to the console.
3. **PROCEDURE Greeting(name: STRING);:** This line declares a procedure named Greeting that takes a string as an argument.
4. **BEGIN:** This line marks the beginning of the Greeting procedure.
5. **WriteString("Hello, ");** This line writes the text "Hello, " to the console.
6. **WriteString(name);** This line writes the value of the name argument to the console.
7. **WriteLn("!");** This line writes a newline character to the console, followed by an exclamation mark (!).
8. **END Greeting;:** This line marks the end of the Greeting procedure.
9. **PROCEDURE Main:** This line declares the Main procedure, which is the entry point of the program.
10. **VAR:** This line declares a local variable named name, which is an array of 20 characters (i.e., a string).
11. **BEGIN:** This line marks the beginning of the Main procedure.
12. **WriteString("What is your name? ");** This line writes the text "What is your name? " to the console.
13. **ReadString(name);** This line reads the user's input from the console and stores it in the name variable.
14. **Greeting(name);** This line calls the Greeting procedure and passes the name variable as an argument.
15. **END Main;:** This line marks the end of the Main procedure.

When you run this program, it will prompt you to enter your name. Once you enter your name, the program will greet you by printing a message like "Hello, [your name]!" to the console.