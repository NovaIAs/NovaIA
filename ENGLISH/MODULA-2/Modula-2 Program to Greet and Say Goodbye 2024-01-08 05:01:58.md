```modula-2
MODULE Modula2Library;
FROM Terminal IMPORT WriteString, WriteLn;

PROCEDURE Greet(name: STRING);
BEGIN
  WriteString("Hello, ");
  WriteString(name);
  WriteLn(".");
END Greet;

PROCEDURE Bye(name: STRING);
BEGIN
  WriteString("Goodbye, ");
  WriteString(name);
  WriteLn(".");
END Bye;

PROCEDURE Main;
VAR
  name: STRING;
BEGIN
  WriteLn("What is your name?");
  ReadString(name);
  Greet(name);
  Bye(name);
END Main.
```

Explanation:

1. MODULE Modula2Library: This line declares the start of a module named Modula2Library. Modules are used to organize related procedures and data types in Modula-2.

2. IMPORT Terminal IMPORT WriteString, WriteLn;: This line imports the WriteString and WriteLn procedures from the Terminal module. These procedures are used for writing text to the console.

3. PROCEDURE Greet(name: STRING);: This line declares a procedure named Greet that takes a string parameter named name.

4. BEGIN: This line marks the beginning of the Greet procedure body.

5. WriteString("Hello, ");: This line uses the WriteString procedure to write the string "Hello, " to the console.

6. WriteString(name);: This line uses the WriteString procedure to write the value of the name parameter to the console.

7. WriteLn(".");: This line uses the WriteLn procedure to write a newline character to the console, effectively moving the cursor to the next line.

8. END Greet;: This line marks the end of the Greet procedure body.

9. PROCEDURE Bye(name: STRING);: This line declares a procedure named Bye that takes a string parameter named name.

10. BEGIN: This line marks the beginning of the Bye procedure body.

11. WriteString("Goodbye, ");: This line uses the WriteString procedure to write the string "Goodbye, " to the console.

12. WriteString(name);: This line uses the WriteString procedure to write the value of the name parameter to the console.

13. WriteLn(".");: This line uses the WriteLn procedure to write a newline character to the console, effectively moving the cursor to the next line.

14. END Bye;: This line marks the end of the Bye procedure body.

15. PROCEDURE Main;: This line declares the Main procedure, which is the entry point of the program.

16. VAR: This line declares a local variable named name of type STRING.

17. WriteLn("What is your name?");: This line uses the WriteLn procedure to write the question "What is your name?" to the console.

18. ReadString(name);: This line uses the ReadString procedure to read a line of text from the console and store it in the name variable.

19. Greet(name);: This line calls the Greet procedure with the value of the name variable as an argument.

20. Bye(name);: This line calls the Bye procedure with the value of the name variable as an argument.

21. END Main.: This line marks the end of the Main procedure body.