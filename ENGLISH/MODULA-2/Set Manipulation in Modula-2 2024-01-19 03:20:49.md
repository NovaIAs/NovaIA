```modula-2

MODULE Demonstration;
FROM Terminal IMPORT WriteLn;

TYPE Set OF INTEGER;

PROCEDURE Initialize(VAR S: Set);
VAR I: INTEGER;
BEGIN
  S := 0;
  FOR I := 1 TO 10 DO
    S := S <> I;
  END;
END Initialize;

PROCEDURE Print(S: Set);
VAR I: INTEGER;
BEGIN
  FOR I := 1 TO 10 DO
    IF S = I THEN
      WriteLn("TRUE")
    ELSE
      WriteLn("FALSE")
    END;
  END;
END Print;

VAR S: Set;

BEGIN
  Initialize(S);
  Print(S);
END Demonstration.

```

This code demonstrates the usage of sets in Modula-2 by initializing a set, `S`, with the values from 1 to 10 and then printing the truth values of the set membership test for each of these values. The expected output is:

```
TRUE
FALSE
FALSE
FALSE
FALSE
FALSE
FALSE
FALSE
FALSE
FALSE
```

Explanation:

1. **MODULE Demonstration:** This line declares the start of a module named ``Demonstration``. Modules in Modula-2 are used to group related data and procedures.

2. **FROM Terminal IMPORT WriteLn:** This line imports the ``WriteLn`` procedure from the ``Terminal`` module. The ``Terminal`` module provides basic input and output functions.

3. **TYPE Set OF INTEGER:** This declares a new type named ``Set`` that can hold a set of integers. Modula-2 allows you to define your own types, which can be useful for representing complex data structures.

4. **PROCEDURE Initialize(VAR S: Set);:** This procedure initializes a set variable `S` with the values from 1 to 10. The `VAR` keyword indicates that the parameter `S` is passed by reference, allowing the procedure to modify its value.

5. **PROCEDURE Print(S: Set);:** This procedure prints the truth values of the set membership test for each of the values from 1 to 10 in the set `S`.

6. **BEGIN:** This starts the main program block.

7. **Initialize(S);** This initializes the set `S` with the values from 1 to 10 using the `Initialize` procedure.

8. **Print(S);** This prints the truth values of the set membership test for each of the values from 1 to 10 in the set `S` using the `Print` procedure.

9. **END Demonstration.** This marks the end of the ``Demonstration`` module.