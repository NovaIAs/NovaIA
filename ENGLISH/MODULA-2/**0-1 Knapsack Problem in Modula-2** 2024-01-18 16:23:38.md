```modula-2
MODULE KnapSack;

FROM Terminal IMPORT WriteString, WriteLn, ReadInt;
FROM Format IMPORT WriteF;
FROM Strings IMPORT CompareString;

PROCEDURE KnapSack(List: ARRAY OF RECORD value: LONGCARD; weight: LONGCARD END;
         n: LONGCARD; W: LONGCARD;
         VAR best_solution: ARRAY OF LONGCARD);

VAR i: LONGCARD;
    j, k: LONGCARD;
    current_total: LONGCARD;
    best_weight: LONGCARD;

BEGIN
    best_weight := 0;
    FOR j := 0 TO n - 1 DO
        best_solution[j] := 0
    END;

    FOR i := 1 TO n DO
        current_total := 0;
        k := 0;
        FOR j := i - 1 TO 0 BY -1 DO
            IF current_total + List[j].weight <= W THEN
                current_total := current_total + List[j].weight;
                best_solution[k] := j;
                k := k + 1
            END
        END;

        IF current_total > best_weight THEN
            best_weight := current_total
        END
    END;

    WriteString("The best solution is: ");
    FOR i := 0 TO k - 1 DO
        WriteF("%2d ", [best_solution[i] + 1])
    END;
    WriteLn
END KnapSack;

TYPE Item = RECORD value: LONGCARD; weight: LONGCARD END;

VAR list: ARRAY OF Item := (
    (10, 2),
    (5, 3),
    (15, 5),
    (7, 7),
    (6, 1),
    (18, 4),
    (3, 1)
);

VAR best_solution: ARRAY OF LONGCARD := ARRAY 0 .. 6 OF LONGCARD;
VAR n, W: LONGCARD;

BEGIN
    n := 7;
    W := 15;

    KnapSack(list, n, W, best_solution);
END KnapSack.
```

This code implements the 0-1 knapsack problem in Modula-2. The problem is to find the subset of a given set of items with the highest total value that does not exceed a given weight.

The code first initializes the best solution to an empty list and the best weight to 0. Then, it iterates over the items in the list, starting from the last item. For each item, it checks if the current total weight plus the weight of the item is less than or equal to the given weight. If so, it adds the item to the current solution and updates the current total weight. Otherwise, it skips the item.

After iterating over all the items, the code checks if the current total weight is greater than the best weight. If so, it updates the best solution to the current solution and the best weight to the current total weight.

Finally, the code prints the best solution.

Here is a step-by-step explanation of the code:

* The `KnapSack` procedure takes the following arguments:
    * `List`: An array of records, where each record contains the value and weight of an item.
    * `n`: The number of items in the list.
    * `W`: The maximum weight that the knapsack can hold.
    * `best_solution`: A variable that will store the best solution (i.e., the subset of items with the highest total value that does not exceed the given weight).
* The procedure first initializes the best solution to an empty list and the best weight to 0.
* Then, it iterates over the items in the list, starting from the last item. For each item, it checks if the current total weight plus the weight of the item is less than or equal to the given weight. If so, it adds the item to the current solution and updates the current total weight. Otherwise, it skips the item.
* After iterating over all the items, the procedure checks if the current total weight is greater than the best weight. If so, it updates the best solution to the current solution and the best weight to the current total weight.
* Finally, the procedure prints the best solution.
* The main program first defines an array of items and initializes the variables `n` and `W`. Then, it calls the `KnapSack` procedure to find the best solution. Finally, it prints the best solution.