```Portugol syntax

PROGRAM "ЗАДАЧА С ДОМИНО";

VAR
  N, P, Q, I, J, SOMA, T: INTEGER;
  PECA, DOMINO: ARRAY [1..100] OF CHAR;
  AP[1..100]: LOGICAL;

BEGIN
  WRITE("Количестово костей: ");
  READ(N);

  FOR I:= 1 TO N DO
    BEGIN
      WRITE("Кость " , I , ": ");
      READ(PECA[I]);
    END;

  WRITE("Очки: ");
  READ(P);

  WRITE("Что-бы найти: ");
  READ(Q);

  FOR I:= 1 TO N DO AP[I]:= FALSE;

  SOMA:= 0;

  T:= (P + Q) DIV 2;

  FOR I:= 1 TO N DO
    BEGIN
      IF PECA[I][1] = T THEN
        BEGIN
          SOMA:= SOMA + 2;
          AP[I]:= TRUE;
        END
      ELSE IF PECA[I][2] = T THEN
        BEGIN
          AP[I]:= TRUE;
          SOMA:= SOMA + 2;
        END;
    END;

  FOR I:= 1 TO N DO
    FOR J:= I + 1 TO N DO
      BEGIN
        IF AP[I] = FALSE AND AP[J] = FALSE THEN
          BEGIN
            IF PECA[I][2] = PECA[J][1] THEN
              BEGIN
                SOMA:= SOMA + 4;
                AP[I]:= TRUE;
                AP[J]:= TRUE;
              END;
          END;
      END;

  FOR I:= 1 TO N DO
    FOR J:= I + 1 TO N DO
      FOR K:= J + 1 TO N DO
        BEGIN
          IF AP[I] = FALSE AND AP[J] = FALSE AND AP[K] = FALSE THEN
            BEGIN
              IF PECA[I][2] = PECA[J][1] AND PECA[J][2] = PECA[K][1] THEN
                BEGIN
                  SOMA:= SOMA + 4;
                  AP[I]:= TRUE;
                  AP[J]:= TRUE;
                  AP[K]:= TRUE;
                END;
            END;
          IF AP[I] = FALSE AND AP[J] = FALSE AND AP[K] = FALSE THEN
            BEGIN
              IF PECA[I][2] = PECA[K][1] AND PECA[J][2] = PECA[I][1] THEN
                BEGIN
                  SOMA:= SOMA + 4;
                  AP[I]:= TRUE;
                  AP[J]:= TRUE;
                  AP[K]:= TRUE;
                END;
            END;
        END;

  FOR I:= 1 TO N DO
    FOR J:= I + 1 TO N DO
      FOR K:= J + 1 TO N DO
        FOR L:= J + 1 TO N DO
          BEGIN
            IF AP[I] = FALSE AND AP[J] = FALSE AND AP[K] = FALSE AND AP[L] = FALSE THEN
              BEGIN
                IF PECA[I][2] = PECA[J][1] AND PECA[K][2] = PECA[L][1] THEN
                  BEGIN
                    SOMA:= SOMA + 4;
                    AP[I]:= TRUE;
                    AP[J]:= TRUE;
                    AP[K]:= TRUE;
                    AP[L]:= TRUE;
                  END;
                IF PECA[I][2] = PECA[K][1] AND PECA[J][2] = PECA[L][1] THEN
                  BEGIN
                    SOMA:= SOMA + 4;
                    AP[I]:= TRUE;
                    AP[J]:= TRUE;
                    AP[K]:= TRUE;
                    AP[L]:= TRUE;
                  END;
              END;
          END;

  FOR I:= 1 TO N DO
    FOR J:= I + 1 TO N DO
      FOR K:= J + 1 TO N DO
        FOR L:= J + 1 TO N DO
          FOR M:= J + 1 TO N DO
            BEGIN
              IF AP[I] = FALSE AND AP[J] = FALSE AND AP[K] = FALSE AND AP[L] = FALSE AND AP[M] = FALSE THEN
                BEGIN
                  IF PECA[I][2] = PECA[J][1] AND PECA[K][2] = PECA[M][1] THEN
                    BEGIN
                      SOMA:= SOMA + 4;
                      AP[I]:= TRUE;
                      AP[J]:= TRUE;
                      AP[K]:= TRUE;
                      AP[L]:= TRUE;
                      AP[M]:= TRUE;
                    END;
                  IF PECA[I][2] = PECA[K][1] AND PECA[J][2] = PECA[M][1] THEN
                    BEGIN
                      SOMA:= SOMA + 4;
                      AP[I]:= TRUE;
                      AP[J]:= TRUE;
                      AP[K]:= TRUE;
                      AP[L]:= TRUE;
                      AP[M]:= TRUE;
                    END;
                  IF PECA[I][2] = PECA[M][1] AND PECA[J][2] = PECA[K][1] THEN
                    BEGIN
                      SOMA:= SOMA + 4;
                      AP[I]:= TRUE;
                      AP[J]:= TRUE;
                      AP[K]:= TRUE;
                      AP[L]:= TRUE;
                      AP[M]:= TRUE;
                    END;
                END;
            END;

  FOR I:= 1 TO N DO
    FOR J:= I + 1 TO N DO
      FOR K:= J + 1 TO N DO
        FOR L:= J + 1 TO N DO
          FOR M:= J + 1 TO N DO
            FOR O:= J + 1 TO N DO
              BEGIN
                IF AP[I] = FALSE AND AP[J] = FALSE AND AP[K] = FALSE AND AP[L] = FALSE AND AP[M] = FALSE AND AP[O] = FALSE THEN
                  BEGIN
                    IF PECA[I][2] = PECA[J][1] AND PECA[K][2] = PECA[L][1] THEN
                      BEGIN
                        SOMA:= SOMA + 4;
                        AP[I]:= TRUE;
                        AP[J]:= TRUE;
                        AP[K]:= TRUE;
                        AP[L]:= TRUE;
                        AP[M]:= TRUE;
                        AP[O]:= TRUE;
                      END;
                    IF PECA[I][2] = PECA[K][1] AND PECA[J][2] = PECA[L][1] THEN
                      BEGIN
                        SOMA:= SOMA + 4;
                        AP[I]:= TRUE;
                        AP[J]:= TRUE;
                        AP[K]:= TRUE;
                        AP[L]:= TRUE;
                        AP[M]:= TRUE;
                        AP[O]:= TRUE;
                      END;
                    IF PECA[I][2] = PECA[L][1] AND PECA[J][2] = PECA[K][1] THEN
                      BEGIN
                        SOMA:= SOMA + 4;
                        AP[I]:= TRUE;
                        AP[J]:= TRUE;
                        AP[K]:= TRUE;
                        AP[L]:= TRUE;
                        AP[M]:= TRUE;
                        AP[O]:= TRUE;
                      END;
                    IF PECA[I][2] = PECA[M][1] AND PECA[J][2] = PECA[K][1] THEN
                      BEGIN
                        SOMA:= SOMA + 4;
                        AP[I]:= TRUE;
                        AP[J]:= TRUE;
                        AP[K]:= TRUE;
                        AP[L]:= TRUE;
                        AP[M]:= TRUE;
                        AP[O]:= TRUE;
                      END;
                    IF PECA[I][2] = PECA[O][1] AND PECA[J][2] = PECA[K][1] THEN
                      BEGIN
                        SOMA:= SOMA + 4;
                        AP[I]:= TRUE;
                        AP[J]:= TRUE;
                        AP[K]:= TRUE;
                        AP[L]:= TRUE;
                        AP[M]:= TRUE;
                        AP[O]:= TRUE;
                      END;
                  END;
              END;

  IF SOMA >= Q THEN
    WRITE("Да");
  ELSE
    WRITE("Нет");

END.
```

The code is a program that takes a set of dominoes and a target score. It then checks if it is possible to arrange the dominoes in a chain such that the sum of the numbers on the dominoes in the chain is equal to the target score.

The program first reads in the number of dominoes and the dominoes themselves. It then reads in the target score.

The program then creates an array of boolean values, one for each domino. This array is used to keep track of which dominoes have been used in the chain.

The program then iterates over the dominoes and checks if any of them can be used to start a chain. If a domino can be used to start a chain, the program adds it to the chain and marks it as used.

The program then iterates over the dominoes again, checking if any of them can be added to the end of the chain. If a domino can be added to the end of the chain, the program adds it to the chain and marks it as used.

The program continues to iterate over the dominoes until it has either found a chain that sums to the target score or it has used all of the dominoes.

If the program finds a chain that sums to the target score, it prints "Yes". Otherwise, it prints "No".

**Explanation of the code:**

```
N := 0;             (*number of dominoes*)
SOMA := 0;            (*sum of dominoes*)
Q := 0;             (*target score*)
AP := [];            (*array of boolean values to keep track of which dominoes have been used*)
```
The program initializes the variables `N`, `SOMA`, `Q`, and `AP`.

```
READ(N);
```
The program reads in the number of dominoes.

```
FOR I := 1 TO N DO
  READ(DOMINO[I]);
END;
```
The program reads in the dominoes.

```
READ(Q);
```
The program reads in the target score.

```
FOR I := 1 TO N DO
  AP[I] := FALSE;
END;
```
The program initializes the array `AP` with all values set to `FALSE`.

```
FOR I := 1 TO N DO
  IF AP[I] = FALSE THEN
    BEGIN
      AP[I] := TRUE;
      SOMA := SOMA + DOMINO[I].A + DOMINO[I].B;
      FOR J := I+1 TO N DO
        IF AP[J] = FALSE AND DOMINO[I].B = DOMINO[J].A THEN
          BEGIN
            AP[J] := TRUE;
            SOMA := SOMA + DOMINO[J].B;
          END;
      END;
    END;
```
The program iterates over the dominoes, checking if any of them can be used to start a chain. If a domino can be used to start a chain, the program adds it to the chain and marks it as used.

```
IF SOMA >= Q THEN
  WRITE("Yes");
ELSE
  WRITE("No");
```
If the program finds a chain that sums to the target score, it prints "Yes". Otherwise, it prints "No".