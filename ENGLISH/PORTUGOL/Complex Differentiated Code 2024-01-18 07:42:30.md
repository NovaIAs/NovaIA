```portugal
PROGRAM COMPLEX_DIFFERENTIATED_CODE;

PROCEDURE BinomialCoefficient(n, k, result);
    INTEGER n, k;
    INTEGER result;
BEGIN
    IF k = 0 OR k = n THEN
        result := 1;
    ELSE
        result := BinomialCoefficient(n - 1, k - 1, result) + BinomialCoefficient(n - 1, k, result);
    END IF;
END PROCEDURE BinomialCoefficient;

PROCEDURE Fibonacci(n, result);
    INTEGER n;
    INTEGER result;
BEGIN
    IF n <= 1 THEN
        result := n;
    ELSE
        result := Fibonacci(n - 1, result) + Fibonacci(n - 2, result);
    END IF;
END PROCEDURE Fibonacci;

PROCEDURE GreatestCommonDivisor(a, b, result);
    INTEGER a, b;
    INTEGER result;
BEGIN
    IF b = 0 THEN
        result := a;
    ELSE
        GreatestCommonDivisor(b, a MOD b, result);
    END IF;
END PROCEDURE GreatestCommonDivisor;

PROCEDURE LeastCommonMultiple(a, b, result);
    INTEGER a, b;
    INTEGER result;
BEGIN
    GreatestCommonDivisor(a, b, result);
    result := (a * b) DIV result;
END PROCEDURE LeastCommonMultiple;

PROCEDURE PrimeFactors(n, result);
    INTEGER n;
    INTEGER result;
BEGIN
    FOR i := 2 TO n DO
        WHILE n MOD i = 0 DO
            result := i;
            n := n DIV i;
        END WHILE;
    END FOR;
END PROCEDURE PrimeFactors;

PROCEDURE SieveOfEratosthenes(n, primeNumbers);
    INTEGER n;
    ARRAY[1..n] OF BOOLEAN primeNumbers;
BEGIN
    FOR i := 2 TO n DO
        primeNumbers[i] := TRUE;
    END FOR;
    FOR i := 2 TO n DO
        IF primeNumbers[i] THEN
            FOR j := i * i TO n BY i DO
                primeNumbers[j] := FALSE;
            END FOR;
        END IF;
    END FOR;
END PROCEDURE SieveOfEratosthenes;

PROCEDURE SortingAlgorithms();
BEGIN
    PROCEDURE BubbleSort(array, length);
        ARRAY[1..length] OF INTEGER array;
        INTEGER length;
    BEGIN
        FOR i := 1 TO length - 1 DO
            FOR j := i + 1 TO length DO
                IF array[i] > array[j] THEN
                    temp := array[i];
                    array[i] := array[j];
                    array[j] := temp;
                END IF;
            END FOR;
        END FOR;
    END PROCEDURE BubbleSort;

    PROCEDURE SelectionSort(array, length);
        ARRAY[1..length] OF INTEGER array;
        INTEGER length;
    BEGIN
        FOR i := 1 TO length - 1 DO
            minIndex := i;
            FOR j := i + 1 TO length DO
                IF array[j] < array[minIndex] THEN
                    minIndex := j;
                END IF;
            END FOR;
            temp := array[i];
            array[i] := array[minIndex];
            array[minIndex] := temp;
        END FOR;
    END PROCEDURE SelectionSort;

    PROCEDURE InsertionSort(array, length);
        ARRAY[1..length] OF INTEGER array;
        INTEGER length;
    BEGIN
        FOR i := 2 TO length DO
            key := array[i];
            j := i - 1;
            WHILE j >= 1 AND key < array[j] DO
                array[j + 1] := array[j];
                j := j - 1;
            END WHILE;
            array[j + 1] := key;
        END FOR;
    END PROCEDURE InsertionSort;

    PROCEDURE QuickSort(array, low, high);
        ARRAY[1..length] OF INTEGER array;
        INTEGER low, high;
    BEGIN
        IF low < high THEN
            pivot := array[high];
            i := low - 1;
            FOR j := low TO high - 1 DO
                IF array[j] <= pivot THEN
                    i := i + 1;
                    temp := array[i];
                    array[i] := array[j];
                    array[j] := temp;
                END IF;
            END FOR;
            temp := array[i + 1];
            array[i + 1] := array[high];
            array[high] := temp;
            QuickSort(array, low, i);
            QuickSort(array, i + 2, high);
        END IF;
    END PROCEDURE QuickSort;

    PROCEDURE MergeSort(array, low, high);
        ARRAY[1..length] OF INTEGER array;
        INTEGER low, high;
    BEGIN
        IF low < high THEN
            mid := (low + high) DIV 2;
            MergeSort(array, low, mid);
            MergeSort(array, mid + 1, high);
            Merge(array, low, mid, high);
        END IF;
    END PROCEDURE MergeSort;

    PROCEDURE Merge(array, low, mid, high);
        ARRAY[1..length] OF INTEGER array;
        INTEGER low, mid, high;
    BEGIN
        i := low;
        j := mid + 1;
        k := 0;
        WHILE i <= mid AND j <= high DO
            IF array[i] <= array[j] THEN
                tempArray[k] := array[i];
                i := i + 1;
            ELSE
                tempArray[k] := array[j];
                j := j + 1;
            END IF;
            k := k + 1;
        END WHILE;
        WHILE i <= mid DO
            tempArray[k] := array[i];