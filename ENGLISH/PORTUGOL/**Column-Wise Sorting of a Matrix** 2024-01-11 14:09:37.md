```portugol

PROCEDURE SwitchMatrix(int m,int n,int mat[m,n])
    LOCAL int i,j,valor,aux;
    IF m<=0 OR n<=0 THEN
        WRITE("Numero de linhas ou colunas invalido!");
        RETURN;
    ENDIF;

    FOR i:=1 TO m DO
        PRINT(Matrix[i,1]);
    OD;
    PRINTLN;
    FOR j:=2 TO n DO
        FOR i:=1 TO m DO
            valor:=Matrix[i,j];
            aux:=i;
            WHILE aux>1 AND valor<Matrix[aux-1,j] DO
                Matrix[aux,j]:=Matrix[aux-1,j];
                aux:=aux-1;
            OD;
            Matrix[aux,j]:=valor;
        OD;
        FOR i:=1 TO m DO
            PRINT(Matrix[i,j]);
        OD;
        PRINTLN;
    OD;
    RETURN;
ENDPROCEDURE;

PROCEDURE main()
    LOCAL int i,j,m,n,mat[100,100]={0};

    READ(m,n);

    IF m<=0 OR n<=0 THEN
        WRITE("Numero de linhas ou colunas invalido!");
        RETURN;
    ENDIF;

    FOR i:=1 TO m DO
        FOR j:=1 TO n DO
            READ(mat[i,j]);
        OD;
    OD;

    SwitchMatrix(m,n,mat);

    RETURN;
ENDPROCEDURE;

```

Explanation:

1. **Procedure SwitchMatrix:** This procedure takes three arguments: `m` (number of rows), `n` (number of columns), and `mat` (a 2D array of integers). It sorts each column of the matrix in ascending order using the selection sort algorithm.

2. **FOR Loops:** The procedure uses nested `FOR` loops to iterate through each column of the matrix.

3. **Selection Sort:** Within each column, the procedure uses the selection sort algorithm to sort the elements in ascending order.

4. **WHILE Loop:** The `WHILE` loop is used to find the correct position for the current element in the sorted part of the column.

5. **Printing the Matrix:** After sorting each column, the procedure prints the updated matrix to the console.

6. **Main Procedure:** The `main` procedure reads the dimensions of the matrix and its elements from the user. Then, it calls the `SwitchMatrix` procedure to sort the matrix and prints the sorted matrix.