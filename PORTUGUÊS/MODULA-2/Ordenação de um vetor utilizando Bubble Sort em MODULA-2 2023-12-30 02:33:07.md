Claro! Aqui está um código em MODULA-2 que realiza a ordenação de um vetor utilizando o algoritmo Bubble Sort, uma técnica simples mas eficiente.

```
MODULE Ordenacao;

FROM InOut IMPORT WriteString, WriteInt, WriteLn;
FROM OutStream IMPORT Output;

CONST
    TAMANHO = 10;

TYPE
    Vetor = ARRAY [1..TAMANHO] OF INTEGER;

VAR
    nums: Vetor;

PROCEDURE BubbleSort(VAR arr: Vetor);
VAR
    i, j, temp: INTEGER;
BEGIN
    FOR i := TAMANHO-1 TO 1 BY -1 DO
        FOR j := 1 TO i DO
            IF arr[j] > arr[j+1] THEN
                temp := arr[j];
                arr[j] := arr[j+1];
                arr[j+1] := temp;
            END;
        END;
    END;
END BubbleSort;

PROCEDURE PreencherVetor(VAR arr: Vetor);
VAR
    i: INTEGER;
BEGIN
    WriteString(Output, "Digite os ", TAMANHO, " números do vetor: ");
    FOR i := 1 TO TAMANHO DO
        Read(arr[i]);
    END;
END PreencherVetor;

PROCEDURE ImprimirVetor(arr: Vetor);
VAR
    i: INTEGER;
BEGIN
    WriteString(Output, "Vetor ordenado: ");
    FOR i := 1 TO TAMANHO DO
        WriteInt(Output, arr[i]);
        WriteString(Output, " ");
    END;
    WriteLn(Output);
END ImprimirVetor;

BEGIN
    WriteString(Output, "Ordenação de um vetor utilizando Bubble Sort");
    WriteLn(Output);

    PreencherVetor(nums);
    WriteLn(Output);

    WriteString(Output, "Vetor original: ");
    FOR i := 1 TO TAMANHO DO
        WriteInt(Output, nums[i]);
        WriteString(Output, " ");
    END;
    WriteLn(Output);

    BubbleSort(nums);
    WriteLn(Output);

    ImprimirVetor(nums);
END Ordenacao.
```

Este código em MODULA-2 implementa o algoritmo Bubble Sort para ordenar um vetor de tamanho fixo. O usuário é solicitado a inserir os números do vetor, que serão armazenados na variável `nums`.

Em seguida, o programa exibe o vetor original e realiza a ordenação utilizando a função `BubbleSort`. Essa função compara os elementos adjacentes do vetor e os troca de posição se estiverem em ordem incorreta. Esse processo é repetido até que o vetor esteja completamente ordenado.

Por fim, o programa exibe o vetor ordenado utilizando a função `ImprimirVetor`.

Note que o código está comentado em português e utiliza uma abordagem procedural para facilitar a compreensão do algoritmo.