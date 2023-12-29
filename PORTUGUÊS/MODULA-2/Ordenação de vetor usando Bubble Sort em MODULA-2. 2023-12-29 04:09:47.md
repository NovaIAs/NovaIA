Claro! Aqui está um exemplo de código em MODULA-2 que atende aos seus requisitos:

```
MODULE MeuCodigo;

FROM InOut IMPORT WriteString, WriteLn;
FROM Input IMPORT ReadInt;

CONST
  TAMANHO_MAXIMO = 100;

TYPE
  Vetor = ARRAY [1..TAMANHO_MAXIMO] OF INTEGER;

VAR
  Arr: Vetor;
  N, i, j, temp: INTEGER;

PROCEDURE BubbleSort(VAR A: Vetor; tamanho: INTEGER);
VAR
  i, j, temp: INTEGER;
BEGIN
  FOR i := 1 TO tamanho DO
    FOR j := 1 TO tamanho - i DO
      IF A[j] > A[j+1] THEN
        temp := A[j];
        A[j] := A[j+1];
        A[j+1] := temp;
      END;
    END;
  END;
END BubbleSort;

BEGIN
  WriteString("Digite o tamanho do vetor: ");
  N := ReadInt();

  IF (N < 1) OR (N > TAMANHO_MAXIMO) THEN
    WriteString("Tamanho inválido!");
  ELSE
    WriteString("Digite os elementos do vetor: ");
    FOR i := 1 TO N DO
      Arr[i] := ReadInt();
    END;

    WriteString("Vetor antes da ordenação: ");
    FOR i := 1 TO N DO
      WriteString(Arr[i], " ");
    END;
    WriteLn();

    BubbleSort(Arr, N);

    WriteString("Vetor após a ordenação: ");
    FOR i := 1 TO N DO
      WriteString(Arr[i], " ");
    END;
    WriteLn();
  END;
END.
```

Este código implementa o algoritmo Bubble Sort em MODULA-2 para ordenar um vetor de inteiros. O usuário deve fornecer o tamanho do vetor e em seguida digitar os elementos. O vetor é então ordenado usando o algoritmo Bubble Sort e os elementos são exibidos antes e depois da ordenação.

A função BubbleSort realiza a ordenação do vetor recebido como parâmetro. Ela utiliza dois loops aninhados para comparar cada elemento com o próximo e realizar as trocas necessárias. O vetor é passado por referência para que as alterações sejam refletidas fora da função.

O programa principal inicia solicitando ao usuário o tamanho do vetor. Em seguida, verifica se o tamanho fornecido é válido. Se for, o usuário é solicitado a digitar os elementos do vetor. Após a digitação, o vetor é exibido antes da ordenação, a função BubbleSort é chamada para ordená-lo e, por fim, o vetor ordenado é exibido.

Espero que este código atenda às suas expectativas!