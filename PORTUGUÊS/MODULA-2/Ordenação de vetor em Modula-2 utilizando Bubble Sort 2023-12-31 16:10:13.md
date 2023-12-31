Claro! Aqui está um exemplo de código em MODULA-2 que realiza a ordenação de um vetor utilizando o algoritmo de Bubble Sort:

```
MODULE Ordenacao;

FROM InOut IMPORT
  WriteString, WriteLn, WriteInt;
FROM SYSTEM IMPORT
  ADR;

CONST
  MAX_ELEMENTOS = 10;

TYPE
  Vetor = ARRAY [1..MAX_ELEMENTOS] OF INTEGER;

VAR
  numeros: Vetor;

PROCEDURE BubbleSort(VAR vetor: Vetor);
VAR
  i, j, temp: INTEGER;
BEGIN
  FOR i := 1 TO MAX_ELEMENTOS - 1 DO
    FOR j := 1 TO MAX_ELEMENTOS - i DO
      IF vetor[j] > vetor[j + 1] THEN
        temp := vetor[j];
        vetor[j] := vetor[j + 1];
        vetor[j + 1] := temp;
      END;
    END;
  END;
END BubbleSort;

PROCEDURE PreencherVetor(VAR vetor: Vetor);
VAR
  i: INTEGER;
BEGIN
  FOR i := 1 TO MAX_ELEMENTOS DO
    WriteString('Digite o elemento ', i:2, ': ');
    ReadInt(vetor[i]);
  END;
END PreencherVetor;

PROCEDURE ImprimirVetor(vetor: Vetor);
VAR
  i: INTEGER;
BEGIN
  FOR i := 1 TO MAX_ELEMENTOS DO
    WriteInt(vetor[i], 4);
  END;
END ImprimirVetor;

BEGIN
  WriteString('Digite ', MAX_ELEMENTOS:2, ' elementos inteiros:');
  PreencherVetor(numeros);

  WriteString('Vetor original: ');
  ImprimirVetor(numeros);

  BubbleSort(numeros);

  WriteString('Vetor ordenado: ');
  ImprimirVetor(numeros);
END Ordenacao.
```

Neste código, o módulo `Ordenacao` contém as definições e procedimentos necessários para ordenar um vetor de inteiros utilizando o algoritmo Bubble Sort.

A constante `MAX_ELEMENTOS` define o tamanho máximo do vetor, e o tipo `Vetor` é definido como um array de inteiros com tamanho `MAX_ELEMENTOS`.

O procedimento `BubbleSort` implementa o algoritmo de ordenação, onde são utilizados dois loops `FOR` aninhados para comparar e trocar os elementos adjacentes do vetor, conforme necessário.

Os procedimentos `PreencherVetor` e `ImprimirVetor` são responsáveis por preencher e exibir o vetor, respectivamente.

No programa principal, o usuário é solicitado a digitar os elementos do vetor, que são preenchidos utilizando o procedimento `PreencherVetor`.

Em seguida, o vetor original é exibido utilizando o procedimento `ImprimirVetor`.

O algoritmo de ordenação `BubbleSort` é chamado, passando o vetor como parâmetro.

Por fim, o vetor ordenado é exibido utilizando o procedimento `ImprimirVetor`.

Espero que este código atenda às suas expectativas!