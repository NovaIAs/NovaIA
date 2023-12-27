Claro! Aqui está um exemplo de código em MODULA-2 que realiza a ordenação de um vetor de números usando o algoritmo de bubble sort, em Português:

```pascal
MODULE OrdenacaoBubbleSort;

FROM InOut IMPORT WriteString, WriteInt, WriteLn;
FROM Random IMPORT Randomize, RandomWhole, RandomizeSeed;

CONST
  TAMANHO_VETOR = 10;

TYPE
  Vetor = ARRAY [1..TAMANHO_VETOR] OF INTEGER;

VAR
  numeros: Vetor;

PROCEDURE InicializarVetor(VAR v: Vetor);
VAR
  i: INTEGER;
BEGIN
  FOR i := 1 TO TAMANHO_VETOR DO
    v[i] := RandomWhole(100);  (* preenche o vetor com números aleatórios de 0 a 99 *)
  END;
END InicializarVetor;

PROCEDURE ImprimirVetor(v: Vetor);
VAR
  i: INTEGER;
BEGIN
  FOR i := 1 TO TAMANHO_VETOR DO
    WriteInt(v[i], 3);
  END;
  WriteLn;
END ImprimirVetor;

PROCEDURE BubbleSort(VAR v: Vetor);
VAR
  i, j, temp: INTEGER;
BEGIN
  FOR i := TAMANHO_VETOR DOWNTO 2 DO
    FOR j := 1 TO i - 1 DO
      IF v[j] > v[j + 1] THEN
        temp := v[j];
        v[j] := v[j + 1];
        v[j + 1] := temp;
      END;
    END;
  END;
END BubbleSort;

BEGIN
  RandomizeSeed;  (* inicializa a semente do gerador de números aleatórios *)
  Randomize;     (* gera uma nova sequência de números aleatórios *)

  WriteString("Vetor original: ");
  InicializarVetor(numeros);
  ImprimirVetor(numeros);

  WriteString("Vetor ordenado: ");
  BubbleSort(numeros);
  ImprimirVetor(numeros);
END OrdenacaoBubbleSort.
```

Este código em MODULA-2 implementa o algoritmo de ordenação Bubble Sort para ordenar um vetor de números. Primeiro, o vetor é preenchido com números aleatórios entre 0 e 99. Em seguida, o vetor original e o vetor ordenado são impressos na tela.

O procedimento `InicializarVetor` é responsável por preencher o vetor com números aleatórios, utilizando a função `RandomWhole` da biblioteca `Random`.

O procedimento `ImprimirVetor` imprime os elementos do vetor na tela, utilizando a função `WriteInt` da biblioteca `InOut`.

O procedimento `BubbleSort` realiza a ordenação do vetor utilizando o algoritmo de Bubble Sort. Esse algoritmo compara elementos adjacentes do vetor e os troca de posição se estiverem na ordem errada, repetindo esse processo até que o vetor esteja completamente ordenado.

No programa principal, a semente do gerador de números aleatórios é inicializada com `RandomizeSeed` e, em seguida, uma nova sequência de números aleatórios é gerada com `Randomize`.

O vetor original é inicializado, impresso na tela, ordenado com `BubbleSort` e, novamente, impresso na tela para exibir o vetor já ordenado.