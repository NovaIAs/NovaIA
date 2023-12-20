Aqui está um exemplo de código em MODULA-2 que realiza a ordenação de um vetor utilizando o algoritmo de Bubble Sort:

```modula-2
MODULE BubbleSort;

IMPORT InOut, Random;

CONST
  TAMANHO_VETOR = 10;

TYPE
  Vetor = ARRAY [1..TAMANHO_VETOR] OF INTEGER;

VAR
  numeros: Vetor;

PROCEDURE PreencherVetor(VAR v: Vetor);
VAR
  i: INTEGER;
BEGIN
  FOR i := 1 TO TAMANHO_VETOR DO
    v[i] := Random.Random(100); (* Preenche o vetor com números aleatórios de 0 a 99 *)
  END;
END PreencherVetor;

PROCEDURE ImprimirVetor(VAR v: Vetor);
VAR
  i: INTEGER;
BEGIN
  FOR i := 1 TO TAMANHO_VETOR DO
    InOut.WriteInt(v[i], 4); (* Imprime cada elemento do vetor *)
  END;
  InOut.WriteLn;
END ImprimirVetor;

PROCEDURE BubbleSort(VAR v: Vetor);
VAR
  i, j, temp: INTEGER;
BEGIN
  FOR i := 1 TO TAMANHO_VETOR - 1 DO
    FOR j := 1 TO TAMANHO_VETOR - i DO
      IF v[j] > v[j+1] THEN (* Compara dois elementos adjacentes *)
        temp := v[j];
        v[j] := v[j+1]; (* Troca os elementos de posição *)
        v[j+1] := temp;
      END;
    END;
  END;
END BubbleSort;

BEGIN
  PreencherVetor(numeros);
  
  InOut.WriteLn("Vetor original:");
  ImprimirVetor(numeros);
  
  BubbleSort(numeros);
  
  InOut.WriteLn("Vetor ordenado:");
  ImprimirVetor(numeros);
  
  InOut.ReadLn; (* Aguarda uma tecla ser pressionada antes de finalizar *)
END BubbleSort.
```

Este código em MODULA-2 realiza a ordenação de um vetor de inteiros utilizando o algoritmo de Bubble Sort. 

Primeiro, o vetor `numeros` é preenchido com números aleatórios de 0 a 99 através da função `PreencherVetor`. Em seguida, o vetor original é impresso na tela utilizando a função `ImprimirVetor`.

O algoritmo Bubble Sort é implementado na função `BubbleSort`. Ele percorre o vetor várias vezes, comparando elementos adjacentes e trocando-os de posição se estiverem fora de ordem. O algoritmo continua executando até que nenhum elemento seja trocado de posição, garantindo que o vetor esteja ordenado ao final.

Após a ordenação, o vetor é impresso novamente utilizando a função `ImprimirVetor` para verificar o resultado.

Por fim, o programa aguarda a leitura de uma entrada para finalizar a execução.