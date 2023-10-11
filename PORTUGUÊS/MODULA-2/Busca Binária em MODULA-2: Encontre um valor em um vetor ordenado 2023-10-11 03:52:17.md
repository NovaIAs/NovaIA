Claro! Aqui está um exemplo de código em MODULA-2 que realiza uma busca binária em um vetor ordenado. O código está em português e contém comentários explicativos para facilitar o entendimento:

```modula2
MODULE BuscaBinaria;

FROM InOut IMPORT ReadInt, WriteString, WriteInt, WriteLn;

CONST
    TAMANHO_VETOR = 10;

TYPE
    Vetor = ARRAY [1..TAMANHO_VETOR] OF INTEGER;

VAR
    vetorOrdenado: Vetor;
    valorBusca: INTEGER;

PROCEDURE PreencherVetor(VAR vetor: Vetor);
VAR
    i: INTEGER;
BEGIN
    FOR i := 1 TO TAMANHO_VETOR DO
        vetor[i] := i * 2;  // Preenche o vetor com valores pares
    END;
END PreencherVetor;

FUNCTION BuscaBinaria(VAR vetor: Vetor; valor: INTEGER): INTEGER;
VAR
    inicio, meio, fim: INTEGER;
BEGIN
    inicio := 1;                   // Início do vetor
    fim := TAMANHO_VETOR;          // Fim do vetor

    WHILE inicio <= fim DO
        meio := (inicio + fim) DIV 2;  // Encontra o meio do vetor

        IF vetor[meio] = valor THEN
            RETURN meio;    // Valor encontrado
        ELSIF vetor[meio] < valor THEN
            inicio := meio + 1;  // Descarta a metade inferior do vetor
        ELSE
            fim := meio - 1;     // Descarta a metade superior do vetor
        END;
    END;

    RETURN -1;  // Valor não encontrado
END BuscaBinaria;

BEGIN
    WriteString("Informe um valor a ser buscado: ");
    ReadInt(valorBusca);

    PreencherVetor(vetorOrdenado);

    WriteString("Vetor ordenado: ");
    FOR i := 1 TO TAMANHO_VETOR DO
        WriteInt(vetorOrdenado[i]);
        WriteString(" ");
    END;
    WriteLn;

    posicao := BuscaBinaria(vetorOrdenado, valorBusca);

    IF posicao <> -1 THEN
        WriteString("O valor ");
        WriteInt(valorBusca);
        WriteString(" foi encontrado na posição ");
        WriteInt(posicao);
        WriteLn;
    ELSE
        WriteString("O valor ");
        WriteInt(valorBusca);
        WriteString(" não foi encontrado no vetor.");
        WriteLn;
    END;
END BuscaBinaria.
```

Neste exemplo, o código começa preenchendo um vetor com valores pares de 2 até 20. Em seguida, solicita ao usuário um valor a ser buscado. A função `BuscaBinaria` realiza a busca binária neste vetor ordenado, retornando a posição do valor caso ele seja encontrado, ou -1 caso contrário.

O resultado da busca é então exibido na tela, informando se o valor foi encontrado e em qual posição do vetor ele se encontra.