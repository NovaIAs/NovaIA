```modula-2
MODULE OrdenarVetor;
FROM FormatString IMPORT Print;

PROCEDURE Ordenar(VAR v: ARRAY OF CARDINAL):
VAR k, j: CARDINAL;
BEGIN
  FOR k := 0 TO LENGTH v - 1 DO
    FOR j := k + 1 TO LENGTH v - 1 DO
      IF v[k] > v[j] THEN
        SWAP v[k], v[j];
      FI;
    OD;
  OD;
END Ordenar;

VAR v: ARRAY OF CARDINAL := (1, 4, 2, 7, 3, 6, 9, 5, 8, 0);

Ordenar(v);

FOR k := 0 TO LENGTH v - 1 DO
  Print("%d ", v[k]);
OD;
END OrdenarVetor.
```

Este código ordena um vetor de números inteiros usando o algoritmo de bolha. O algoritmo de bolha percorre o vetor comparando cada elemento com o seguinte, e se o primeiro for maior que o segundo, ele os troca de posição. O algoritmo repete esse processo até que nenhum elemento do vetor possa ser trocado novamente.

O código acima foi escrito em Modula-2, uma linguagem de programação compilada desenvolvida na década de 1970. Modula-2 é uma linguagem de programação segura, eficiente e compacta que foi projetada para ser usada em sistemas operacionais e outros aplicativos de sistemas.

O código é dividido em três partes:

* A primeira parte é a declaração dos módulos que serão usados pelo programa. O módulo `FormatString` é usado para formatar strings e o módulo `OrdenarVetor` é o módulo que contém o código principal do programa.
* A segunda parte é a declaração das variáveis que serão usadas pelo programa. A variável `v` é um vetor de números inteiros que contém os números que serão ordenados.
* A terceira parte é o código principal do programa. A função `Ordenar` ordena o vetor `v` usando o algoritmo de bolha. Em seguida, o programa exibe o vetor ordenado na tela.