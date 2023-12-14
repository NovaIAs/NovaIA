Claro! Aqui está um exemplo de um código em ADA que realiza a ordenação de um array utilizando o algoritmo de ordenação por seleção:

```
-- Programa em ADA para ordenação de um array utilizando o algoritmo de ordenação por seleção

with Ada.Text_IO; use Ada.Text_IO;

procedure OrdenacaoPorSelecao is
  
   type Array_Inteiros is array (Integer range <>) of Integer;

   -- Função para encontrar o índice do menor elemento no array
   function IndiceMenorElemento (A : Array_Inteiros; Primeiro, Ultimo : Integer) return Integer is
      Menor : Integer := Primeiro;
   begin
      for i in Primeiro+1 .. Ultimo loop
         if A(i) < A(Menor) then
            Menor := i;
         end if;
      end loop;
      return Menor;
   end IndiceMenorElemento;

   -- Procedimento para trocar dois elementos de posição no array
   procedure TrocarElementos (A : in out Array_Inteiros; i, j : Integer) is
      Temp : Integer;
   begin
      Temp := A(i);
      A(i) := A(j);
      A(j) := Temp;
   end TrocarElementos;

   -- Procedimento para imprimir o array
   procedure ImprimirArray (A : Array_Inteiros) is
   begin
      for i in A'Range loop
         Put(A(i));
         Put(" ");
      end loop;
      New_Line;
   end ImprimirArray;

   -- Procedimento principal
   procedure Main is
      A : Array_Inteiros := (9, 4, 2, 7, 5, 1, 6, 8, 3);
   begin
      Put("Array original: ");
      ImprimirArray(A);
      
      for i in A'First .. A'Last-1 loop
         -- Encontra o índice do menor elemento no array não ordenado
         Menor := IndiceMenorElemento(A, i, A'Last);
         
         -- Troca o menor elemento com o primeiro elemento não ordenado
         TrocarElementos(A, i, Menor);
      end loop;

      Put("Array ordenado: ");
      ImprimirArray(A);
   end Main;

begin
   Main;
end OrdenacaoPorSelecao;
```

Neste código, utilizamos a linguagem ADA para implementar o algoritmo de ordenação por seleção. Primeiramente, definimos o tipo `Array_Inteiros` que representa um array de inteiros.

Em seguida, temos a função `IndiceMenorElemento`, que recebe como parâmetros o array `A`, o índice do primeiro elemento não ordenado (`Primeiro`) e o índice do último elemento do array (`Ultimo`). Essa função percorre o array a partir do índice `Primeiro+1` até `Ultimo`, comparando cada elemento com o elemento de menor valor encontrado até o momento. Ao final, ela retorna o índice do menor elemento encontrado.

O procedimento `TrocarElementos` recebe como parâmetros o array `A` e os índices `i` e `j`. Ele realiza a troca de posição dos elementos nos índices `i` e `j` do array `A`.

O procedimento `ImprimirArray` recebe como parâmetro o array `A` e imprime seus elementos na saída padrão.

No procedimento principal `Main`, definimos um array de exemplo `A` e utilizamos um loop para percorrer os elementos não ordenados do array. Em cada iteração, encontramos o menor elemento do array não ordenado utilizando a função `IndiceMenorElemento` e trocamos sua posição com o primeiro elemento não ordenado utilizando o procedimento `TrocarElementos`. Ao final, imprimimos o array original e o array ordenado utilizando o procedimento `ImprimirArray`.

Por fim, chamamos o procedimento `Main` para executar o programa.