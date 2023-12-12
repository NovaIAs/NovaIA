Claro! Aqui está um exemplo de código em ADA que realiza a ordenação de um array de números usando o algoritmo Bubble Sort:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Bubble_Sort is
   type Array is array (1..10) of Integer;
   Numbers : Array := (5, 2, 8, 1, 9, 3, 7, 6, 4, 10);
   Temp : Integer;

begin
   -- Imprimir o array antes da ordenação
   Put("Array antes da ordenação: ");
   for I in Array'Range loop
      Put(Integer'Image(Numbers(I)) & " ");
   end loop;
   New_Line;

   -- Algoritmo Bubble Sort
   for I in Array'First + 1 .. Array'Last loop
      for J in Array'First .. Array'Last - I loop
         -- Comparar elementos adjacentes e trocar se necessário
         if Numbers(J) > Numbers(J+1) then
            Temp := Numbers(J);
            Numbers(J) := Numbers(J+1);
            Numbers(J+1) := Temp;
         end if;
      end loop;
   end loop;

   -- Imprimir o array após a ordenação
   Put("Array após a ordenação: ");
   for I in Array'Range loop
      Put(Integer'Image(Numbers(I)) & " ");
   end loop;
   New_Line;

end Bubble_Sort;
```

Este código define um tipo `Array` que é um array de 10 elementos do tipo `Integer`. Em seguida, declara-se um array chamado `Numbers` contendo 10 números inteiros desordenados.

O algoritmo de ordenação Bubble Sort é utilizado para classificar os elementos do array `Numbers`. Ele consiste em comparar os elementos adjacentes e, caso estejam fora de ordem, trocá-los de posição. O algoritmo percorre o array várias vezes até que todos os elementos estejam em ordem.

O primeiro laço `for` itera sobre os elementos do array a partir do segundo elemento (`Array'First + 1`) até o último elemento (`Array'Last`). O segundo laço `for` itera sobre os elementos do array, exceto o último elemento já ordenado (`Array'Last - I`). Dentro deste segundo laço, é feita a comparação dos elementos adjacentes e a troca de posição, se necessário.

Após a ordenação, o array é impresso antes e depois da ordenação usando o procedimento `Put` do módulo `Ada.Text_IO`. A função `Integer'Image` é utilizada para converter os elementos do array em sequências de caracteres antes de imprimi-los.

Espero que isso te ajude a entender como funciona o algoritmo Bubble Sort em ADA!