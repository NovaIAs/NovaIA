Claro, vou criar um código complexo em ADA para você. Aqui está um exemplo de um programa em ADA que implementa um algoritmo de ordenação chamado Bubble Sort. O Bubble Sort é um algoritmo simples, mas eficiente, que percorre uma lista várias vezes, comparando cada elemento adjacente e trocando-os se estiverem na ordem errada. 

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Bubble_Sort is

   type Array_Type is array(1..10) of Integer; -- Define o tipo para um array de 10 inteiros
   Lista : Array_Type := (9, 5, 2, 7, 1, 8, 6, 3, 10, 4); -- Inicializa a lista desordenada

   procedure Swap(Item1 : in out Integer; Item2 : in out Integer) is
      Temp : Integer;
   begin
      Temp := Item1;
      Item1 := Item2;
      Item2 := Temp;
   end Swap;

begin

   for I in 1..Lista'Length - 1 loop -- Percorre a lista
      for J in 1..Lista'Length - I loop -- Percorre a lista novamente
         if Lista(J) > Lista(J + 1) then -- Compara os elementos adjacentes
            Swap(Lista(J), Lista(J + 1)); -- Troca os elementos se estiverem na ordem errada
         end if;
      end loop;
   end loop;

   -- Exibe a lista ordenada
   Put("Lista Ordenada: ");
   for I in 1..Lista'Length loop
      Put(Integer'Image(Lista(I)) & " ");
   end loop;
   New_Line;

end Bubble_Sort;
```

Neste código, primeiro definimos o tipo de dados `Array_Type` como um array de 10 inteiros. Em seguida, inicializamos a variável `Lista` com valores desordenados.

O procedimento `Swap` é utilizado para trocar dois elementos de posição. Em seguida, temos dois loops aninhados que percorrem a lista várias vezes. O loop externo `for I in 1..Lista'Length - 1` controla o número de passagens pelo array, enquanto o loop interno `for J in 1..Lista'Length - I` percorre os elementos da lista e compara os elementos adjacentes. Se os elementos estiverem na ordem errada, eles são trocados usando o procedimento `Swap`.

Após a ordenação, o código exibe a lista ordenada na saída.

Espero que este código complexo em ADA atenda às suas expectativas.