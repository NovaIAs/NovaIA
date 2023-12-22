Claro! Aqui está um código em ADA que implementa o algoritmo de ordenação QuickSort. O QuickSort é um algoritmo de ordenação eficiente que utiliza a técnica de divisão e conquista.

```
with Ada.Text_IO; use Ada.Text_IO;

procedure QuickSort is

   type Vetor is array (Integer range <>) of Integer;
   procedure Ordenar (A : in out Vetor);

   procedure Trocar (A : in out Vetor; I, J : Integer) is
      Temp : Integer;
   begin
      Temp := A(I);
      A(I) := A(J);
      A(J) := Temp;
   end Trocar;

   procedure Particionar (A : in out Vetor; Esquerda, Direita : Integer; Pivo : out Integer) is
      I, J : Integer;
   begin
      Pivo := A((Esquerda + Direita) / 2);
      I := Esquerda;
      J := Direita;
      loop
         while A(I) < Pivo loop
            I := I + 1;
         end loop;
         while A(J) > Pivo loop
            J := J - 1;
         end loop;
         exit when I >= J;
         Trocar(A, I, J);
         I := I + 1;
         J := J - 1;
      end loop;
   end Particionar;

   procedure Ordenar (A : in out Vetor) is
      Pivo : Integer;
      Esquerda, Direita : Integer;
   begin
      if A'Length <= 1 then
         return;
      else
         Esquerda := A'First;
         Direita := A'Last;
         Particionar(A, Esquerda, Direita, Pivo);
         if Esquerda < Pivo - 1 then
            Ordenar(A(Esquerda .. Pivo - 1));
         end if;
         if Pivo + 1 < Direita then
            Ordenar(A(Pivo + 1 .. Direita));
         end if;
      end if;
   end Ordenar;

   V : Vetor(1 .. 10) := (9, 4, 6, 2, 8, 1, 5, 10, 3, 7);
begin
   Put("Vetor original: ");
   for I in V'Range loop
      Put(V(I)'Image & " ");
   end loop;
   New_Line;

   Ordenar(V);

   Put("Vetor ordenado: ");
   for I in V'Range loop
      Put(V(I)'Image & " ");
   end loop;
   New_Line;
end QuickSort;
```

Neste código, a função principal é `QuickSort`. Primeiro, definimos um tipo `Vetor` que é um array de inteiros. Em seguida, temos a função `Trocar`, que é usada para trocar dois elementos do vetor.

A função `Particionar` é a parte principal do algoritmo QuickSort. Ela seleciona um elemento do vetor como o pivô e rearranja os elementos de forma que todos os elementos menores que o pivô estejam à esquerda e os elementos maiores estejam à direita. Ela retorna a posição do pivô.

A função `Ordenar` é recursiva e utiliza a função `Particionar` para dividir o vetor em subvetores menores. A função é chamada novamente para ordenar os subvetores à esquerda e à direita do pivô.

No final do código, temos um exemplo de uso da função `QuickSort`. É criado um vetor de 10 elementos e o algoritmo é aplicado a ele. Os elementos do vetor original e do vetor ordenado são impressos na saída padrão.