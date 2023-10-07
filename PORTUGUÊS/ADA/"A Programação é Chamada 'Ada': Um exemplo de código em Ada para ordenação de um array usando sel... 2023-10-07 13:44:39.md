Claro! Aqui está um exemplo de um código complexo em Ada que realiza a ordenação de um array usando o algoritmo de ordenação por seleção:

```
with Ada.Text_IO; use Ada.Text_IO;

procedure Ordenacao is

   type Array_Inteiros is array (Integer range <>) of Integer;

   procedure Trocar (A: in out Array_Inteiros; I, J: Integer) is
      Temp: Integer;
   begin
      Temp := A(I);
      A(I) := A(J);
      A(J) := Temp;
   end Trocar;

   procedure Ordenacao_Por_Selecao (A: in out Array_Inteiros) is
      Minimo: Integer;
   begin
      for I in A'First .. A'Last - 1 loop
         Minimo := I;
         for J in I + 1 .. A'Last loop
            if A(J) < A(Minimo) then
               Minimo := J;
            end if;
         end loop;
         if Minimo /= I then
            Trocar(A, I, Minimo);
         end if;
      end loop;
   end Ordenacao_Por_Selecao;

   A: Array_Inteiros (1 .. 10) := (5, 9, 1, 3, 7, 2, 8, 6, 4, 10);

begin
   Put("Array antes da ordenacao: ");
   for Item of A loop
      Put(Item'Image & " ");
   end loop;
   New_Line;

   Ordenacao_Por_Selecao(A);

   Put("Array apos a ordenacao: ");
   for Item of A loop
      Put(Item'Image & " ");
   end loop;
   New_Line;
   
end Ordenacao;
```

Explicação do código:

1. A primeira linha do código importa o pacote `Ada.Text_IO`, que é necessário para exibir mensagens na saída padrão.
2. Em seguida, é definido um tipo chamado `Array_Inteiros`, que é um array de inteiros.
3. A função `Trocar` é definida para trocar dois elementos de um array.
4. A função `Ordenacao_Por_Selecao` implementa o algoritmo de ordenação por seleção, que itera pelo array e encontra o menor elemento em cada iteração, trocando-o com o elemento na posição atual.
5. No bloco principal do programa, é criado um array chamado `A` com 10 elementos.
6. A primeira parte do bloco principal imprime o array antes da ordenação.
7. Em seguida, a função `Ordenacao_Por_Selecao` é chamada para ordenar o array `A`.
8. Por fim, a última parte do bloco principal imprime o array após a ordenação.

Este código em Ada realiza a ordenação por seleção de um array de inteiros e o exibe antes e depois da ordenação. O algoritmo de ordenação por seleção é escolhido por sua simplicidade e facilidade de compreensão.