Claro! Aqui está um exemplo de código em ADA que realiza a ordenação de um vetor utilizando o algoritmo de ordenação por inserção:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Ordenacao_Insertion is
   type Vetor is array (Integer range <>) of Integer;

   procedure Ordenar (A : in out Vetor) is
      ItemTemp : Integer;
      J : Integer;
   begin
      for I in 1 .. A'Last loop
         ItemTemp := A(I);
         J := I - 1;
         while J >= 0 and A(J) > ItemTemp loop
            A(J + 1) := A(J);
            J := J - 1;
         end loop;
         A(J + 1) := ItemTemp;
      end loop;
   end Ordenar;

   V : Vetor (1 .. 10) := (4, 2, 8, 6, 10, 1, 3, 9, 7, 5);
begin
   Put_Line("Vetor original:");
   for I in V'Range loop
      Put(V(I)'Img & " ");
   end loop;
   New_Line;

   Ordenar(V);

   Put_Line("Vetor ordenado:");
   for I in V'Range loop
      Put(V(I)'Img & " ");
   end loop;
   New_Line;
end Ordenacao_Insertion;
```

Explicação do código:

1. A primeira linha, `with Ada.Text_IO; use Ada.Text_IO;`, importa a biblioteca `Ada.Text_IO`, que é utilizada para realizar operações de entrada e saída.
2. Em seguida, declaramos o procedimento principal, `Ordenacao_Insertion`.
3. Dentro do procedimento, definimos o tipo `Vetor`, que é um array de inteiros.
4. Em seguida, declaramos o procedimento `Ordenar`, que recebe um vetor como parâmetro e realiza a ordenação por inserção.
5. Dentro do procedimento `Ordenar`, declaramos as variáveis `ItemTemp` para armazenar o valor temporário de um elemento do vetor durante a ordenação, e `J` para controlar o índice do elemento anterior.
6. Utilizamos um loop `for` para percorrer todos os elementos do vetor.
7. A cada iteração do loop, armazenamos o valor do elemento atual em `ItemTemp`.
8. Em seguida, decrementamos `J` e verificamos se ele é maior ou igual a 0 e se o elemento no índice `J` é maior que `ItemTemp`.
9. Se a condição for verdadeira, movemos o elemento no índice `J` para a posição seguinte (`J + 1`).
10. Continuamos decrementando `J` até que a condição seja falsa.
11. Por fim, atribuímos o valor de `ItemTemp` na posição `J + 1`.
12. Voltamos para o loop principal e repetimos o processo para os próximos elementos.
13. Após a ordenação, exibimos o vetor original e o vetor ordenado utilizando o procedimento `Put_Line` para exibir cada elemento.
14. Por fim, chamamos o procedimento `Ordenar` passando o vetor `V` como argumento.