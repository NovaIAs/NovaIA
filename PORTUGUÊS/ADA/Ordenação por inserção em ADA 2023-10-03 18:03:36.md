Claro! Aqui está um exemplo de um programa em ADA que realiza uma ordenação de números usando o algoritmo de ordenação por inserção:

```
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure OrdenacaoPorInsercao is
   type Vetor is array (Positive range <>) of Integer;
   procedure Ordenar (A : in out Vetor) is
      N : constant Positive := A'Last;
      J : Integer;
      Chave : Integer;
   begin
      for I in 2 .. N loop
         Chave := A (I);
         J := I - 1;
         while J >= 1 and then A (J) > Chave loop
            A (J + 1) := A (J);
            J := J - 1;
         end loop;
         A (J + 1) := Chave;
      end loop;
   end Ordenar;

   A : Vetor (1 .. 10) := (9, 5, 2, 7, 1, 3, 8, 6, 4, 10);
begin
   Put_Line ("Vetor original:");
   for I in A'range loop
      Put (A (I));
      if I < A'Last then
         Put (", ");
      end if;
   end loop;
   New_Line;

   Ordenar (A);

   Put_Line ("Vetor ordenado:");
   for I in A'range loop
      Put (A (I));
      if I < A'Last then
         Put (", ");
      end if;
   end loop;
   New_Line;
end OrdenacaoPorInsercao;
```

Neste código, começamos definindo um tipo `Vetor` que é uma matriz unidimensional de inteiros. Em seguida, temos uma sub-rotina chamada `Ordenar`, que recebe como parâmetro um vetor e o ordena usando o algoritmo de ordenação por inserção.

Dentro da sub-rotina `Ordenar`, temos um loop que percorre o vetor a partir do segundo elemento. Para cada elemento, armazenamos seu valor na variável `Chave`. Em seguida, comparamos o valor da chave com os elementos anteriores do vetor e, se encontrarmos um elemento maior, movemos esse elemento uma posição à frente. Isso é feito até que encontremos a posição correta para inserir a chave. Finalmente, colocamos a chave na posição correta.

Na parte principal do programa, definimos um vetor chamado `A` com 10 elementos desordenados. Em seguida, imprimimos o vetor original na tela. Chamamos então a sub-rotina `Ordenar`, passando o vetor como parâmetro. Por fim, imprimimos o vetor ordenado na tela.

Esse exemplo demonstra um código em ADA que realiza uma ordenação por inserção em um vetor de inteiros. É uma implementação simples, mas eficiente para pequenos conjuntos de dados.