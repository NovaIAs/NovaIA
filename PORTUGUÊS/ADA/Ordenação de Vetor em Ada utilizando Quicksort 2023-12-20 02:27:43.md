Claro! Aqui está um exemplo de código em Ada que realiza a ordenação de um vetor utilizando o algoritmo Quicksort:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Quicksort is
   type Vetor is array (Integer range <>) of Integer;

   procedure Trocar (A: in out Vetor; I, J: Integer) is
      Temp: Integer;
   begin
      Temp := A(I);
      A(I) := A(J);
      A(J) := Temp;
   end Trocar;

   procedure Ordenar (A: in out Vetor; Baixo, Alto: Integer) is
      Pivot, I, J: Integer;
   begin
      if Baixo < Alto then
         Pivot := A((Baixo + Alto) / 2);
         I := Baixo;
         J := Alto;

         loop
            while A(I) < Pivot loop
               I := I + 1;
            end loop;

            while A(J) > Pivot loop
               J := J - 1;
            end loop;

            if I <= J then
               Trocar(A, I, J);
               I := I + 1;
               J := J - 1;
            end if;

            exit when I > J;
         end loop;

         Ordenar(A, Baixo, J);
         Ordenar(A, I, Alto);
      end if;
   end Ordenar;

   procedure Imprimir (A: in Vetor) is
   begin
      for I in A'Range loop
         Put(A(I));
         Put(" ");
      end loop;
      New_Line;
   end Imprimir;

   V: Vetor(1..10) := (9, 3, 6, 2, 8, 1, 5, 4, 7, 10);
begin
   Put("Vetor original: ");
   Imprimir(V);

   Ordenar(V, V'First, V'Last);

   Put("Vetor ordenado: ");
   Imprimir(V);
end Quicksort;
```

Este código é escrito em Ada e implementa o algoritmo Quicksort para ordenar um vetor de inteiros. 

O programa começa definindo um novo tipo de dado chamado `Vetor`, que representa um array de inteiros. Em seguida, temos o procedimento `Trocar`, que recebe um vetor `A` e dois índices `I` e `J` como parâmetros e realiza a troca de valores entre as posições `I` e `J` do vetor.

O procedimento `Ordenar` implementa o algoritmo Quicksort recursivamente. Recebe o vetor `A`, o índice `Baixo` e o índice `Alto` como parâmetros. Dentro do procedimento, primeiro verifica se `Baixo < Alto`, caso contrário, o vetor já está ordenado. Em seguida, seleciona o pivô como o valor central do vetor e inicializa os índices `I` e `J` com os valores mais baixos e mais altos, respectivamente.

Em um loop, o algoritmo encontra os elementos menores que o pivô na parte esquerda do vetor e os elementos maiores que o pivô na parte direita do vetor. Quando encontra um par de elementos que precisa ser trocado, chama o procedimento `Trocar`. O loop continua até que `I` seja maior que `J`.

Após o loop, o vetor é dividido em duas partes menores e a função `Ordenar` é chamada recursivamente para ordenar essas partes. O procedimento `Imprimir` é utilizado para exibir o vetor antes e depois da ordenação.

No programa principal, um vetor de teste `V` é declarado e inicializado com valores desordenados. Em seguida, chama-se o procedimento `Imprimir` para exibir o vetor original. Em seguida, chama-se o procedimento `Ordenar` para ordenar o vetor e, por fim, chama-se novamente o procedimento `Imprimir` para exibir o vetor ordenado.