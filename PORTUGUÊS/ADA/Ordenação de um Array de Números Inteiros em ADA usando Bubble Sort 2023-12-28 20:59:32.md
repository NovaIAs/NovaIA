Claro, vou gerar um código complexo em ADA para você. Aqui está um exemplo de código em ADA que utiliza o algoritmo de ordenação "Bubble Sort" para ordenar um array de números inteiros em ordem crescente. Farei um código bem comentado e explicativo:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Bubble_Sort is
   -- Declaração do array a ser ordenado
   type Array_Inteiros is array (Integer range <>) of Integer;
   A : Array_Inteiros (1..10) := (3, 1, 4, 1, 5, 9, 2, 6, 5, 3);
   
   -- Variável auxiliar para troca de elementos
   Temp : Integer;
   
begin
   -- Imprime o array original
   Put("Array original: ");
   for I in A'Range loop
      Put(A(I)'Img & " ");
   end loop;
   New_Line;
   
   -- Loop principal do Bubble Sort
   for I in A'First .. A'Last - 1 loop
      for J in A'First .. A'Last - I loop
         -- Verifica se o elemento atual é maior que o próximo
         if A(J) > A(J + 1) then
            -- Realiza a troca dos elementos
            Temp := A(J);
            A(J) := A(J + 1);
            A(J + 1) := Temp;
         end if;
      end loop;
   end loop;
   
   -- Imprime o array ordenado
   Put("Array ordenado: ");
   for I in A'Range loop
      Put(A(I)'Img & " ");
   end loop;
   New_Line;
   
end Bubble_Sort;
```

Neste código, começamos declarando um tipo de dado `Array_Inteiros` que é um array de inteiros. Em seguida, criamos uma instância `A` desse tipo com um array de 10 elementos. 

O algoritmo de ordenação utilizado é o Bubble Sort, que consiste em comparar pares de elementos adjacentes e trocá-los se estiverem na ordem errada. Esse processo é repetido várias vezes até que o array esteja completamente ordenado.

Dentro do loop principal do Bubble Sort, temos um loop interno que compara os elementos adjacentes e realiza as trocas, se necessário. A variável `Temp` é utilizada para realizar a troca dos elementos. 

Ao final do algoritmo, imprimimos o array original e o array ordenado na saída do programa.

*Observação:* Lembre-se de que este é apenas um exemplo de código complexo em ADA. A complexidade pode variar dependendo do tamanho do array e do problema que você está resolvendo.