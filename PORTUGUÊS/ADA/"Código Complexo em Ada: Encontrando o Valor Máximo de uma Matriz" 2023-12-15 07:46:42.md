Certamente! Aqui está um código complexo em ADA:

```
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Complexo is
   -- Definindo um tipo para representar uma matriz
   type Matriz is array (Positive range <>) of array (Positive range <>)
     of Integer;

   -- Função para obter o valor máximo de uma matriz
   function Valor_Maximo(M: Matriz) return Integer is
      Maximo: Integer := M(1)(1);
   begin
      for I in M'Range(1) loop
         for J in M(I)'Range(1) loop
            if M(I)(J) > Maximo then
               Maximo := M(I)(J);
            end if;
         end loop;
      end loop;
      return Maximo;
   end Valor_Maximo;

   -- Procedimento para imprimir uma matriz
   procedure Imprimir_Matriz(M: Matriz) is
   begin
      for I in M'Range(1) loop
         for J in M(I)'Range(1) loop
            Put(Item => M(I)(J), Width => 5);
         end loop;
         New_Line;
      end loop;
   end Imprimir_Matriz;

   -- Procedimento principal
   procedure Main is
      M: Matriz(1..3, 1..4) :=
         ( (1, 2, 3, 4),
           (5, 6, 7, 8),
           (9, 10, 11, 12) );
      Max: Integer;
   begin
      Put_Line("Matriz original:");
      Imprimir_Matriz(M);
      Max := Valor_Maximo(M);
      Put_Line("O valor máximo da matriz é: " & Integer'Image(Max));
   end Main;
begin
   Main;
end Complexo;
```

Neste código em Ada, inicialmente definimos um tipo `Matriz`, que é uma matriz de inteiros. Em seguida, criamos uma função `Valor_Maximo` que recebe uma matriz como parâmetro e retorna o valor máximo encontrado nessa matriz.

Também temos um procedimento `Imprimir_Matriz` que recebe uma matriz como parâmetro e imprime seus elementos na tela. 

No procedimento principal `Main`, criamos uma matriz `M` de tamanho 3x4 e a preenchemos com alguns valores. Em seguida, chamamos a função `Valor_Maximo` para obter o valor máximo da matriz e o armazenamos na variável `Max`. Por fim, imprimimos a matriz original e o valor máximo encontrado.

Espero que este código atenda às suas expectativas!