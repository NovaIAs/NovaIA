Claro! Aqui está um código em ADA que realiza a ordenação de um vetor utilizando o algoritmo de ordenação por inserção:

```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Ordenacao is

   type Vetor is array (Integer range <>) of Integer;

   procedure Ordenacao_Insercao (A : in out Vetor) is
      Valor : Integer;
      J : Integer;
   begin
      for I in 1 .. A'Length - 1 loop
         Valor := A(I+1);
         J := I;
         while J > 0 and A(J) > Valor loop
            A(J+1) := A(J);
            J := J - 1;
         end loop;
         A(J+1) := Valor;
      end loop;
   end Ordenacao_Insercao;

   V : Vetor (1 .. 10) := (5, 2, 9, 1, 7, 3, 6, 4, 8, 10);

begin
   Ada.Text_IO.Put_Line("Vetor original:");
   for I in V'Range loop
      Ada.Integer_Text_IO.Put(Item => V(I), Width => 3);
   end loop;
   Ada.Text_IO.New_Line;

   Ordenacao_Insercao(A => V);

   Ada.Text_IO.Put_Line("Vetor ordenado:");
   for I in V'Range loop
      Ada.Integer_Text_IO.Put(Item => V(I), Width => 3);
   end loop;
   Ada.Text_IO.New_Line;

end Ordenacao;
```

Este código em ADA implementa o algoritmo de ordenação por inserção para ordenar um vetor de números inteiros. 

A primeira parte do código define um tipo de dado chamado `Vetor` que é um array de inteiros. Em seguida, temos a declaração do procedimento `Ordenacao_Insercao` que recebe um vetor como parâmetro e realiza a ordenação por inserção.

Dentro do procedimento `Ordenacao_Insercao`, o algoritmo percorre o vetor da esquerda para a direita, a partir do segundo elemento. Em cada iteração, o valor atual é armazenado na variável `Valor` e o índice `J` é inicializado com o índice anterior. Enquanto `J` for maior que zero e o elemento anterior for maior que `Valor`, os elementos são deslocados uma posição à direita. Ao final, o valor `Valor` é inserido na posição correta.

Na parte principal do programa, é declarado um vetor `V` com 10 elementos e é exibido na tela o vetor original. Em seguida, o procedimento `Ordenacao_Insercao` é chamado passando o vetor `V` como argumento. Por fim, é exibido o vetor ordenado na tela.

Espero que este código atenda às suas expectativas!