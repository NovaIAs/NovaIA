Claro! Aqui está um exemplo de código complexo em ADA, escrito em português:

```
------------------------------------------------------------------------
--  Programa: Manipulação de Matrizes
--  Autor: NovaIA
--  Data: 01/01/2023
--  Descrição: Este programa realiza operações básicas em matrizes,
--             como soma, multiplicação e transposição.
------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Manipulacao_de_Matrizes is

   -- Tamanho máximo das matrizes
   Max_Linhas : constant Natural := 10;
   Max_Colunas : constant Natural := 10;

   -- Definição do tipo Matriz
   type Matriz is array(1..Max_Linhas, 1..Max_Colunas) of Float;

   -- Função para leitura dos elementos de uma matriz
   procedure Ler_Matriz(M: out Matriz; Linhas, Colunas: in Natural) is
   begin
      for I in 1..Linhas loop
         for J in 1..Colunas loop
            Put("Informe o elemento da posição (");
            Put(I);
            Put(",");
            Put(J);
            Put("): ");
            Get(M(I,J));
         end loop;
      end loop;
   end Ler_Matriz;

   -- Função para impressão dos elementos de uma matriz
   procedure Imprimir_Matriz(M: in Matriz; Linhas, Colunas: in Natural) is
   begin
      for I in 1..Linhas loop
         for J in 1..Colunas loop
            Put(M(I,J), Width => 8, Fore => 2);
         end loop;
         New_Line;
      end loop;
   end Imprimir_Matriz;

   -- Função para somar duas matrizes
   procedure Somar_Matrizes(A, B: in Matriz; Linhas, Colunas: in Natural; Soma: out Matriz) is
   begin
      for I in 1..Linhas loop
         for J in 1..Colunas loop
            Soma(I,J) := A(I,J) + B(I,J);
         end loop;
      end loop;
   end Somar_Matrizes;

   -- Função para multiplicar duas matrizes
   procedure Multiplicar_Matrizes(A, B: in Matriz; Linhas_A, Colunas_A, Linhas_B, Colunas_B: in Natural; Multiplicacao: out Matriz) is
   begin
      if Colunas_A /= Linhas_B then
         Put_Line("Erro: O número de colunas da matriz A deve ser igual ao número de linhas da matriz B.");
      else
         for I in 1..Linhas_A loop
            for J in 1..Colunas_B loop
               Multiplicacao(I, J) := 0.0;
               for K in 1..Colunas_A loop
                  Multiplicacao(I, J) := Multiplicacao(I, J) + A(I, K) * B(K, J);
               end loop;
            end loop;
         end if;
      end loop;
   end Multiplicar_Matrizes;

   -- Função para transpor uma matriz
   procedure Transpor_Matriz(A: in Matriz; Linhas, Colunas: in Natural; Transposta: out Matriz) is
   begin
      for I in 1..Linhas loop
         for J in 1..Colunas loop
            Transposta(J, I) := A(I, J);
         end loop;
      end loop;
   end Transpor_Matriz;

   -- Variáveis locais
   A, B, Soma, Multiplicacao, Transposta: Matriz;
   Linhas_A, Colunas_A, Linhas_B, Colunas_B: Natural;

begin

   -- Leitura das dimensões e elementos das matrizes A e B
   Put("Informe o número de linhas da matriz A: ");
   Get(Linhas_A);
   Put("Informe o número de colunas da matriz A: ");
   Get(Colunas_A);
   Ler_Matriz(A, Linhas_A, Colunas_A);

   Put("Informe o número de linhas da matriz B: ");
   Get(Linhas_B);
   Put("Informe o número de colunas da matriz B: ");
   Get(Colunas_B);
   Ler_Matriz(B, Linhas_B, Colunas_B);

   -- Operações com matrizes
   Put_Line("Matriz A:");
   Imprimir_Matriz(A, Linhas_A, Colunas_A);

   Put_Line("Matriz B:");
   Imprimir_Matriz(B, Linhas_B, Colunas_B);

   Put_Line("Soma de A e B:");
   Somar_Matrizes(A, B, Linhas_A, Colunas_A, Soma);
   Imprimir_Matriz(Soma, Linhas_A, Colunas_A);

   Put_Line("Multiplicação de A e B:");
   Multiplicar_Matrizes(A, B, Linhas_A, Colunas_A, Linhas_B, Colunas_B, Multiplicacao);
   Imprimir_Matriz(Multiplicacao, Linhas_A, Colunas_B);

   Put_Line("Transposta de A:");
   Transpor_Matriz(A, Linhas_A, Colunas_A, Transposta);
   Imprimir_Matriz(Transposta, Colunas_A, Linhas_A);

end Manipulacao_de_Matrizes;
```

Este é um programa em ADA que realiza manipulações básicas em matrizes, como soma, multiplicação e transposição. Ele utiliza o pacote `Ada.Text_IO` para realizar a leitura e impressão dos elementos das matrizes.

A função `Ler_Matriz` é responsável por ler os elementos de uma matriz do usuário. Ela utiliza um loop para percorrer todas as posições da matriz e solicitar ao usuário que informe o valor de cada elemento.

A função `Imprimir_Matriz` é responsável por imprimir os elementos de uma matriz. Ela também utiliza um loop para percorrer todas as posições da matriz e utiliza a função `Put` do pacote `Ada.Text_IO` para imprimir cada elemento.

A função `Somar_Matrizes` realiza a soma entre duas matrizes. Ela utiliza um loop para percorrer todas as posições das matrizes A e B e armazenar o resultado da soma na matriz Soma.

A função `Multiplicar_Matrizes` realiza a multiplicação entre duas matrizes. Antes de realizar a multiplicação, ela verifica se o número de colunas da matriz A é igual ao número de linhas da matriz B, pois essa é uma condição necessária para que a multiplicação seja válida. Em seguida, ela utiliza três loops para percorrer todas as posições das matrizes A e B, e realiza a multiplicação dos elementos correspondentes.

A função `Transpor_Matriz` realiza a transposição de uma matriz. Ela utiliza dois loops para percorrer todas as posições da matriz A e armazenar o valor transposto na matriz Transposta.

No programa principal, o usuário informa as dimensões e elementos das matrizes A e B. Em seguida, o programa realiza as operações de soma, multiplicação e transposição e imprime os resultados.

Espero que este código em ADA atenda às suas expectativas! Se tiver alguma dúvida, estou à disposição para ajudar.