Claro! Aqui está um código complexo em ADA que faz a leitura de um arquivo de entrada contendo números inteiros e realiza o cálculo da média desses números:

```ada
-- Programa para calcular a média de números inteiros
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure CalcularMedia is
   type Vetor is array (Integer range <>) of Integer;

   procedure LerArquivo (NomeArquivo : String; Numeros : out Vetor; TotalNumeros : out Integer) is
      Arquivo : File_Type;
      Numero : Integer;
   begin
      Open (File => Arquivo, Mode => In_File, Name => NomeArquivo);
      TotalNumeros := 0;
      while not End_Of_File (Arquivo) loop
         TotalNumeros := TotalNumeros + 1;
         Get (File => Arquivo, Item => Numero);
         Numeros (TotalNumeros) := Numero;
      end loop;
      Close (File => Arquivo);
   end LerArquivo;

   procedure CalcularMedia (Numeros : in Vetor; TotalNumeros : in Integer; Media : out Float) is
      Soma : Integer := 0;
   begin
      for I in Numeros'Range loop
         Soma := Soma + Numeros (I);
      end loop;
      Media := Float (Soma) / Float (TotalNumeros);
   end CalcularMedia;

   NomeArquivo : String := "numeros.txt";
   Numeros : Vetor (1 .. 1000);
   TotalNumeros : Integer;
   Media : Float;

begin
   Put_Line ("Calculando a média dos números contidos no arquivo " & NomeArquivo);

   LerArquivo (NomeArquivo, Numeros, TotalNumeros);
   CalcularMedia (Numeros, TotalNumeros, Media);

   Put_Line ("A média dos números é: ");
   Put (Media, 2, 2);
   New_Line;
end CalcularMedia;
```

Este código em ADA consiste em um programa que lê números inteiros de um arquivo de entrada chamado "numeros.txt" e calcula a média desses números. 

Primeiramente, definimos um tipo chamado `Vetor` que é um array de números inteiros. Em seguida, temos a função `LerArquivo`, que recebe o nome do arquivo e o vetor de números como parâmetros de saída. Essa função abre o arquivo, lê os números e os armazena no vetor. 

Em seguida, temos a função `CalcularMedia`, que recebe o vetor de números e o total de números como parâmetros de entrada, e calcula a média dos números utilizando um laço for. 

Dentro do procedimento principal `CalcularMedia`, definimos o nome do arquivo como "numeros.txt" e criamos um vetor chamado `Numeros` com tamanho 1000. Em seguida, invocamos a função `LerArquivo` para ler os números do arquivo para o vetor. Depois, chamamos a função `CalcularMedia` para calcular a média dos números. 

Por fim, exibimos a média dos números na tela utilizando a função `Put_Line` e `Put` da biblioteca Ada.Text_IO.

Espero que este código atenda às suas expectativas e demonstre a complexidade e o potencial da linguagem ADA.