Claro! Aqui está um código complexo em ADA que envolve manipulação de arquivos e estruturas de dados:

```
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Text_IO.Integer_IO; use Ada.Text_IO.Integer_IO;

procedure ManipulacaoArquivos is

   -- Definição do tipo Registro
   type Registro is record
      Nome: Unbounded_String;
      Idade: Integer;
      Salario: Float;
   end record;

   -- Definição do tipo VetorRegistros
   type VetorRegistros is array (Positive range <>) of Registro;

   -- Declaração de variáveis
   Registros: VetorRegistros (1..100);
   NumRegistros: Natural := 0;
   Arquivo: File_Type;

begin

   -- Abertura do arquivo para leitura
   Open (Arquivo, In_File, "dados.txt");

   -- Leitura dos registros do arquivo
   while not End_Of_File (Arquivo) loop
      NumRegistros := NumRegistros + 1;

      -- Leitura de cada campo do registro
      Get_Line (Arquivo, Registros(NumRegistros).Nome);
      Get (Arquivo, Registros(NumRegistros).Idade);
      Get (Arquivo, Registros(NumRegistros).Salario);

      Skip_Line (Arquivo);
   end loop;

   -- Fechamento do arquivo
   Close (Arquivo);

   -- Impressão dos registros na tela
   for I in 1..NumRegistros loop
      Put_line("Registro " & Integer'Image(I));
      Put("Nome: ");
      Put_line(To_String(Registros(I).Nome));
      Put("Idade: ");
      Put(Registros(I).Idade);
      New_Line;
      Put("Salario: ");
      Put(Registros(I).Salario, Fore => 8, Aft => 2);
      New_Line;
      New_Line;
   end loop;

   -- Ordenação dos registros pelo campo Salario
   for I in 1..NumRegistros-1 loop
      for J in I+1..NumRegistros loop
         if Registros(I).Salario > Registros(J).Salario then

            -- Troca dos registros de posição
            declare
               Temp: Registro;
            begin
               Temp := Registros(I);
               Registros(I) := Registros(J);
               Registros(J) := Temp;
            end;

         end if;
      end loop;
   end loop;

   -- Impressão dos registros ordenados na tela
   Put_line("Registros Ordenados por Salario:");
   for I in 1..NumRegistros loop
      Put_line("Registro " & Integer'Image(I));
      Put("Nome: ");
      Put_line(To_String(Registros(I).Nome));
      Put("Idade: ");
      Put(Registros(I).Idade);
      New_Line;
      Put("Salario: ");
      Put(Registros(I).Salario, Fore => 8, Aft => 2);
      New_Line;
      New_Line;
   end loop;

   -- Abertura do arquivo para escrita
   Open (Arquivo, Out_File, "dados_ordenados.txt");

   -- Escrita dos registros ordenados no arquivo
   for I in 1..NumRegistros loop
      Put_line(Arquivo, To_String(Registros(I).Nome));
      Put(Arquivo, Registros(I).Idade);
      Put(Arquivo, Registros(I).Salario, Fore => 8, Aft => 2);
      New_Line(Arquivo);
   end loop;

   -- Fechamento do arquivo
   Close (Arquivo);

end ManipulacaoArquivos;
```

Neste código em ADA, estamos realizando a manipulação de um arquivo chamado "dados.txt" que contém registros com informações de nome, idade e salário. O objetivo é ler esses registros, armazená-los em um vetor de registros e, em seguida, ordená-los pelo campo de salário e escrevê-los em um novo arquivo chamado "dados_ordenados.txt".

O código começa definindo o tipo `Registro`, que possui os campos `Nome` (do tipo `Unbounded_String`), `Idade` (do tipo Integer) e `Salario` (do tipo Float). Em seguida, é definido o tipo `VetorRegistros`, que é um array de registros.

Dentro do bloco principal do programa, o arquivo "dados.txt" é aberto para leitura. Em seguida, é feita a leitura dos registros do arquivo utilizando um loop `while` que percorre o arquivo até o final. A cada iteração do loop, os campos de cada registro são lidos utilizando as operações `Get_Line` e `Get`, e o contador `NumRegistros` é incrementado para contar a quantidade total de registros lidos.

Depois de ler todos os registros do arquivo, o arquivo é fechado e os registros são impressos na tela utilizando um loop `for`. Cada registro é exibido com seu respectivo número, seguido dos campos `Nome`, `Idade` e `Salario`.

Em seguida, os registros são ordenados pelo campo `Salario` utilizando dois loops `for` aninhados. A cada iteração, é comparado se o `Salario` do registro atual é maior do que o `Salario` do registro seguinte. Se for, os registros são trocados de posição.

Após a ordenação, os registros são impressos novamente na tela, agora na ordem dos salários, utilizando um loop `for`.

Finalmente, é aberto um novo arquivo chamado "dados_ordenados.txt" para escrita. Os registros ordenados são escritos no arquivo utilizando um loop `for`, e, em seguida, o arquivo é fechado.

Esse código realiza a manipulação de arquivos em ADA, além de utilizar estruturas de dados como registros e vetores.