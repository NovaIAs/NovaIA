Claro, vou criar um código complexo em ADA para você. O código a seguir é um exemplo de um sistema de gerenciamento de uma biblioteca, onde é possível cadastrar livros, consultar livros disponíveis, emprestar livros e devolvê-los. Vou explicar cada parte do código para facilitar o entendimento:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Biblioteca is
   type Livro is record
      Titulo : String(1..50);
      Autor : String(1..30);
      Ano : Integer;
      Disponivel : Boolean;
   end record;

   type Biblioteca_Tipo is array (1..100) of Livro;

   procedure Cadastrar_Livro (Livros : in out Biblioteca_Tipo; Indice : in Integer) is
   begin
      Put_Line("Digite o título do livro: ");
      Get_Line(Livros(Indice).Titulo);

      Put_Line("Digite o nome do autor do livro: ");
      Get_Line(Livros(Indice).Autor);

      Put_Line("Digite o ano de lançamento do livro: ");
      Get(Livros(Indice).Ano);
      
      Livros(Indice).Disponivel := True;
   end Cadastrar_Livro;

   procedure Consultar_Livro (Livros : in Biblioteca_Tipo; Indice : in Integer) is
   begin
      Put_Line("Título: " & Livros(Indice).Titulo);
      Put_Line("Autor: " & Livros(Indice).Autor);
      Put_Line("Ano: " & Livros(Indice).Ano'Img);
      if Livros(Indice).Disponivel then
         Put_Line("Disponível: Sim");
      else
         Put_Line("Disponível: Não");
      end if;
   end Consultar_Livro;

   procedure Emprestar_Livro (Livros : in out Biblioteca_Tipo; Indice : in Integer) is
   begin
      if Livros(Indice).Disponivel then
         Livros(Indice).Disponivel := False;
         Put_Line("Livro emprestado com sucesso!");
      else
         Put_Line("Livro indisponível para empréstimo.");
      end if;
   end Emprestar_Livro;

   procedure Devolver_Livro (Livros : in out Biblioteca_Tipo; Indice : in Integer) is
   begin
      if not Livros(Indice).Disponivel then
         Livros(Indice).Disponivel := True;
         Put_Line("Livro devolvido com sucesso!");
      else
         Put_Line("Este livro já está disponível.");
      end if;
   end Devolver_Livro;

   procedure Menu_Biblioteca (Livros : in out Biblioteca_Tipo) is
      Opcao : Integer := 0;
      Indice : Integer := 1;
   begin
      while Opcao /= 5 loop
         Put_Line("");
         Put_Line("----- MENU BIBLIOTECA -----");
         Put_Line("1 - Cadastrar livro");
         Put_Line("2 - Consultar livro");
         Put_Line("3 - Emprestar livro");
         Put_Line("4 - Devolver livro");
         Put_Line("5 - Sair");
         Put_Line("---------------------------");
         Put_Line("Digite a opção desejada: ");
         Get(Opcao);
         Put_Line("");

         case Opcao is
            when 1 =>
               Cadastrar_Livro(Livros, Indice);
               Indice := Indice + 1;
            when 2 =>
               if Indice > 1 then
                  Put_Line("Digite o índice do livro a ser consultado: ");
                  Get(Indice);
                  if Indice in Livros'Range then
                     Consultar_Livro(Livros, Indice);
                  else
                     Put_Line("Índice inválido.");
                  end if;
               else
                  Put_Line("Nenhum livro cadastrado.");
               end if;
            when 3 =>
               if Indice > 1 then
                  Put_Line("Digite o índice do livro a ser emprestado: ");
                  Get(Indice);
                  if Indice in Livros'Range then
                     Emprestar_Livro(Livros, Indice);
                  else
                     Put_Line("Índice inválido.");
                  end if;
               else
                  Put_Line("Nenhum livro cadastrado.");
               end if;
            when 4 =>
               if Indice > 1 then
                  Put_Line("Digite o índice do livro a ser devolvido: ");
                  Get(Indice);
                  if Indice in Livros'Range then
                     Devolver_Livro(Livros, Indice);
                  else
                     Put_Line("Índice inválido.");
                  end if;
               else
                  Put_Line("Nenhum livro cadastrado.");
               end if;
            when 5 =>
               Put_Line("Encerrando o programa...");
         end case;
      end loop;
   end Menu_Biblioteca;

   Livros : Biblioteca_Tipo;
begin
   Menu_Biblioteca(Livros);
end Biblioteca;
```

Explicação:
1. Na declaração do programa, definimos um tipo de dado `Livro`, que possui os campos `Titulo`, `Autor`, `Ano` e `Disponivel`.
2. Também definimos um tipo de dado `Biblioteca_Tipo`, que é um array de `Livro` com tamanho máximo de 100.
3. Em seguida, temos as sub-rotinas `Cadastrar_Livro`, `Consultar_Livro`, `Emprestar_Livro` e `Devolver_Livro`, responsáveis por cadastrar um livro, consultar informações de um livro, emprestar um livro e devolver um livro, respectivamente.
4. A sub-rotina `Menu_Biblioteca` é a principal do programa, que exibe um menu com opções para o usuário interagir com a biblioteca, como cadastrar, consultar, emprestar, devolver ou sair.
5. Dentro do loop do menu, cada opção selecionada chama a sub-rotina correspondente.
6. Por fim, temos a declaração do array `Livros` e a chamada para a função `Menu_Biblioteca`, que inicia a execução do programa.

Esse código em ADA implementa um sistema básico de gerenciamento de biblioteca, permitindo ao usuário cadastrar livros, consultar informações sobre livros, emprestar livros e devolvê-los.