Claro! Aqui está um exemplo de código em ADA:

```ada
-- Este é um exemplo de código em ADA que implementa um sistema de gerenciamento de biblioteca.

with Ada.Text_IO; -- Biblioteca para entrada/saída de texto
use Ada.Text_IO;

with Ada.Integer_Text_IO; -- Biblioteca para entrada/saída de inteiros
use Ada.Integer_Text_IO;

procedure Gerenciamento_Biblioteca is
   -- Definição do tipo de registro para representar um livro
   type Livro is record
      Titulo   : String(1..50);  -- Título do livro
      Autor    : String(1..50);  -- Autor do livro
      Ano      : Integer;        -- Ano de publicação do livro
      Disponivel : Boolean;      -- Indica se o livro está disponível para empréstimo ou não
   end record;

   -- Definição do tipo de vetor para armazenar os livros
   type Biblioteca is array(1..100) of Livro;

   -- Variável para controlar o número de livros na biblioteca
   Livros_Cadastrados : Integer := 0;

   -- Função para cadastrar um novo livro na biblioteca
   procedure Cadastrar_Livro is
      Novo_Livro : Livro;
   begin
      Put_Line("----- Cadastro de Livro -----");
      
      Put("Título: ");
      Get_Line(Novo_Livro.Titulo);
      
      Put("Autor: ");
      Get_Line(Novo_Livro.Autor);
      
      Put("Ano de publicação: ");
      Get(Novo_Livro.Ano);
      
      Novo_Livro.Disponivel := True;
      
      Livros_Cadastrados := Livros_Cadastrados + 1;
      
      Put_Line("Livro cadastrado com sucesso!");
   end Cadastrar_Livro;

   -- Função para listar todos os livros disponíveis na biblioteca
   procedure Listar_Livros_Disponiveis is
   begin
      Put_Line("----- Livros Disponíveis -----");
      
      if Livros_Cadastrados = 0 then
         Put_Line("Nenhum livro cadastrado na biblioteca.");
      else
         for I in 1..Livros_Cadastrados loop
            if Biblioteca(I).Disponivel then
               Put("Título: ");
               Put_Line(Biblioteca(I).Titulo);
               
               Put("Autor: ");
               Put_Line(Biblioteca(I).Autor);
               
               Put("Ano de publicação: ");
               Put(Biblioteca(I).Ano, Width => 4);
               New_Line;
            end if;
         end loop;
      end if;
   end Listar_Livros_Disponiveis;

   -- Função para emprestar um livro da biblioteca
   procedure Emprestar_Livro is
      Titulo_Livro : String(1..50);
      Encontrado    : Boolean := False;
   begin
      Put_Line("----- Empréstimo de Livro -----");
      
      Put("Título do livro: ");
      Get_Line(Titulo_Livro);
      
      for I in 1..Livros_Cadastrados loop
         if Biblioteca(I).Titulo = Titulo_Livro and then Biblioteca(I).Disponivel then
            Biblioteca(I).Disponivel := False;
            Encontrado := True;
            Put_Line("Livro emprestado com sucesso!");
            exit;  -- Para sair do loop assim que encontrar o livro
         end if;
      end loop;
      
      if not Encontrado then
         Put_Line("Livro não encontrado ou indisponível.");
      end if;
   end Emprestar_Livro;
   
   -- Função para devolver um livro à biblioteca
   procedure Devolver_Livro is
      Titulo_Livro : String(1..50);
      Encontrado    : Boolean := False;
   begin
      Put_Line("----- Devolução de Livro -----");
      
      Put("Título do livro: ");
      Get_Line(Titulo_Livro);
      
      for I in 1..Livros_Cadastrados loop
         if Biblioteca(I).Titulo = Titulo_Livro and then not Biblioteca(I).Disponivel then
            Biblioteca(I).Disponivel := True;
            Encontrado := True;
            Put_Line("Livro devolvido com sucesso!");
            exit;  -- Para sair do loop assim que encontrar o livro
         end if;
      end loop;
      
      if not Encontrado then
         Put_Line("Livro não encontrado ou já disponível.");
      end if;
   end Devolver_Livro;

   -- Menu principal
   procedure Menu_Principal is
      Opcao : Integer;
   begin
      loop
         Put_Line("----- Menu Principal -----");
         Put_Line("1. Cadastrar livro");
         Put_Line("2. Listar livros disponíveis");
         Put_Line("3. Emprestar livro");
         Put_Line("4. Devolver livro");
         Put_Line("5. Sair");
         Put("");
         Put("Opção: ");
         Get(Opcao);
         New_Line;
         
         case Opcao is
            when 1 =>
               Cadastrar_Livro;
               New_Line;
            when 2 =>
               Listar_Livros_Disponiveis;
               New_Line;
            when 3 =>
               Emprestar_Livro;
               New_Line;
            when 4 =>
               Devolver_Livro;
               New_Line;
            when 5 =>
               exit;  -- Para sair do loop do menu
            when others =>
               Put_Line("Opção inválida. Tente novamente.");
               New_Line;
         end case;
      end loop;
   end Menu_Principal;

begin
   Menu_Principal;
end Gerenciamento_Biblioteca;
```

Este código implementa um sistema de gerenciamento de biblioteca em ADA. Ele possui um tipo de registro chamado `Livro`, que armazena informações como título, autor, ano de publicação e disponibilidade. Também utiliza um vetor chamado `Biblioteca` para armazenar os livros cadastrados.

O programa oferece um menu principal com opções para cadastrar um novo livro, listar os livros disponíveis, emprestar um livro e devolvê-lo. O número máximo de livros que podem ser cadastrados é 100.

Cada função no programa é responsável por uma tarefa específica, como cadastrar um livro, listar os livros disponíveis, emprestar um livro ou devolvê-lo. O programa utiliza a biblioteca `Ada.Text_IO` para entrada/saída de texto e `Ada.Integer_Text_IO` para entrada/saída de inteiros.

Espero que este exemplo de código em ADA atenda às suas necessidades! Se você tiver mais alguma solicitação ou alguma dúvida, fique à vontade para perguntar.