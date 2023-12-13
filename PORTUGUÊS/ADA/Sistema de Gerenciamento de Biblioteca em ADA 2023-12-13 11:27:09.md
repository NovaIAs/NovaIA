Claro! Aqui está um exemplo de um código complexo em ADA:

```ada
-- Este é um programa em ADA que simula um sistema de gerenciamento de biblioteca

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Float_IO; use Ada.Text_IO.Float_IO;

procedure Gerenciador_Biblioteca is

   -- Definição do registro Livro
   type Livro is record
      Titulo        : Unbounded_String;
      Autor         : Unbounded_String;
      Ano_Publicado : Integer;
      Preco         : Float;
   end record;

   -- Criação de um array de Livros
   type Array_Livros is array (Integer range <>) of Livro;

   -- Variáveis
   Livros : Array_Livros (1..10);
   Quantidade_Livros : Integer := 0;

   -- Procedimento para adicionar um livro ao sistema
   procedure Adicionar_Livro (Titulo : Unbounded_String; Autor : Unbounded_String; Ano_Publicado : Integer; Preco : Float) is
   begin
      if Quantidade_Livros >= 10 then
         Put_Line ("Limite máximo de livros atingido");
      else
         Quantidade_Livros := Quantidade_Livros + 1;
         Livros(Quantidade_Livros).Titulo := Titulo;
         Livros(Quantidade_Livros).Autor := Autor;
         Livros(Quantidade_Livros).Ano_Publicado := Ano_Publicado;
         Livros(Quantidade_Livros).Preco := Preco;
         Put_Line ("Livro adicionado com sucesso");
      end if;
   end Adicionar_Livro;

   -- Função para listar todos os livros do sistema
   function Listar_Livros return Unbounded_String is
      Lista : Unbounded_String := "";
   begin
      for I in 1..Quantidade_Livros loop
         Lista := Lista & "Título: " & Livros(I).Titulo & ", Autor: " & Livros(I).Autor & ", Ano de Publicação: " & Integer'Image(Livros(I).Ano_Publicado) & ", Preço: ";
         Put(Float'Image(Livros(I).Preco), List => Lista);
         Lista := Lista & " R$" & ASCII.LF; -- Quebra de linha
      end loop;
      return Lista;
   end Listar_Livros;

   -- Procedimento principal
   procedure Main is
      Opcao : Integer;
      Titulo, Autor : Unbounded_String;
      Ano_Publicado : Integer;
      Preco : Float;
      Lista_Livros : Unbounded_String;
   begin
      loop
         Put_Line ("---------------------");
         Put_Line ("1 - Adicionar Livro");
         Put_Line ("2 - Listar Livros");
         Put_Line ("3 - Sair");
         Put_Line ("---------------------");
         Put("Digite a opção desejada: ");
         Get(Opcao);
         case Opcao is
            when 1 =>
               Put("Digite o título do livro: ");
               Get_Line(Titulo);
               Put("Digite o autor do livro: ");
               Get_Line(Autor);
               Put("Digite o ano de publicação do livro: ");
               Get(Ano_Publicado);
               Put("Digite o preço do livro: ");
               Get(Preco);
               Adicionar_Livro(Titulo, Autor, Ano_Publicado, Preco);
            when 2 =>
               Lista_Livros := Listar_Livros;
               Put_Line(Lista_Livros);
            when 3 =>
               Put_Line("Saindo do programa...");
               exit;
            when others =>
               Put_Line("Opção inválida");
         end case;
      end loop;
   end Main;

begin
   Main;
end Gerenciador_Biblioteca;
```

Neste código em ADA, temos um simulador de sistema de gerenciamento de biblioteca. Ele utiliza registros (registros são estruturas de dados que podem armazenar diferentes tipos de informações em uma única variável) para representar os livros, e um array de registros para armazenar os livros adicionados ao sistema. 

O programa principal possui um loop que exibe um menu de opções ao usuário, permitindo que ele adicione livros ao sistema, liste todos os livros cadastrados ou saia do programa. 

Ao adicionar um livro, o usuário é solicitado a digitar o título, autor, ano de publicação e preço do livro. Essas informações são então armazenadas em um novo registro e adicionadas ao array de livros, desde que ainda haja espaço disponível. Caso contrário, é exibida uma mensagem informando que o limite máximo de livros foi atingido.

Ao listar os livros, o programa percorre o array de livros e gera uma string contendo todas as informações dos livros cadastrados, separadas por vírgulas. Essa string é retornada e exibida na tela.

O programa continua em execução até que o usuário escolha a opção de sair.

Espero que este exemplo atenda às suas expectativas!