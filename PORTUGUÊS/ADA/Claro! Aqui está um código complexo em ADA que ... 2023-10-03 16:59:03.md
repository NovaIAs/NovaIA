Claro! Aqui está um código complexo em ADA que envolve a criação de um sistema de gerenciamento de biblioteca. Ele permite aos usuários adicionar e pesquisar livros, além de exibir informações sobre os mesmos. Vou explicar o código para você.

```
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Gerenciador_Biblioteca is

   -- Definição do tipo de registro para armazenar informações sobre os livros
   type Livro is record
      Titulo : String(1..100);
      Autor : String(1..50);
      Ano : Integer;
   end record;

   -- Definição de um vetor para armazenar os livros cadastrados
   type Catalogo is array(1..100) of Livro;

   -- Variável para controlar o número de livros cadastrados
   Livros_Cadastrados : Integer := 0;

   -- Procedimento para adicionar um livro ao catálogo
   procedure Adicionar_Livro(Titulo : in String; Autor : in String; Ano : in Integer) is
      Livro_Novo : Livro;
   begin
      -- Verifica se o catálogo já está cheio
      if Livros_Cadastrados = 100 then
         Put_Line("O catálogo está cheio. Não é possível adicionar mais livros.");
      else
         -- Incrementa o contador de livros cadastrados
         Livros_Cadastrados := Livros_Cadastrados + 1;
         
         -- Preenche as informações do novo livro
         Livro_Novo.Titulo := Titulo;
         Livro_Novo.Autor := Autor;
         Livro_Novo.Ano := Ano;
         
         -- Adiciona o livro ao catálogo
         Catalogo(Livros_Cadastrados) := Livro_Novo;
         
         Put_Line("Livro cadastrado com sucesso!");
      end if;
   end Adicionar_Livro;

   -- Procedimento para pesquisar um livro pelo título
   procedure Pesquisar_Livro(Titulo : in String) is
      Encontrou : Boolean := False;
   begin
      -- Percorre o catálogo em busca do livro desejado
      for i in 1..Livros_Cadastrados loop
         if Catalogo(i).Titulo = Titulo then
            -- Exibe as informações do livro encontrado
            Put_Line("Título: " & Catalogo(i).Titulo);
            Put_Line("Autor: " & Catalogo(i).Autor);
            Put_Line("Ano: " & Integer'Image(Catalogo(i).Ano));
            Encontrou := True;
            exit; -- Sai do loop ao encontrar o livro
         end if;
      end loop;
      
      -- Caso não encontre o livro, exibe uma mensagem de erro
      if not Encontrou then
         Put_Line("Livro não encontrado.");
      end if;
   end Pesquisar_Livro;

begin
   -- Exemplo de uso das funcionalidades do sistema de gerenciamento de biblioteca
   
   -- Adiciona alguns livros ao catálogo
   Adicionar_Livro("Dom Quixote", "Miguel de Cervantes", 1605);
   Adicionar_Livro("Guerra e Paz", "Liev Tolstói", 1869);
   Adicionar_Livro("1984", "George Orwell", 1949);
   
   -- Pesquisa um livro pelo título
   Pesquisar_Livro("Guerra e Paz");
   
   -- Pesquisa um livro que não está cadastrado
   Pesquisar_Livro("A Origem das Espécies");
   
end Gerenciador_Biblioteca;
```

Neste código, utilizamos a linguagem ADA para criar um sistema de gerenciamento de biblioteca. Primeiro, definimos um tipo de registro chamado "Livro" para armazenar informações sobre os livros, como título, autor e ano. Também definimos um vetor chamado "Catalogo" para armazenar os livros cadastrados.

Em seguida, criamos o procedimento "Adicionar_Livro" que permite adicionar um novo livro ao catálogo. Verificamos se o catálogo já está cheio e, se não estiver, incrementamos o contador de livros cadastrados e preenchemos as informações do novo livro. Depois, adicionamos o livro ao catálogo e exibimos uma mensagem de sucesso.

Também criamos o procedimento "Pesquisar_Livro" que permite pesquisar um livro pelo título. Percorremos o catálogo em busca do livro desejado e, caso seja encontrado, exibimos as informações do livro. Caso contrário, exibimos uma mensagem de erro.

Por fim, exemplificamos o uso das funcionalidades do sistema de gerenciamento de biblioteca. Adicionamos alguns livros ao catálogo e realizamos uma pesquisa por título.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, é só me perguntar.