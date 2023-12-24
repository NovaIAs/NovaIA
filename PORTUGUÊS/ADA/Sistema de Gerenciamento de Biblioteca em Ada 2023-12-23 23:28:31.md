Claro! Aqui está um exemplo complexo de código em Ada:

```
-- Este é um exemplo de um programa Ada complexo que implementa um sistema de gerenciamento de biblioteca.

-- Definição do pacote com as especificações das entidades do sistema
package Biblioteca is
   type Livro is record
      Titulo  : String(1..100);
      Autor   : String(1..100);
      Ano     : Integer;
      Disponivel : Boolean;
   end record;

   type Livros is array(Integer range <>) of Livro;

   procedure AdicionarLivro(L : in out Livros; T : String; A : String; An : Integer);
   procedure RemoverLivro(L : in out Livros; Posicao : Integer);
   procedure ListarLivros(L : Livros);
   procedure ReservarLivro(L : in out Livros; Posicao : Integer);
   procedure DevolverLivro(L : in out Livros; Posicao : Integer);
end Biblioteca;

-- Implementação do pacote com os corpos das entidades do sistema
package body Biblioteca is
   procedure AdicionarLivro(L : in out Livros; T : String; A : String; An : Integer) is
   begin
      L(L'Length + 1).Titulo := T;
      L(L'Length).Autor := A;
      L(L'Length).Ano := An;
      L(L'Length).Disponivel := True;
   end AdicionarLivro;

   procedure RemoverLivro(L : in out Livros; Posicao : Integer) is
   begin
      if Posicao > 0 and Posicao <= L'Length then
         for i in Posicao..L'Length-1 loop
            L(i) := L(i + 1);
         end loop;
         L := L(1..L'Length-1);
      end if;
   end RemoverLivro;

   procedure ListarLivros(L : Livros) is
   begin
      for i in L'Range loop
         Put_Line("Livro " & Integer'Image(i) & ":");
         Put_Line("   Título: " & L(i).Titulo);
         Put_Line("   Autor: " & L(i).Autor);
         Put_Line("   Ano: " & Integer'Image(L(i).Ano));
         if L(i).Disponivel then
            Put_Line("   Disponível: Sim");
         else
            Put_Line("   Disponível: Não");
         end if;
      end loop;
   end ListarLivros;

   procedure ReservarLivro(L : in out Livros; Posicao : Integer) is
   begin
      if Posicao > 0 and Posicao <= L'Length then
         L(Posicao).Disponivel := False;
      end if;
   end ReservarLivro;

   procedure DevolverLivro(L : in out Livros; Posicao : Integer) is
   begin
      if Posicao > 0 and Posicao <= L'Length then
         L(Posicao).Disponivel := True;
      end if;
   end DevolverLivro;
end Biblioteca;

-- Programa principal que utiliza o pacote Biblioteca
with Ada.Text_IO; use Ada.Text_IO;
with Biblioteca; use Biblioteca;

procedure Main is
   LivrosDaBiblioteca : Livros(1..10);
begin
   AdicionarLivro(LivrosDaBiblioteca, "Dom Casmurro", "Machado de Assis", 1899);
   AdicionarLivro(LivrosDaBiblioteca, "Grande Sertão: Veredas", "João Guimarães Rosa", 1956);
   AdicionarLivro(LivrosDaBiblioteca, "1984", "George Orwell", 1949);
   AdicionarLivro(LivrosDaBiblioteca, "Crime e Castigo", "Fiódor Dostoiévski", 1866);

   Put_Line("Lista de Livros:");
   ListarLivros(LivrosDaBiblioteca);

   RemoverLivro(LivrosDaBiblioteca, 3);
   ReservarLivro(LivrosDaBiblioteca, 2);
   DevolverLivro(LivrosDaBiblioteca, 1);

   Put_Line("Lista de Livros Atualizada:");
   ListarLivros(LivrosDaBiblioteca);
end Main;
```

Neste exemplo, é implementado um sistema de gerenciamento de biblioteca em Ada. O pacote `Biblioteca` define as entidades do sistema, como o tipo `Livro` com os atributos "Titulo", "Autor", "Ano" e "Disponivel". Além disso, o pacote define procedimentos para adicionar, remover, listar, reservar e devolver livros.

O pacote `Biblioteca` é implementado no pacote body `Biblioteca`, onde os procedimentos são definidos. O procedimento `AdicionarLivro` adiciona um novo livro ao array de livros, o procedimento `RemoverLivro` remove um livro do array, o procedimento `ListarLivros` exibe informações sobre todos os livros no array, o procedimento `ReservarLivro` marca um livro como indisponível e o procedimento `DevolverLivro` marca um livro como disponível.

O programa principal `Main` utiliza o pacote `Biblioteca` e demonstra o uso dos procedimentos. Livros são adicionados, a lista de livros é exibida, um livro é removido, um livro é reservado e um livro é devolvido. No final, a lista de livros atualizada é exibida.

Espero que isso ajude a entender como implementar um programa complexo em Ada!