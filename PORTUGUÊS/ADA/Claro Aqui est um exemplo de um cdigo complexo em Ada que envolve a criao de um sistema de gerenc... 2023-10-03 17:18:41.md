Claro! Aqui está um exemplo de um código complexo em Ada, que envolve a criação de um sistema de gerenciamento de biblioteca. O código está dividido em módulos para facilitar a organização e explicação.

Módulo 1: biblioteca.ads

```ada
package Biblioteca is
    type Livro is record
        Titulo : String(1..100);
        Autor : String(1..100);
        Ano : Integer;
    end record;
    
    type Livros is array(Integer range <>) of Livro;

    procedure AdicionarLivro(Livro : in Livro; ListaLivros : in out Livros);
    procedure RemoverLivro(Titulo : in String; ListaLivros : in out Livros);
    procedure ListarLivros(ListaLivros : in Livros);
    
    function BuscarLivro(Titulo : in String; ListaLivros : in Livros) return Livro;
end Biblioteca;
```

Neste módulo, definimos o pacote `Biblioteca` que contém as definições de tipos e procedimentos relacionados à biblioteca. O tipo `Livro` representa a estrutura de informações de um livro, contendo título, autor e ano. O tipo `Livros` é um array dinâmico de livros.

As procedimentos `AdicionarLivro`, `RemoverLivro` e `ListarLivros` são responsáveis por adicionar, remover e listar os livros na biblioteca, respectivamente. A função `BuscarLivro` busca um livro pelo título na biblioteca.

Módulo 2: biblioteca.adb

```ada
with Ada.Text_IO;
use Ada.Text_IO;

package body Biblioteca is
    LivrosBiblioteca : Livros := (1 => (Titulo => "Livro 1", Autor => "Autor 1", Ano => 2000),
                                  2 => (Titulo => "Livro 2", Autor => "Autor 2", Ano => 2005),
                                  3 => (Titulo => "Livro 3", Autor => "Autor 3", Ano => 2010));
    
    procedure AdicionarLivro(Livro : in Livro; ListaLivros : in out Livros) is
    begin
        ListaLivros(ListaLivros'Last + 1) := Livro;
        Put_Line("Livro adicionado com sucesso!");
    end AdicionarLivro;
    
    procedure RemoverLivro(Titulo : in String; ListaLivros : in out Livros) is
        LivroEncontrado : Boolean := False;
    begin
        for I in ListaLivros'Range loop
            if ListaLivros(I).Titulo = Titulo then
                ListaLivros(I) := ListaLivros(ListaLivros'Last);
                ListaLivros := ListaLivros(1..ListaLivros'Last - 1);
                LivroEncontrado := True;
                Put_Line("Livro removido com sucesso!");
                exit;
            end if;
        end loop;
        
        if not LivroEncontrado then
            Put_Line("Livro não encontrado!");
        end if;
    end RemoverLivro;
    
    procedure ListarLivros(ListaLivros : in Livros) is
    begin
        Put_Line("Lista de Livros:");
        for I in ListaLivros'Range loop
            Put_Line("Título: " & ListaLivros(I).Titulo);
            Put_Line("Autor: " & ListaLivros(I).Autor);
            Put_Line("Ano: " & Integer'Image(ListaLivros(I).Ano));
            Put_Line("---------------------------");
        end loop;
    end ListarLivros;
    
    function BuscarLivro(Titulo : in String; ListaLivros : in Livros) return Livro is
        LivroEncontrado : Boolean := False;
        LivroEncontradoTemp : Livro;
    begin
        for I in ListaLivros'Range loop
            if ListaLivros(I).Titulo = Titulo then
                LivroEncontradoTemp := ListaLivros(I);
                LivroEncontrado := True;
                exit;
            end if;
        end loop;
        
        if LivroEncontrado then
            return LivroEncontradoTemp;
        else
            raise Program_Error;
        end if;
    end BuscarLivro;
end Biblioteca;
```

Neste módulo, implementamos as funcionalidades do pacote `Biblioteca`. Inicialmente, definimos uma variável `LivrosBiblioteca` que armazena alguns livros como exemplo.

As procedimentos `AdicionarLivro`, `RemoverLivro` e `ListarLivros` são implementadas de acordo com suas respectivas especificações. A função `BuscarLivro` realiza uma busca pelo título na lista de livros e retorna o livro correspondente, caso seja encontrado.

Com esse código, é possível criar um sistema de gerenciamento de biblioteca em Ada. É importante ressaltar que essa é apenas uma implementação simplificada e que pode ser aprimorada com muitas outras funcionalidades.