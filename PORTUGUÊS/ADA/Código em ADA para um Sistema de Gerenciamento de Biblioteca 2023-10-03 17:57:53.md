Claro! Aqui está um exemplo de código complexo em ADA que envolve o uso de tipos abstratos de dados, manipulação de strings e estruturas de controle. Este código é um sistema de gerenciamento de biblioteca, que permite adicionar, remover e pesquisar livros:

```
-------------------------------------------------
-- Definição dos tipos de dados abstratos
-------------------------------------------------

-- Tipo de dado abstrato para representar um livro
type Livro is record
    Titulo: String(1..100);
    Autor: String(1..50);
    AnoPublicacao: Integer range 0..9999;
end record;

-- Tipo de dado abstrato para representar uma biblioteca
type Biblioteca is record
    Livros: array(1..100) of Livro;
    QuantidadeLivros: Integer range 0..100;
end record;

-------------------------------------------------
-- Procedimentos para manipulação dos livros
-------------------------------------------------

procedure AdicionarLivro(B: in out Biblioteca; T: in String; A: in String; AP: in Integer) is
begin
    if B.QuantidadeLivros < 100 then
        B.QuantidadeLivros := B.QuantidadeLivros + 1;
        B.Livros(B.QuantidadeLivros).Titulo := T;
        B.Livros(B.QuantidadeLivros).Autor := A;
        B.Livros(B.QuantidadeLivros).AnoPublicacao := AP;
    else
        raise Program_Error;
    end if;
end AdicionarLivro;

procedure RemoverLivro(B: in out Biblioteca; T: in String) is
    LivroEncontrado: Boolean := False;
    i: Integer;
begin
    for i in 1..B.QuantidadeLivros loop
        if B.Livros(i).Titulo = T then
            LivroEncontrado := True;
            exit;
        end if;
    end loop;
    
    if LivroEncontrado then
        for j in i..B.QuantidadeLivros-1 loop
            B.Livros(j) := B.Livros(j+1);
        end loop;
        
        B.QuantidadeLivros := B.QuantidadeLivros - 1;
    else
        raise Program_Error;
    end if;
end RemoverLivro;

procedure PesquisarLivro(B: in Biblioteca; T: in String) is
    LivroEncontrado: Boolean := False;
    i: Integer;
begin
    for i in 1..B.QuantidadeLivros loop
        if B.Livros(i).Titulo = T then
            LivroEncontrado := True;
            exit;
        end if;
    end loop;
    
    if LivroEncontrado then
        Put_Line("Livro encontrado!");
        Put_Line("Título: " & B.Livros(i).Titulo);
        Put_Line("Autor: " & B.Livros(i).Autor);
        Put_Line("Ano de Publicação: " & B.Livros(i).AnoPublicacao'Image);
    else
        Put_Line("Livro não encontrado!");
    end if;
end PesquisarLivro;

-------------------------------------------------
-- Programa principal
-------------------------------------------------

BibliotecaPrincipal: Biblioteca;

begin
    -- Adicionando livros na biblioteca
    AdicionarLivro(BibliotecaPrincipal, "Dom Quixote", "Miguel de Cervantes", 1605);
    AdicionarLivro(BibliotecaPrincipal, "Grande Sertão: Veredas", "João Guimarães Rosa", 1956);
    AdicionarLivro(BibliotecaPrincipal, "1984", "George Orwell", 1949);
    
    -- Pesquisando um livro
    PesquisarLivro(BibliotecaPrincipal, "Grande Sertão: Veredas");
    
    -- Removendo um livro
    RemoverLivro(BibliotecaPrincipal, "1984");
    
    -- Pesquisando novamente após a remoção
    PesquisarLivro(BibliotecaPrincipal, "1984");
end.
```

Neste código, temos a definição dos tipos de dados abstratos `Livro` e `Biblioteca`, que representam um livro e uma biblioteca, respectivamente. Em seguida, temos os procedimentos `AdicionarLivro`, `RemoverLivro` e `PesquisarLivro` que realizam as operações de adicionar, remover e pesquisar livros na biblioteca.

No programa principal, exemplificamos o uso dos procedimentos ao adicionar três livros na `BibliotecaPrincipal`, pesquisar um livro pelo título "Grande Sertão: Veredas" e, em seguida, remover o livro "1984". Por fim, realizamos outra pesquisa para verificar se o livro foi removido com sucesso.

Este código é apenas um exemplo de um sistema de gerenciamento de biblioteca em ADA. É importante ressaltar que pode ser necessário adaptá-lo para atender às necessidades específicas de um projeto real.