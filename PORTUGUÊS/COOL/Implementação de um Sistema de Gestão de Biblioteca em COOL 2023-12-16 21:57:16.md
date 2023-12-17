Claro! Aqui está um código em COOL (Classroom Object-Oriented Language) que implementa um sistema de gestão de biblioteca:

```
class Livro {

    atributo titulo: String;
    atributo autor: String;
    atributo anoPublicacao: Int;

    metodos {
        metodo inicializar(t: String, a: String, ap: Int) : SELF_TYPE {
            {
                self.titulo <- t;
                self.autor <- a;
                self.anoPublicacao <- ap;
                return self;
            }
        }

        metodo exibirDetalhes() : Object {
            {
                out_string("Título: ");
                out_string(self.titulo);
                out_string("\nAutor: ");
                out_string(self.autor);
                out_string("\nAno de publicação: ");
                out_int(self.anoPublicacao);
                out_string("\n");
                return void;
            }
        }
    }
}

class Biblioteca {

    atributo livros: AUTO_TYPE <- nil;

    metodos {
        metodo adicionarLivro(l: Livro) : SELF_TYPE {
            {
                if livros = nil then
                    livros <- l;
                else
                    livros.inserir(l);
                fi;
                return self;
            }
        }

        metodo removerLivro(t: String) : SELF_TYPE {
            {
                if livros = nil then
                    return self;
                else
                    livros.remover(t);
                    return self;
                fi;
            }
        }

        metodo exibirLivros() : Object {
            {
                if livros = nil then
                    out_string("Não há livros na biblioteca.\n");
                else
                    livros.exibir();
                fi;
                return void;
            }
        }
    }
}

class ListaLivros {

    atributo livro: Livro;
    atributo proximo: AUTO_TYPE <- nil;

    metodos {
        metodo inserir(l: Livro) : SELF_TYPE {
            {
                if proximo = nil then
                    proximo <- new ListaLivros;
                    proximo.livro <- l;
                else
                    proximo.inserir(l);
                fi;
                return self;
            }
        }

        metodo remover(t: String) : SELF_TYPE {
            {
                if proximo = nil then
                    return self;
                else if proximo.livro.titulo = t then
                    proximo <- proximo.proximo;
                else
                    proximo.remover(t);
                fi;
                return self;
            }
        }

        metodo exibir() : Object {
            {
                out_string("Título: ");
                out_string(self.livro.titulo);
                out_string("\nAutor: ");
                out_string(self.livro.autor);
                out_string("\nAno de publicação: ");
                out_int(self.livro.anoPublicacao);
                out_string("\n\n");
                if proximo /= nil then
                    proximo.exibir();
                fi;
                return void;
            }
        }
    }
}

metodo main() : Object {
    {
        let l1: Livro <- new Livro.inicializar("Dom Casmurro", "Machado de Assis", 1899);
        let l2: Livro <- new Livro.inicializar("Grande Sertão: Veredas", "João Guimarães Rosa", 1956);
        let l3: Livro <- new Livro.inicializar("Cem Anos de Solidão", "Gabriel García Márquez", 1967);

        let biblioteca: Biblioteca <- new Biblioteca;
        biblioteca.adicionarLivro(l1);
        biblioteca.adicionarLivro(l2);
        biblioteca.adicionarLivro(l3);

        biblioteca.exibirLivros();

        biblioteca.removerLivro("Dom Casmurro");
        biblioteca.exibirLivros();
    }
}
```

Este código em COOL implementa três classes: `Livro`, `ListaLivros` e `Biblioteca`.

A classe `Livro` representa um livro e possui três atributos: `titulo`, `autor` e `anoPublicacao`. Ela também possui os métodos `inicializar`, para atribuir valores aos atributos, e `exibirDetalhes`, para exibir as informações do livro.

A classe `ListaLivros` representa uma lista encadeada de livros. Ela possui dois atributos: `livro`, que representa um livro, e `proximo`, que representa o próximo livro na lista. Os métodos dessa classe são `inserir`, para adicionar um livro à lista, `remover`, para remover um livro da lista, e `exibir`, para exibir as informações de todos os livros da lista.

A classe `Biblioteca` representa uma biblioteca e possui um atributo `livros`, que é uma instância da classe `ListaLivros`. Os métodos dessa classe são `adicionarLivro`, para adicionar um livro à biblioteca, `removerLivro`, para remover um livro da biblioteca, e `exibirLivros`, para exibir as informações de todos os livros da biblioteca.

No método `main`, são criados três livros e uma biblioteca. Os livros são adicionados à biblioteca, que em seguida exibe todos os livros. Depois, um livro é removido da biblioteca e a lista de livros é exibida novamente.

Esse código em COOL exemplifica como implementar um sistema de gestão de biblioteca com suporte para adicionar, remover e exibir livros.