Claro! Aqui está um exemplo de um código complexo em C++ que visa criar um sistema de gerenciamento de biblioteca:

```cpp
#include <iostream>
#include <string>
#include <vector>

using namespace std;

// Classe que representa um livro
class Livro {
private:
    string titulo;
    string autor;
    int anoPublicacao;

public:
    Livro(string titulo, string autor, int anoPublicacao) {
        this->titulo = titulo;
        this->autor = autor;
        this->anoPublicacao = anoPublicacao;
    }

    string getTitulo() {
        return titulo;
    }

    string getAutor() {
        return autor;
    }

    int getAnoPublicacao() {
        return anoPublicacao;
    }
};

// Classe que representa a biblioteca
class Biblioteca {
private:
    vector<Livro> acervo;

public:
    void adicionarLivro(Livro livro) {
        acervo.push_back(livro);
    }

    void removerLivro(string titulo) {
        for (auto it = acervo.begin(); it != acervo.end(); ++it) {
            if (it->getTitulo() == titulo) {
                acervo.erase(it);
                break;
            }
        }
    }

    void listarLivros() {
        cout << "--- Lista de Livros ---" << endl;
        for (auto livro : acervo) {
            cout << "Título: " << livro.getTitulo() << endl;
            cout << "Autor: " << livro.getAutor() << endl;
            cout << "Ano de Publicação: " << livro.getAnoPublicacao() << endl;
            cout << "----------------------" << endl;
        }
    }
};

int main() {
    Biblioteca biblioteca;

    Livro livro1("Dom Casmurro", "Machado de Assis", 1899);
    Livro livro2("Grande Sertão: Veredas", "João Guimarães Rosa", 1956);
    Livro livro3("Memórias Póstumas de Brás Cubas", "Machado de Assis", 1881);

    biblioteca.adicionarLivro(livro1);
    biblioteca.adicionarLivro(livro2);
    biblioteca.adicionarLivro(livro3);

    biblioteca.listarLivros();

    biblioteca.removerLivro("Dom Casmurro");

    biblioteca.listarLivros();

    return 0;
}
```

Neste código, criamos duas classes: `Livro`, que representa um livro com título, autor e ano de publicação; e `Biblioteca`, que representa a biblioteca, com um vetor de livros e métodos para adicionar, remover e listar os livros.

Na função `main()`, criamos três instâncias da classe `Livro` e as adicionamos à biblioteca usando o método `adicionarLivro()`. Em seguida, chamamos o método `listarLivros()` para exibir todos os livros do acervo.

Depois, removemos o livro "Dom Casmurro" usando o método `removerLivro()` e chamamos novamente o método `listarLivros()` para ver a lista atualizada.

Espero que este exemplo atenda às suas expectativas! Caso precise de mais informações ou outros tipos de códigos, estou à disposição.