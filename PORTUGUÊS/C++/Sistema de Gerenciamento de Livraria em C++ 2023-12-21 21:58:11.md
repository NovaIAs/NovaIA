Claro! Aqui está um código complexo em C++ que envolve múltiplos conceitos e padrões de programação. Ele é um exemplo de um sistema de gerenciamento de uma livraria, com funcionalidades como adicionar livros, listar livros, pesquisar livros por título e remover livros. 

```cpp
#include <iostream>
#include <vector>
#include <algorithm>

class Livro {
private:
    std::string titulo;
    std::string autor;
    int anoPublicacao;
public:
    Livro(std::string titulo, std::string autor, int anoPublicacao) {
        this->titulo = titulo;
        this->autor = autor;
        this->anoPublicacao = anoPublicacao;
    }

    std::string getTitulo() {
        return titulo;
    }

    std::string getAutor() {
        return autor;
    }

    int getAnoPublicacao() {
        return anoPublicacao;
    }
};

class Livraria {
private:
    std::vector<Livro> livros;
public:
    void adicionarLivro(Livro livro) {
        livros.push_back(livro);
    }

    void listarLivros() {
        std::cout << "Livros na livraria:\n";
        for (const auto& livro : livros) {
            std::cout << "Título: " << livro.getTitulo() << "\n";
            std::cout << "Autor: " << livro.getAutor() << "\n";
            std::cout << "Ano de Publicação: " << livro.getAnoPublicacao() << "\n";
            std::cout << "-------------------------\n";
        }
    }

    void pesquisarLivroPorTitulo(std::string titulo) {
        auto it = std::find_if(livros.begin(), livros.end(), [titulo](const Livro& livro) {
            return livro.getTitulo() == titulo;
        });

        if (it != livros.end()) {
            std::cout << "Livro encontrado:\n";
            std::cout << "Título: " << it->getTitulo() << "\n";
            std::cout << "Autor: " << it->getAutor() << "\n";
            std::cout << "Ano de Publicação: " << it->getAnoPublicacao() << "\n";
        } else {
            std::cout << "Livro não encontrado.\n";
        }
    }

    void removerLivro(std::string titulo) {
        livros.erase(std::remove_if(livros.begin(), livros.end(), [titulo](const Livro& livro) {
            return livro.getTitulo() == titulo;
        }), livros.end());
    }
};

int main() {
    Livraria livraria;

    Livro livro1("Dom Casmurro", "Machado de Assis", 1899);
    Livro livro2("1984", "George Orwell", 1949);
    Livro livro3("Harry Potter e a Pedra Filosofal", "J.K. Rowling", 1997);

    livraria.adicionarLivro(livro1);
    livraria.adicionarLivro(livro2);
    livraria.adicionarLivro(livro3);

    livraria.listarLivros();

    std::cout << "\nPesquisar livro por título:\n";
    std::string titulo;
    std::getline(std::cin, titulo);
    livraria.pesquisarLivroPorTitulo(titulo);

    std::cout << "\nRemover livro por título:\n";
    std::getline(std::cin, titulo);
    livraria.removerLivro(titulo);

    livraria.listarLivros();

    return 0;
}
```

Neste código, utilizamos classes para representar os Livros e a Livraria. A classe Livro possui atributos como título, autor e ano de publicação, além de um construtor para inicializar esses atributos. A classe Livraria tem um vetor de Livros e métodos para adicionar, listar, pesquisar e remover livros. 

No método `main`, criamos instâncias de Livro e as adicionamos à Livraria usando o método `adicionarLivro()`. Em seguida, chamamos o método `listarLivros()` para exibir todos os livros na livraria.

Após isso, o programa pede ao usuário para inserir um título de livro para pesquisar usando o método `pesquisarLivroPorTitulo()`. Se o livro for encontrado, suas informações são exibidas. Caso contrário, uma mensagem informando que o livro não foi encontrado é mostrada.

Por fim, o programa pede ao usuário para inserir um título de livro para remover usando o método `removerLivro()`, e então chama `listarLivros()` novamente para mostrar a lista atualizada de livros na livraria.

Espero que esse código complexo e diferenciado ajude você a entender melhor o uso de classes, vetores, algoritmos de busca e manipulação de objetos em C++.