Claro! Aqui está um exemplo de um código complexo em C++ que envolve manipulação de arquivos e estruturas de dados avançadas. Este código cria um sistema de gerenciamento de biblioteca onde os usuários podem adicionar, remover e pesquisar por livros.

```c++
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

// Definindo a estrutura de dados para representar um livro
struct Livro {
    string titulo;
    string autor;
    int anoPublicacao;
};

// Função para adicionar um livro ao sistema
void adicionarLivro(vector<Livro>& biblioteca) {
    Livro novoLivro;
    cout << "Digite o título do livro: ";
    getline(cin, novoLivro.titulo);
    cout << "Digite o autor do livro: ";
    getline(cin, novoLivro.autor);
    cout << "Digite o ano de publicação do livro: ";
    cin >> novoLivro.anoPublicacao;
    cin.ignore();

    biblioteca.push_back(novoLivro);

    cout << "Livro adicionado com sucesso!" << endl;
}

// Função para remover um livro do sistema
void removerLivro(vector<Livro>& biblioteca) {
    string tituloRemover;
    cout << "Digite o título do livro que deseja remover: ";
    getline(cin, tituloRemover);

    for (int i = 0; i < biblioteca.size(); i++) {
        if (biblioteca[i].titulo == tituloRemover) {
            biblioteca.erase(biblioteca.begin() + i);
            cout << "Livro removido com sucesso!" << endl;
            return;
        }
    }

    cout << "Livro não encontrado." << endl;
}

// Função para pesquisar por um livro no sistema
void pesquisarLivro(const vector<Livro>& biblioteca) {
    string tituloPesquisar;
    cout << "Digite o título do livro que deseja pesquisar: ";
    getline(cin, tituloPesquisar);

    for (const auto& livro : biblioteca) {
        if (livro.titulo == tituloPesquisar) {
            cout << "Título: " << livro.titulo << endl;
            cout << "Autor: " << livro.autor << endl;
            cout << "Ano de publicação: " << livro.anoPublicacao << endl;
            return;
        }
    }

    cout << "Livro não encontrado." << endl;
}

// Função para salvar a biblioteca em um arquivo de texto
void salvarBiblioteca(const vector<Livro>& biblioteca) {
    ofstream arquivo("biblioteca.txt");

    for (const auto& livro : biblioteca) {
        arquivo << "Título: " << livro.titulo << endl;
        arquivo << "Autor: " << livro.autor << endl;
        arquivo << "Ano de publicação: " << livro.anoPublicacao << endl;
        arquivo << endl;
    }

    arquivo.close();

    cout << "Biblioteca salva com sucesso!" << endl;
}

// Função principal
int main() {
    vector<Livro> biblioteca;

    // Loop principal do programa
    while (true) {
        cout << "===== Sistema de Gerenciamento de Biblioteca =====" << endl;
        cout << "1. Adicionar livro" << endl;
        cout << "2. Remover livro" << endl;
        cout << "3. Pesquisar livro" << endl;
        cout << "4. Salvar biblioteca" << endl;
        cout << "0. Sair" << endl;
        cout << "Digite a opção desejada: ";

        int opcao;
        cin >> opcao;
        cin.ignore();

        switch (opcao) {
            case 1:
                adicionarLivro(biblioteca);
                break;
            case 2:
                removerLivro(biblioteca);
                break;
            case 3:
                pesquisarLivro(biblioteca);
                break;
            case 4:
                salvarBiblioteca(biblioteca);
                break;
            case 0:
                cout << "Encerrando o programa..." << endl;
                return 0;
            default:
                cout << "Opção inválida." << endl;
        }

        cout << endl;
    }
}
```

Neste código, utilizamos a linguagem C++ para criar um sistema de gerenciamento de biblioteca. O usuário pode adicionar, remover e pesquisar por livros. Os dados dos livros são armazenados em uma estrutura de dados chamada `vector` e são salvos em um arquivo de texto chamado "biblioteca.txt".

A função `adicionarLivro()` permite que o usuário insira as informações de título, autor e ano de publicação de um livro, e então o livro é adicionado à biblioteca.

A função `removerLivro()` solicita que o usuário digite o título do livro que deseja remover, e então procura por esse livro na biblioteca. Se encontrado, o livro é removido da biblioteca.

A função `pesquisarLivro()` permite que o usuário pesquise por um livro na biblioteca através do título. Se o livro for encontrado, suas informações são exibidas na tela.

A função `salvarBiblioteca()` é responsável por salvar a biblioteca em um arquivo de texto chamado "biblioteca.txt", onde cada livro é salvo com seu título, autor e ano de publicação.

No `main()`, temos um loop principal que exibe um menu de opções para o usuário. O usuário pode escolher uma opção digitando o número correspondente. O programa então chama a função correspondente à opção escolhida.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, sinta-se à vontade para perguntar.