Claro! Vou criar um código complexo em C++ para você. Este código é um exemplo de um sistema de gerenciamento de biblioteca, onde você pode adicionar, remover e buscar livros. Vou explicar cada parte do código para que você possa entender melhor.

```cpp
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

// Definindo a estrutura do livro
struct Livro {
    string titulo;
    string autor;
    int ano;
};

// Função para adicionar um livro à biblioteca
void adicionarLivro(vector<Livro> &biblioteca) {
    Livro novoLivro;
    cout << "Digite o título do livro: ";
    getline(cin, novoLivro.titulo);
    cout << "Digite o autor do livro: ";
    getline(cin, novoLivro.autor);
    cout << "Digite o ano do livro: ";
    cin >> novoLivro.ano;
    cin.ignore(); // Limpa o buffer do teclado
    biblioteca.push_back(novoLivro);
    cout << "Livro adicionado com sucesso!" << endl;
}

// Função para remover um livro da biblioteca
void removerLivro(vector<Livro> &biblioteca) {
    string titulo;
    cout << "Digite o título do livro que deseja remover: ";
    getline(cin, titulo);
    auto it = find_if(biblioteca.begin(), biblioteca.end(), [titulo](const Livro &livro) {
        return livro.titulo == titulo;
    });
    if (it != biblioteca.end()) {
        biblioteca.erase(it);
        cout << "Livro removido com sucesso!" << endl;
    } else {
        cout << "Livro não encontrado na biblioteca." << endl;
    }
}

// Função para buscar um livro na biblioteca
void buscarLivro(const vector<Livro> &biblioteca) {
    string titulo;
    cout << "Digite o título do livro que deseja buscar: ";
    getline(cin, titulo);
    auto it = find_if(biblioteca.begin(), biblioteca.end(), [titulo](const Livro &livro) {
        return livro.titulo == titulo;
    });
    if (it != biblioteca.end()) {
        cout << "Livro encontrado na biblioteca!" << endl;
        cout << "Título: " << it->titulo << endl;
        cout << "Autor: " << it->autor << endl;
        cout << "Ano: " << it->ano << endl;
    } else {
        cout << "Livro não encontrado na biblioteca." << endl;
    }
}

// Função principal
int main() {
    vector<Livro> biblioteca;
    int opcao;

    do {
        cout << "==== Gerenciamento de Biblioteca ====" << endl;
        cout << "1. Adicionar livro" << endl;
        cout << "2. Remover livro" << endl;
        cout << "3. Buscar livro" << endl;
        cout << "4. Sair" << endl;
        cout << "Digite a opção desejada: ";
        cin >> opcao;
        cin.ignore(); // Limpa o buffer do teclado

        switch (opcao) {
            case 1:
                adicionarLivro(biblioteca);
                break;
            case 2:
                removerLivro(biblioteca);
                break;
            case 3:
                buscarLivro(biblioteca);
                break;
            case 4:
                cout << "Encerrando o programa..." << endl;
                break;
            default:
                cout << "Opção inválida. Tente novamente." << endl;
                break;
        }

        cout << endl;
    } while (opcao != 4);

    return 0;
}
```

Explicação do código:

1. Começamos incluindo as bibliotecas necessárias: `iostream` para entrada e saída de dados, `vector` para armazenamento dos livros e `algorithm` para facilitar a busca de livros na biblioteca.

2. Definimos a estrutura `Livro` que contém os atributos `titulo`, `autor` e `ano`.

3. Em seguida, temos a função `adicionarLivro`, que permite ao usuário adicionar um novo livro à biblioteca. Ela solicita ao usuário todas as informações do livro e adiciona o livro ao vetor `biblioteca`.

4. A função `removerLivro` permite ao usuário remover um livro da biblioteca. Ela solicita ao usuário o título do livro a ser removido e utiliza a função `find_if` para procurar o livro no vetor `biblioteca`. Se o livro for encontrado, ele é removido do vetor.

5. A função `buscarLivro` permite ao usuário buscar um livro na biblioteca. Ela solicita ao usuário o título do livro a ser buscado e utiliza a função `find_if` para procurar o livro no vetor `biblioteca`. Se o livro for encontrado, suas informações são exibidas na tela.

6. Na função `main`, criamos o vetor `biblioteca` para armazenar os livros e a variável `opcao` para armazenar a opção escolhida pelo usuário.

7. Em um loop `do-while`, exibimos um menu de opções para o usuário e executamos a ação correspondente.

8. No final do loop, verificamos se a opção escolhida foi a de sair do programa. Se for, encerramos o programa.

Espero que este código seja útil para você! Caso tenha alguma dúvida, estou à disposição para ajudar.