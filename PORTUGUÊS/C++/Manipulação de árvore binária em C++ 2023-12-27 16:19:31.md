Claro! Aqui está um código complexo em C++ que utiliza uma estrutura de dados de árvore binária para armazenar e manipular informações:

```cpp
#include <iostream>
using namespace std;

struct No {
    int valor;
    No* esquerda;
    No* direita;
};

class ArvoreBinaria {
private:
    No* raiz;

    No* criarNo(int valor) {
        No* novoNo = new No;
        novoNo->valor = valor;
        novoNo->esquerda = nullptr;
        novoNo->direita = nullptr;
        return novoNo;
    }

    No* inserirNo(No* noAtual, int valor) {
        if (noAtual == nullptr) {
            return criarNo(valor);
        }

        if (valor < noAtual->valor) {
            noAtual->esquerda = inserirNo(noAtual->esquerda, valor);
        } else if (valor > noAtual->valor) {
            noAtual->direita = inserirNo(noAtual->direita, valor);
        }

        return noAtual;
    }

    No* encontrarNo(No* noAtual, int valor) {
        if (noAtual == nullptr || noAtual->valor == valor) {
            return noAtual;
        }

        if (valor < noAtual->valor) {
            return encontrarNo(noAtual->esquerda, valor);
        }

        return encontrarNo(noAtual->direita, valor);
    }

    No* encontrarNoMinimo(No* noAtual) {
        No* atual = noAtual;
        while (atual && atual->esquerda != nullptr) {
            atual = atual->esquerda;
        }
        return atual;
    }

    No* removerNo(No* noAtual, int valor) {
        if (noAtual == nullptr) {
            return noAtual;
        }

        if (valor < noAtual->valor) {
            noAtual->esquerda = removerNo(noAtual->esquerda, valor);
        } else if (valor > noAtual->valor) {
            noAtual->direita = removerNo(noAtual->direita, valor);
        } else {
            if (noAtual->esquerda == nullptr) {
                No* temp = noAtual->direita;
                delete noAtual;
                return temp;
            } else if (noAtual->direita == nullptr) {
                No* temp = noAtual->esquerda;
                delete noAtual;
                return temp;
            }

            No* temp = encontrarNoMinimo(noAtual->direita);
            noAtual->valor = temp->valor;
            noAtual->direita = removerNo(noAtual->direita, temp->valor);
        }
        return noAtual;
    }

    void imprimirEmOrdem(No* noAtual) {
        if (noAtual == nullptr) {
            return;
        }

        imprimirEmOrdem(noAtual->esquerda);
        cout << noAtual->valor << " ";
        imprimirEmOrdem(noAtual->direita);
    }

public:
    ArvoreBinaria() {
        raiz = nullptr;
    }

    void inserir(int valor) {
        raiz = inserirNo(raiz, valor);
    }

    void remover(int valor) {
        raiz = removerNo(raiz, valor);
    }

    bool buscar(int valor) {
        return encontrarNo(raiz, valor) != nullptr;
    }

    void imprimir() {
        imprimirEmOrdem(raiz);
        cout << endl;
    }
};

int main() {
    ArvoreBinaria arvore;

    arvore.inserir(50);
    arvore.inserir(30);
    arvore.inserir(20);
    arvore.inserir(40);
    arvore.inserir(70);
    arvore.inserir(60);
    arvore.inserir(80);

    cout << "Árvore Binária em ordem (in-order): ";
    arvore.imprimir();

    cout << "Removendo o valor 30..." << endl;
    arvore.remover(30);

    cout << "Árvore Binária em ordem após a remoção: ";
    arvore.imprimir();

    cout << "O valor 60 está na árvore? ";
    cout << (arvore.buscar(60) ? "Sim" : "Não") << endl;

    return 0;
}
```

Esse código implementa uma árvore binária em C++. A árvore é utilizada para armazenar valores inteiros e permite inserir, remover e buscar valores. No exemplo, são inseridos valores na árvore (50, 30, 20, 40, 70, 60, 80), em seguida é removido o valor 30 e é verificado se o valor 60 está presente na árvore.