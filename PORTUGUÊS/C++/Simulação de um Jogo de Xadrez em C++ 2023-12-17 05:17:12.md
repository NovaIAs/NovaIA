Claro! Aqui está um exemplo de código complexo em C++ que envolve uma simulação de um jogo de xadrez:

```cpp
#include <iostream>
#include <vector>

// Definindo as classes de cada peça do jogo de xadrez
class Peca {
public:
    virtual void mover() = 0;
};

class Peao : public Peca {
public:
    void mover() override {
        std::cout << "O peão se move para frente." << std::endl;
    }
};

class Torre : public Peca {
public:
    void mover() override {
        std::cout << "A torre se move na vertical ou horizontal." << std::endl;
    }
};

class Bispo : public Peca {
public:
    void mover() override {
        std::cout << "O bispo se move na diagonal." << std::endl;
    }
};

class Cavalo : public Peca {
public:
    void mover() override {
        std::cout << "O cavalo se move em forma de 'L'." << std::endl;
    }
};

class Rainha : public Peca {
public:
    void mover() override {
        std::cout << "A rainha se move na vertical, horizontal ou diagonal." << std::endl;
    }
};

class Rei : public Peca {
public:
    void mover() override {
        std::cout << "O rei se move para qualquer direção, mas apenas uma casa por vez." << std::endl;
    }
};

// Classe que representa o tabuleiro de xadrez
class Tabuleiro {
private:
    std::vector<Peca*> pecas;

public:
    Tabuleiro() {
        // Adicionando as peças no tabuleiro
        pecas.push_back(new Peao());
        pecas.push_back(new Torre());
        pecas.push_back(new Bispo());
        pecas.push_back(new Cavalo());
        pecas.push_back(new Rainha());
        pecas.push_back(new Rei());
    }

    ~Tabuleiro() {
        // Liberando a memória das peças
        for (auto peca : pecas) {
            delete peca;
        }
    }

    void moverPecas() {
        // Movendo cada peça no tabuleiro
        for (auto peca : pecas) {
            peca->mover();
        }
    }
};

int main() {
    Tabuleiro tabuleiro;
    tabuleiro.moverPecas();

    return 0;
}
```

Neste código, estamos criando classes para representar cada peça do jogo de xadrez (Peao, Torre, Bispo, Cavalo, Rainha e Rei) que herdam da classe base Peca. Cada classe implementa o método `mover()` que descreve como a peça se move no jogo.

Em seguida, temos a classe Tabuleiro que possui um vetor de ponteiros para objetos de Peca. No construtor do Tabuleiro, criamos instâncias de cada peça e as adicionamos no vetor. No destrutor, liberamos a memória alocada para as peças.

O método `moverPecas()` itera sobre cada peça no vetor e chama o método `mover()` correspondente. Por fim, no `main()`, criamos uma instância do Tabuleiro e chamamos o método `moverPecas()` para simular o movimento das peças no tabuleiro.