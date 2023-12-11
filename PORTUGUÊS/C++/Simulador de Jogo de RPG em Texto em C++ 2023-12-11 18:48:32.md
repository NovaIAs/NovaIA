Claro! Vou gerar um código complexo em C++ para você. Este código é um exemplo de um simulador de um jogo de RPG em texto. Ele contém várias classes e funcionalidades para criar personagens, batalhas e habilidades. Vou explicar cada parte do código para que você possa entendê-lo melhor:

```cpp
#include <iostream>
#include <string>
#include <vector>
#include <ctime>

using namespace std;

// Classe base para representar um personagem
class Personagem {
protected:
    string nome;
    int nivel;
    int vida;
public:
    Personagem(string _nome) {
        nome = _nome;
        nivel = 1;
        vida = 100;
    }

    virtual void atacar() = 0;

    void receberDano(int dano) {
        vida -= dano;
        if (vida <= 0) {
            cout << nome << " foi derrotado(a)!\n";
        }
    }
};

// Classe para representar um guerreiro
class Guerreiro : public Personagem {
public:
    Guerreiro(string _nome) : Personagem(_nome) {}

    void atacar() override {
        cout << nome << " atacou com sua espada!\n";
    }
};

// Classe para representar um mago
class Mago : public Personagem {
public:
    Mago(string _nome) : Personagem(_nome) {}

    void atacar() override {
        cout << nome << " lançou uma bola de fogo!\n";
    }
};

// Classe para representar uma batalha
class Batalha {
private:
    vector<Personagem*> jogadores;
    vector<Personagem*> inimigos;

public:
    void adicionarJogador(Personagem* jogador) {
        jogadores.push_back(jogador);
    }

    void adicionarInimigo(Personagem* inimigo) {
        inimigos.push_back(inimigo);
    }

    void iniciar() {
        cout << "Começou a batalha!\n";

        while (!jogadores.empty() && !inimigos.empty()) {
            // Escolhe um jogador aleatório para atacar
            int indiceJogador = rand() % jogadores.size();
            Personagem* jogador = jogadores[indiceJogador];

            // Escolhe um inimigo aleatório para atacar
            int indiceInimigo = rand() % inimigos.size();
            Personagem* inimigo = inimigos[indiceInimigo];

            // Jogador ataca inimigo
            jogador->atacar();
            inimigo->receberDano(20);

            // Inimigo ataca jogador
            inimigo->atacar();
            jogador->receberDano(10);

            // Remove personagens derrotados
            if (jogador->vida <= 0) {
                jogadores.erase(jogadores.begin() + indiceJogador);
                delete jogador;
            }
            if (inimigo->vida <= 0) {
                inimigos.erase(inimigos.begin() + indiceInimigo);
                delete inimigo;
            }
        }

        cout << "A batalha terminou!\n";

        // Libera a memória dos personagens restantes
        for (Personagem* jogador : jogadores) {
            delete jogador;
        }
        for (Personagem* inimigo : inimigos) {
            delete inimigo;
        }
    }
};

int main() {
    // Define a semente para geração de números aleatórios
    srand(time(0));

    // Cria os personagens
    Guerreiro* guerreiro = new Guerreiro("Guerreiro");
    Mago* mago = new Mago("Mago");

    // Cria a batalha
    Batalha batalha;
    batalha.adicionarJogador(guerreiro);
    batalha.adicionarJogador(mago);

    // Adiciona alguns inimigos
    batalha.adicionarInimigo(new Guerreiro("Inimigo 1"));
    batalha.adicionarInimigo(new Mago("Inimigo 2"));

    // Inicia a batalha
    batalha.iniciar();

    // Libera a memória dos personagens principais
    delete guerreiro;
    delete mago;

    return 0;
}
```

Este código simula uma batalha entre jogadores (guerreiro e mago) e inimigos. 

A classe `Personagem` é uma classe base abstrata que representa um personagem genérico. Ela possui um nome, um nível e uma quantidade de vida. A função `atacar()` é definida como puramente virtual, o que significa que ela deve ser implementada nas classes derivadas. A função `receberDano()` recebe a quantidade de dano que o personagem sofre e subtrai da sua vida. Se a vida chegar a 0 ou menos, o personagem é considerado derrotado.

As classes `Guerreiro` e `Mago` são classes derivadas de `Personagem` e implementam a função `atacar()` de acordo com o tipo de personagem.

A classe `Batalha` é responsável por gerenciar a lógica da batalha. Ela possui vetores de ponteiros para `Personagem` para armazenar os jogadores e inimigos. A função `adicionarJogador()` adiciona um jogador à batalha, enquanto a função `adicionarInimigo()` adiciona um inimigo. A função `iniciar()` inicia a batalha, onde os personagens atacam uns aos outros até que não haja mais jogadores ou inimigos restantes. A lógica de ataque é aleatória, onde um jogador e um inimigo são escolhidos aleatoriamente para atacar. O dano causado é fixo, mas poderia ser facilmente modificado para ser aleatório também.

No `main()`, criamos os personagens principais (guerreiro e mago), criamos a batalha, adicionamos os jogadores e inimigos à batalha e iniciamos a batalha.

Espero que este código atenda às suas expectativas! Se precisar de mais informações ou algum ajuste, é só me avisar.