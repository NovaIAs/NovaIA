Claro! Aqui está um código complexo em C++ que realiza uma simulação de um jogo de tabuleiro chamado "Labirinto Mágico":

```cpp
#include <iostream>
#include <vector>
#include <cstdlib>
#include <ctime>

using namespace std;

const int TAMANHO_TABULEIRO = 10;
const int NUM_JOGADORES = 4;

enum Direcao { CIMA, BAIXO, ESQUERDA, DIREITA };

struct Jogador {
    int x;
    int y;
};

struct Tesouro {
    int x;
    int y;
    char simbolo;
};

vector<vector<char>> tabuleiro(TAMANHO_TABULEIRO, vector<char>(TAMANHO_TABULEIRO, '.'));

vector<Jogador> jogadores(NUM_JOGADORES);
vector<Tesouro> tesouros = {
    {2, 4, 'A'},
    {7, 3, 'B'},
    {5, 8, 'C'},
    {9, 1, 'D'}
};

void mostrarTabuleiro() {
    cout << "  ";
    for (int j = 0; j < TAMANHO_TABULEIRO; j++) {
        cout << j << ' ';
    }
    cout << endl;
    for (int i = 0; i < TAMANHO_TABULEIRO; i++) {
        cout << i << ' ';
        for (int j = 0; j < TAMANHO_TABULEIRO; j++) {
            cout << tabuleiro[i][j] << ' ';
        }
        cout << endl;
    }
}

void colocarJogadoresNoTabuleiro() {
    for (int i = 0; i < NUM_JOGADORES; i++) {
        jogadores[i].x = rand() % TAMANHO_TABULEIRO;
        jogadores[i].y = rand() % TAMANHO_TABULEIRO;
        tabuleiro[jogadores[i].y][jogadores[i].x] = i + '1';
    }
}

bool moverJogador(int jogador, Direcao direcao) {
    int x = jogadores[jogador].x;
    int y = jogadores[jogador].y;
    switch (direcao) {
        case CIMA:
            if (y > 0 && tabuleiro[y - 1][x] == '.') {
                tabuleiro[y][x] = '.';
                jogadores[jogador].y--;
                tabuleiro[y - 1][x] = jogador + '1';
                return true;
            }
            break;
        case BAIXO:
            if (y < TAMANHO_TABULEIRO - 1 && tabuleiro[y + 1][x] == '.') {
                tabuleiro[y][x] = '.';
                jogadores[jogador].y++;
                tabuleiro[y + 1][x] = jogador + '1';
                return true;
            }
            break;
        case ESQUERDA:
            if (x > 0 && tabuleiro[y][x - 1] == '.') {
                tabuleiro[y][x] = '.';
                jogadores[jogador].x--;
                tabuleiro[y][x - 1] = jogador + '1';
                return true;
            }
            break;
        case DIREITA:
            if (x < TAMANHO_TABULEIRO - 1 && tabuleiro[y][x + 1] == '.') {
                tabuleiro[y][x] = '.';
                jogadores[jogador].x++;
                tabuleiro[y][x + 1] = jogador + '1';
                return true;
            }
            break;
    }
    return false;
}

bool jogadorEncontrouTesouro(int jogador) {
    int x = jogadores[jogador].x;
    int y = jogadores[jogador].y;
    for (const Tesouro& tesouro : tesouros) {
        if (x == tesouro.x && y == tesouro.y) {
            tabuleiro[y][x] = tesouro.simbolo;
            return true;
        }
    }
    return false;
}

void jogar() {
    colocarJogadoresNoTabuleiro();
    mostrarTabuleiro();
    cout << endl;

    srand(time(0));
    int jogadorAtual = 0;
    while (true) {
        cout << "Jogador " << jogadorAtual + 1 << ", sua vez de jogar!" << endl;
        cout << "Escolha uma direção (0 - CIMA, 1 - BAIXO, 2 - ESQUERDA, 3 - DIREITA): ";
        int direcao;
        cin >> direcao;

        if (moverJogador(jogadorAtual, static_cast<Direcao>(direcao))) {
            if (jogadorEncontrouTesouro(jogadorAtual)) {
                cout << "Parabéns, você encontrou um tesouro!" << endl;
                tesouros.erase(tesouros.begin() + jogadorAtual);
                if (tesouros.empty()) {
                    cout << "Todos os tesouros foram encontrados! Fim de jogo!" << endl;
                    break;
                }
            }
            jogadorAtual = (jogadorAtual + 1) % NUM_JOGADORES;
            mostrarTabuleiro();
            cout << endl;
        } else {
            cout << "Movimento inválido. Tente novamente." << endl;
        }
    }
}

int main() {
    jogar();
    return 0;
}
```

Neste código, é criado um jogo de labirinto mágico onde cada jogador deve encontrar tesouros espalhados pelo tabuleiro. 

O tabuleiro é uma matriz quadrada de tamanho 10x10 representada pelo vetor de vetores `tabuleiro`. Cada elemento do tabuleiro é inicializado com o caractere '.' para indicar que a posição está vazia.

Os jogadores são representados pela estrutura `Jogador`, que armazena as coordenadas (x, y) da posição atual de cada jogador no tabuleiro.

Os tesouros são representados pela estrutura `Tesouro`, que armazena as coordenadas (x, y) e o símbolo de cada tesouro.

A função `mostrarTabuleiro()` imprime o tabuleiro atualizado na tela.

A função `colocarJogadoresNoTabuleiro()` posiciona os jogadores aleatoriamente no tabuleiro.

A função `moverJogador()` recebe o número do jogador e uma direção (cima, baixo, esquerda ou direita) e verifica se é possível mover o jogador naquela direção. Se for possível, a função atualiza as coordenadas do jogador e o tabuleiro.

A função `jogadorEncontrouTesouro()` verifica se o jogador encontrou algum tesouro na sua posição atual. Caso tenha encontrado, o tesouro é marcado no tabuleiro.

A função `jogar()` é responsável por controlar o fluxo do jogo. Ela exibe o tabuleiro, pergunta ao jogador atual qual direção ele deseja mover e chama as funções correspondentes. O jogo continua até que todos os tesouros sejam encontrados.

No `main()`, a função `jogar()` é chamada para iniciar o jogo.

Este é um exemplo de um código complexo em C++ que utiliza estruturas de dados e lógica de programação para simular um jogo de tabuleiro.