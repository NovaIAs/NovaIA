Claro! Aqui está um código complexo em C++ que cria uma simulação de um jogo de tabuleiro chamado "Batalha Naval". Neste jogo, o jogador deve tentar afundar todos os navios inimigos, acertando tiros em diferentes posições do tabuleiro.

```cpp
#include <iostream>
#include <vector>
#include <ctime>

using namespace std;

const int TAMANHO_TABULEIRO = 10;
const int NUMERO_NAVIOS = 5;

enum TipoNavio { PortaAvioes, Encouracado, Cruzador, Submarino, Destroyer };

struct Posicao {
    int linha;
    int coluna;
};

struct Navio {
    TipoNavio tipo;
    vector<Posicao> posicoes;
};

class BatalhaNaval {
private:
    vector<vector<char>> tabuleiro;
    vector<Navio> navios;

public:
    BatalhaNaval() {
        tabuleiro.resize(TAMANHO_TABULEIRO, vector<char>(TAMANHO_TABULEIRO, '~'));

        inicializarNavios();
        posicionarNavios();
    }

    void jogar() {
        int tiros = 0;
        int acertos = 0;

        while (acertos < NUMERO_NAVIOS) {
            cout << "=== BATALHA NAVAL ===" << endl;
            cout << "Tentativas: " << tiros << " | Acertos: " << acertos << endl;
            cout << "======================" << endl;

            mostrarTabuleiro();

            Posicao tiro = fazerTiro();

            if (verificarTiro(tiro)) {
                cout << "Você acertou um navio!" << endl;
                acertos++;
                tabuleiro[tiro.linha][tiro.coluna] = 'X';
            } else {
                cout << "Você errou o tiro." << endl;
                tabuleiro[tiro.linha][tiro.coluna] = 'O';
            }

            tiros++;
        }

        cout << "Parabéns! Você afundou todos os navios inimigos em " << tiros << " tiros!" << endl;
    }

private:
    void inicializarNavios() {
        navios.push_back({ PortaAvioes, {} });
        navios.push_back({ Encouracado, {} });
        navios.push_back({ Cruzador, {} });
        navios.push_back({ Submarino, {} });
        navios.push_back({ Destroyer, {} });
    }

    void posicionarNavios() {
        srand(time(nullptr));

        for (auto& navio : navios) {
            bool posicaoValida = false;

            while (!posicaoValida) {
                posicaoValida = true;
                navio.posicoes.clear();

                int linha = rand() % TAMANHO_TABULEIRO;
                int coluna = rand() % TAMANHO_TABULEIRO;

                bool horizontal = rand() % 2 == 0;

                for (int i = 0; i < navio.tipo + 1; i++) {
                    Posicao posicao;

                    if (horizontal) {
                        posicao.linha = linha;
                        posicao.coluna = coluna + i;
                    } else {
                        posicao.linha = linha + i;
                        posicao.coluna = coluna;
                    }

                    if (!validarPosicao(posicao)) {
                        posicaoValida = false;
                        break;
                    }

                    navio.posicoes.push_back(posicao);
                }
            }

            for (const auto& posicao : navio.posicoes) {
                tabuleiro[posicao.linha][posicao.coluna] = 'N';
            }
        }
    }

    void mostrarTabuleiro() {
        cout << "  ";
        for (int i = 0; i < TAMANHO_TABULEIRO; i++) {
            cout << i << " ";
        }
        cout << endl;

        for (int i = 0; i < TAMANHO_TABULEIRO; i++) {
            cout << i << " ";
            for (int j = 0; j < TAMANHO_TABULEIRO; j++) {
                cout << tabuleiro[i][j] << " ";
            }
            cout << endl;
        }
        cout << endl;
    }

    Posicao fazerTiro() {
        Posicao tiro;

        cout << "Digite a linha do tiro: ";
        cin >> tiro.linha;

        cout << "Digite a coluna do tiro: ";
        cin >> tiro.coluna;

        return tiro;
    }

    bool verificarTiro(const Posicao& tiro) {
        for (const auto& navio : navios) {
            for (const auto& posicao : navio.posicoes) {
                if (posicao.linha == tiro.linha && posicao.coluna == tiro.coluna) {
                    return true;
                }
            }
        }

        return false;
    }

    bool validarPosicao(const Posicao& posicao) {
        if (posicao.linha < 0 || posicao.linha >= TAMANHO_TABULEIRO || posicao.coluna < 0 || posicao.coluna >= TAMANHO_TABULEIRO) {
            return false;
        }

        for (const auto& navio : navios) {
            for (const auto& pos : navio.posicoes) {
                if (abs(posicao.linha - pos.linha) <= 1 && abs(posicao.coluna - pos.coluna) <= 1) {
                    return false;
                }
            }
        }

        return true;
    }
};

int main() {
    BatalhaNaval jogo;
    jogo.jogar();

    return 0;
}
```

Explicação do código:
- Definimos o tamanho do tabuleiro e o número de navios como constantes.
- Criamos uma enumeração para representar os diferentes tipos de navios.
- Definimos duas estruturas, `Posicao` para armazenar as coordenadas (linha e coluna) e `Navio` para armazenar o tipo do navio e suas posições.
- A classe `BatalhaNaval` possui um tabuleiro e uma lista de navios.
- O construtor da classe inicializa o tabuleiro com espaços vazios e chama as funções `inicializarNavios` e `posicionarNavios`.
- A função `jogar` permite ao jogador fazer tiros até afundar todos os navios inimigos.
- A função `inicializarNavios` preenche a lista de navios com os tipos correspondentes.
- A função `posicionarNavios` posiciona aleatoriamente os navios no tabuleiro, garantindo que não se sobreponham e estejam completamente dentro do tabuleiro.
- A função `mostrarTabuleiro` exibe o estado atual do tabuleiro.
- A função `fazerTiro` solicita ao jogador as coordenadas do tiro.
- A função `verificarTiro` verifica se o tiro acertou algum navio.
- A função `validarPosicao` verifica se uma dada posição é válida, ou seja, não está fora do tabuleiro e não está próxima de outros navios.

Espero que este código atenda às suas expectativas!