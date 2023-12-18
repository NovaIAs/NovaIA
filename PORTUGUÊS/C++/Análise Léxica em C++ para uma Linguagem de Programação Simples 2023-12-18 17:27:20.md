Claro! Aqui está um código complexo em C++ que realiza a análise léxica de uma linguagem de programação simples. Este código é escrito em português e explicado linha por linha:

```cpp
#include <iostream>
#include <string>
#include <vector>

using namespace std;

// Classe que representa um token
class Token {
public:
    string tipo;
    string valor;
};

// Função de análise léxica
vector<Token> analiseLexica(string codigo) {
    vector<Token> tokens;
    string tokenAtual = "";
    string tipoAtual = "";

    // Percorre o código caractere por caractere
    for (char c : codigo) {
        // Verifica se o caractere é um espaço em branco
        if (isspace(c)) {
            // Se houver um token atual, adiciona à lista de tokens
            if (!tokenAtual.empty()) {
                Token token;
                token.tipo = tipoAtual;
                token.valor = tokenAtual;
                tokens.push_back(token);

                // Limpa o token atual e o tipo atual
                tokenAtual = "";
                tipoAtual = "";
            }
        }
        // Verifica se o caractere é um dígito
        else if (isdigit(c)) {
            tokenAtual += c;
            tipoAtual = "Número";
        }
        // Verifica se o caractere é uma letra
        else if (isalpha(c)) {
            tokenAtual += c;
            tipoAtual = "Identificador";
        }
        // Verifica se o caractere é um operador
        else if (c == '+' || c == '-' || c == '*' || c == '/') {
            tokenAtual += c;
            tipoAtual = "Operador";
        }
        // Verifica se o caractere é um símbolo de pontuação
        else if (c == ';' || c == ',' || c == '(' || c == ')') {
            tokenAtual += c;
            tipoAtual = "Símbolo";
        }
    }

    // Adiciona o último token à lista de tokens, se houver
    if (!tokenAtual.empty()) {
        Token token;
        token.tipo = tipoAtual;
        token.valor = tokenAtual;
        tokens.push_back(token);
    }

    return tokens;
}

int main() {
    string codigo = "int x = 10; float y = 3.14; x = x + y;";

    // Realiza a análise léxica do código
    vector<Token> tokens = analiseLexica(codigo);

    // Imprime os tokens encontrados
    for (Token token : tokens) {
        cout << "Tipo: " << token.tipo << ", Valor: " << token.valor << endl;
    }

    return 0;
}
```

Neste código, temos uma classe `Token` que representa um token de uma linguagem de programação. A função `analiseLexica` realiza a análise léxica de um código, identificando os diferentes tipos de tokens presentes, como números, identificadores, operadores e símbolos de pontuação.

O código principal no `main` define um exemplo de código-fonte a ser analisado e chama a função `analiseLexica`. Em seguida, os tokens encontrados são impressos na tela, exibindo o tipo e o valor de cada um.

Esta é apenas uma implementação básica de análise léxica em C++. É importante ressaltar que a análise léxica é apenas uma etapa do processo de compilação de uma linguagem de programação e, em uma implementação real, outros passos, como análise sintática e geração de código intermediário, também seriam necessários.