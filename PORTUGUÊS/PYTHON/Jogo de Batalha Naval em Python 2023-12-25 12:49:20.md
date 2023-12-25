Claro! Vou criar um código que simula um jogo de tabuleiro chamado "Batalha Naval". Neste jogo, dois jogadores vão tentar afundar os navios um do outro em um tabuleiro de 10x10. Cada jogador tem uma frota de 5 navios, sendo eles: um porta-aviões (5 casas), um encouraçado (4 casas), um cruzador (3 casas), um contratorpedeiro (2 casas) e um submarino (1 casa).

Aqui está o código:

```python
import random

# Criando o tabuleiro
tabuleiro = []
for _ in range(10):
    linha = ["~"] * 10
    tabuleiro.append(linha)

# Definindo a frota de cada jogador
frota_jogador1 = {"Porta-aviões": 5, "Encouraçado": 4, "Cruzador": 3, "Contratorpedeiro": 2, "Submarino": 1}
frota_jogador2 = {"Porta-aviões": 5, "Encouraçado": 4, "Cruzador": 3, "Contratorpedeiro": 2, "Submarino": 1}

# Posicionando os navios do jogador 1
for navio, tamanho in frota_jogador1.items():
    while True:
        linha = random.randint(0, 9)
        coluna = random.randint(0, 9)
        direcao = random.choice(["horizontal", "vertical"])

        if direcao == "horizontal" and coluna + tamanho <= 10:
            valido = True
            for i in range(tamanho):
                if tabuleiro[linha][coluna + i] != "~":
                    valido = False
                    break
            if valido:
                for i in range(tamanho):
                    tabuleiro[linha][coluna + i] = navio[0]
                break

        elif direcao == "vertical" and linha + tamanho <= 10:
            valido = True
            for i in range(tamanho):
                if tabuleiro[linha + i][coluna] != "~":
                    valido = False
                    break
            if valido:
                for i in range(tamanho):
                    tabuleiro[linha + i][coluna] = navio[0]
                break

# Posicionando os navios do jogador 2
for navio, tamanho in frota_jogador2.items():
    while True:
        linha = random.randint(0, 9)
        coluna = random.randint(0, 9)
        direcao = random.choice(["horizontal", "vertical"])

        if direcao == "horizontal" and coluna + tamanho <= 10:
            valido = True
            for i in range(tamanho):
                if tabuleiro[linha][coluna + i] != "~":
                    valido = False
                    break
            if valido:
                for i in range(tamanho):
                    tabuleiro[linha][coluna + i] = navio[0]
                break

        elif direcao == "vertical" and linha + tamanho <= 10:
            valido = True
            for i in range(tamanho):
                if tabuleiro[linha + i][coluna] != "~":
                    valido = False
                    break
            if valido:
                for i in range(tamanho):
                    tabuleiro[linha + i][coluna] = navio[0]
                break

# Função para exibir o tabuleiro
def exibir_tabuleiro():
    for linha in tabuleiro:
        print(" ".join(linha))

# Início do jogo
while True:
    # Turno do jogador 1
    print("JOGADOR 1")
    exibir_tabuleiro()
    linha = int(input("Digite a linha (0-9): "))
    coluna = int(input("Digite a coluna (0-9): "))

    if tabuleiro[linha][coluna] == "~":
        print("Água!")
        tabuleiro[linha][coluna] = "O"
    elif tabuleiro[linha][coluna] == "X":
        print("Você já atirou nessa posição!")
    else:
        print("Acertou um navio!")
        tabuleiro[linha][coluna] = "X"
        frota_jogador2[tabuleiro[linha][coluna]] -= 1
        if frota_jogador2[tabuleiro[linha][coluna]] == 0:
            print("Você afundou o", tabuleiro[linha][coluna] + "!")

    # Verifica se o jogador 1 venceu
    if all(value == 0 for value in frota_jogador2.values()):
        print("Parabéns, Jogador 1! Você venceu!")
        break

    # Turno do jogador 2
    print("JOGADOR 2")
    exibir_tabuleiro()
    linha = int(input("Digite a linha (0-9): "))
    coluna = int(input("Digite a coluna (0-9): "))

    if tabuleiro[linha][coluna] == "~":
        print("Água!")
        tabuleiro[linha][coluna] = "O"
    elif tabuleiro[linha][coluna] == "X":
        print("Você já atirou nessa posição!")
    else:
        print("Acertou um navio!")
        tabuleiro[linha][coluna] = "X"
        frota_jogador1[tabuleiro[linha][coluna]] -= 1
        if frota_jogador1[tabuleiro[linha][coluna]] == 0:
            print("Você afundou o", tabuleiro[linha][coluna] + "!")

    # Verifica se o jogador 2 venceu
    if all(value == 0 for value in frota_jogador1.values()):
        print("Parabéns, Jogador 2! Você venceu!")
        break
```

Neste código, utilizei listas e dicionários para representar o tabuleiro, a frota de cada jogador e a posição dos navios. O jogo é jogado por turnos, onde cada jogador escolhe uma posição no tabuleiro para atirar. O programa verifica se a posição escolhida é água, um navio ou se já foi atirada anteriormente. Além disso, o programa também verifica se um navio foi afundado e exibe uma mensagem adequada. O jogo continua até que um dos jogadores afunde todos os navios do adversário.