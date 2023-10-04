Claro! Vou criar um código em Python que simula um jogo de cartas chamado "Blackjack". O objetivo do jogo é chegar o mais perto possível de 21 pontos, sem ultrapassar esse valor.

```python
import random

def criar_baralho():
    baralho = []
    naipes = ['Copas', 'Espadas', 'Ouros', 'Paus']
    valores = ['Ás', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'Valete', 'Dama', 'Rei']
    
    for naipe in naipes:
        for valor in valores:
            carta = (valor, naipe)
            baralho.append(carta)
    
    return baralho

def calcular_valor_mao(mao):
    valor_total = 0
    ases = 0
    
    for carta in mao:
        valor = carta[0]
        
        if valor.isnumeric():
            valor_total += int(valor)
        elif valor == 'Ás':
            valor_total += 11
            ases += 1
        else:
            valor_total += 10
    
    while valor_total > 21 and ases > 0:
        valor_total -= 10
        ases -= 1
    
    return valor_total

def mostrar_mao(mao):
    for carta in mao:
        print(f"{carta[0]} de {carta[1]}")
    
    print()

def jogar_blackjack():
    baralho = criar_baralho()
    random.shuffle(baralho)
    
    mao_jogador = []
    mao_crupie = []
    
    for _ in range(2):
        mao_jogador.append(baralho.pop())
        mao_crupie.append(baralho.pop())
    
    while True:
        print("Sua mão:")
        mostrar_mao(mao_jogador)
        
        valor_mao_jogador = calcular_valor_mao(mao_jogador)
        valor_mao_crupie = calcular_valor_mao(mao_crupie)
        
        if valor_mao_jogador == 21:
            print("Você fez Blackjack! Você ganhou!")
            break
        elif valor_mao_jogador > 21:
            print("Você estourou! Você perdeu.")
            break
        
        opcao = input("Deseja pedir mais uma carta? (S/N): ")
        
        if opcao.lower() == 's':
            mao_jogador.append(baralho.pop())
        else:
            while valor_mao_crupie < 17:
                mao_crupie.append(baralho.pop())
                valor_mao_crupie = calcular_valor_mao(mao_crupie)
            
            print("Mão do crupiê:")
            mostrar_mao(mao_crupie)
            
            if valor_mao_crupie == 21:
                print("Crupiê fez Blackjack! Você perdeu.")
            elif valor_mao_crupie > 21:
                print("Crupiê estourou! Você ganhou!")
            elif valor_mao_jogador > valor_mao_crupie:
                print("Você ganhou!")
            elif valor_mao_jogador < valor_mao_crupie:
                print("Você perdeu.")
            else:
                print("Empate!")
            
            break

jogar_blackjack()
```

Neste código, começo definindo três funções principais: `criar_baralho()`, `calcular_valor_mao(mao)` e `mostrar_mao(mao)`. A primeira função cria um baralho de cartas, onde cada carta é uma tupla contendo o valor e o naipe. A segunda função calcula o valor total de uma mão de cartas, levando em consideração que o Ás pode valer 1 ou 11. A terceira função imprime na tela as cartas de uma mão.

Em seguida, temos a função `jogar_blackjack()`, responsável por controlar o jogo. Ela inicializa o baralho, embaralha as cartas e distribui duas cartas para o jogador e duas para o crupiê. Dentro de um loop, o jogador pode escolher pedir mais cartas ou parar. Quando o jogador para, o crupiê continua pedindo cartas até alcançar pelo menos 17 pontos. Depois disso, o jogo compara as mãos do jogador e do crupiê para determinar o vencedor.

Espero que este código seja útil para você!