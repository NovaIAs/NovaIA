Claro! Aqui está um exemplo de um código complexo em Python:

```python
# Importação de bibliotecas
import random
import time

# Classe principal
class JogoAdivinhacao:
    def __init__(self, nivel):
        self.nivel = nivel
        self.numero_secreto = random.randint(1, 100 * self.nivel)

    def iniciar_jogo(self):
        print("Bem-vindo ao jogo de Adivinhação!")
        print("Estou pensando em um número entre 1 e", 100 * self.nivel)

        pontuacao = 0
        tentativas_maximas = 3 * self.nivel

        for tentativa in range(1, tentativas_maximas + 1):
            print("\nTentativa", tentativa)
            chute = self.obter_chute_valido()

            if chute == self.numero_secreto:
                self.mostrar_mensagem_acerto(pontuacao)
                break
            else:
                self.mostrar_mensagem_erro(chute, pontuacao)

            pontuacao -= abs(self.numero_secreto - chute) ** 2
            time.sleep(1)

        print("\n\nFim do jogo!")
        print("O número secreto era:", self.numero_secreto)
        print("Sua pontuação final é:", pontuacao)

    def obter_chute_valido(self):
        while True:
            try:
                chute = int(input("Digite um número: "))
                if not (1 <= chute <= 100 * self.nivel):
                    print("Valor fora do intervalo válido! Tente novamente.")
                else:
                    return chute
            except ValueError:
                print("Valor inválido! Tente novamente.")

    def mostrar_mensagem_acerto(self, pontuacao):
        print("Parabéns! Você acertou!")
        print("O número secreto era:", self.numero_secreto)
        print("Sua pontuação é:", pontuacao)

    def mostrar_mensagem_erro(self, chute, pontuacao):
        if chute > self.numero_secreto:
            print("Seu chute foi maior que o número secreto!")
        else:
            print("Seu chute foi menor que o número secreto!")
        
        print("Tente novamente...")
        print("Sua pontuação é:", pontuacao)

# Função principal
def main():
    print("=== JOGO DE ADIVINHAÇÃO ===")
    nivel = int(input("Escolha o nível de dificuldade (1 - Fácil | 2 - Médio | 3 - Difícil): "))
    
    # Validação do nível de dificuldade escolhido
    while nivel not in [1, 2, 3]:
        print("Nível inválido! Escolha novamente.")
        nivel = int(input("Escolha o nível de dificuldade (1 - Fácil | 2 - Médio | 3 - Difícil): "))

    jogo = JogoAdivinhacao(nivel)
    jogo.iniciar_jogo()

# Chamada da função principal
if __name__ == '__main__':
    main()
```

Neste código, criamos um jogo de Adivinhação em que o jogador precisa adivinhar um número secreto entre 1 e um limite superior determinado pelo nível de dificuldade escolhido. 

A classe `JogoAdivinhacao` é responsável por gerenciar o jogo. O método `__init__` inicializa o jogo com o nível de dificuldade informado e gera um número secreto aleatório dentro do intervalo válido. 

O método `iniciar_jogo` inicia o jogo exibindo as instruções e as mensagens de acerto e erro. O jogador tem um número máximo de tentativas determinado pelo nível de dificuldade. Se adivinhar o número secreto, recebe uma pontuação positiva baseada na proximidade do chute em relação ao número secreto. Caso contrário, recebe uma pontuação negativa.

O método `obter_chute_valido` solicita ao jogador que digite um número válido dentro do intervalo permitido. Ele continua pedindo um novo número até que o jogador insira um valor válido.

A função `main` é o ponto de entrada do programa. Ela solicita ao jogador que escolha o nível de dificuldade e, em seguida, cria uma instância do jogo de adivinhação e inicia o jogo.

Espero que esse código atenda às suas expectativas!