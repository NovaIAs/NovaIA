Claro! Vou criar um código em Python que simula um jogo de adivinhação. Neste jogo, o usuário deve tentar adivinhar um número aleatório gerado pelo programa. O código será dividido em diferentes funções para facilitar a compreensão. 

```python
import random

def exibir_mensagem_inicial():
    print("Bem-vindo ao jogo de adivinhação!")
    print("Tente adivinhar o número secreto entre 1 e 100.")

def obter_numero():
    while True:
        try:
            numero = int(input("Digite um número: "))
            return numero
        except ValueError:
            print("Por favor, digite um número válido.")

def verificar_palpite(palpite, numero_secreto):
    if palpite < numero_secreto:
        return "Maior"
    elif palpite > numero_secreto:
        return "Menor"
    else:
        return "Acertou"

def jogar_novamente():
    resposta = input("Deseja jogar novamente? (s/n): ")
    return resposta.lower() == "s"

def jogar():
    exibir_mensagem_inicial()
    numero_secreto = random.randint(1, 100)
    tentativas = 0
    
    while True:
        palpite = obter_numero()
        resultado = verificar_palpite(palpite, numero_secreto)
        tentativas += 1
        
        if resultado == "Acertou":
            print(f"Parabéns, você acertou em {tentativas} tentativas!")
            break
        else:
            print(f"Tente novamente! O número secreto é {resultado} que {palpite}.")
    
    if not jogar_novamente():
        print("Obrigado por jogar. Até a próxima!")
    else:
        jogar()

jogar()
```

Explicação do código:

1. Primeiro, importamos o módulo `random` para gerar números aleatórios.
2. Em seguida, definimos a função `exibir_mensagem_inicial()` para mostrar as instruções do jogo.
3. A função `obter_numero()` é responsável por solicitar ao usuário um número válido.
4. A função `verificar_palpite(palpite, numero_secreto)` compara o palpite do usuário com o número secreto e retorna uma mensagem indicando se o palpite é maior, menor ou igual ao número secreto.
5. A função `jogar_novamente()` pergunta ao usuário se ele deseja jogar novamente e retorna `True` se a resposta for afirmativa.
6. Por fim, a função principal `jogar()` inicia o jogo, gerando um número secreto aleatório entre 1 e 100 e permitindo que o usuário faça palpites. O jogo continua até o usuário acertar o número ou decidir não jogar novamente.

Espero que você goste deste código! Se tiver alguma dúvida, estou à disposição para ajudar.