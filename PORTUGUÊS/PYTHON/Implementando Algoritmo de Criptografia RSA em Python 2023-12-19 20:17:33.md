Claro! Aqui está um código complexo em Python que implementa um algoritmo de criptografia RSA:

```python
import random

def gerar_primo():
    while True:
        num = random.randint(100, 1000)
        if eh_primo(num):
            return num

def eh_primo(num):
    if num <= 1:
        return False
    for i in range(2, int(num**0.5) + 1):
        if num % i == 0:
            return False
    return True

def calcular_mdc(a, b):
    while b != 0:
        a, b = b, a % b
    return a

def calcular_chave_publica(phi_n):
    while True:
        e = random.randint(2, phi_n - 1)
        if calcular_mdc(e, phi_n) == 1:
            return e

def calcular_chave_privada(e, phi_n):
    d = 0
    while (d * e) % phi_n != 1 or d == e:
        d += 1
    return d

def criptografar(mensagem, e, n):
    mensagem_criptografada = []
    for char in mensagem:
        c = ord(char)
        c = pow(c, e, n)
        mensagem_criptografada.append(c)
    return mensagem_criptografada

def descriptografar(mensagem_criptografada, d, n):
    mensagem_descriptografada = ""
    for c in mensagem_criptografada:
        c = pow(c, d, n)
        char = chr(c)
        mensagem_descriptografada += char
    return mensagem_descriptografada

def main():
    p = gerar_primo()
    q = gerar_primo()
    n = p * q
    phi_n = (p - 1) * (q - 1)
    e = calcular_chave_publica(phi_n)
    d = calcular_chave_privada(e, phi_n)

    mensagem = input("Digite a mensagem a ser criptografada: ")

    mensagem_criptografada = criptografar(mensagem, e, n)
    print("Mensagem criptografada:", mensagem_criptografada)

    mensagem_descriptografada = descriptografar(mensagem_criptografada, d, n)
    print("Mensagem descriptografada:", mensagem_descriptografada)

if __name__ == "__main__":
    main()
```

Explicação do código:

1. A função `gerar_primo` é responsável por gerar um número primo aleatório entre 100 e 1000.

2. A função `eh_primo` verifica se um número é primo. Ela percorre todos os números de 2 até a raiz quadrada do número e verifica se ele é divisível por algum desses números.

3. A função `calcular_mdc` calcula o máximo divisor comum entre dois números utilizando o algoritmo de Euclides.

4. A função `calcular_chave_publica` gera uma chave pública aleatória baseada no valor de phi(n). A chave pública é um número entre 2 e phi(n) - 1, cujo máximo divisor comum com phi(n) é igual a 1.

5. A função `calcular_chave_privada` calcula a chave privada com base na chave pública e no valor de phi(n). A chave privada é um número que, quando multiplicado pela chave pública e módulo phi(n), resulta em 1.

6. A função `criptografar` recebe uma mensagem como entrada e criptografa cada caractere utilizando a chave pública e o módulo n. A criptografia é realizada elevando cada caractere à potência da chave pública e fazendo o módulo n.

7. A função `descriptografar` recebe a mensagem criptografada e a chave privada, e realiza o processo inverso ao da criptografia para obter a mensagem original.

8. A função `main` é responsável por executar todo o código. Ela gera dois números primos aleatórios (p e q), calcula n e phi(n), gera a chave pública e privada, e permite ao usuário digitar uma mensagem para ser criptografada. Em seguida, a mensagem é criptografada e descriptografada, e os resultados são exibidos na tela.

Esse código implementa um algoritmo de criptografia RSA, que é amplamente utilizado para garantir a segurança de informações transmitidas pela internet. Ele utiliza conceitos matemáticos avançados, como números primos, mdc e exponenciação modular, para garantir a confidencialidade dos dados.