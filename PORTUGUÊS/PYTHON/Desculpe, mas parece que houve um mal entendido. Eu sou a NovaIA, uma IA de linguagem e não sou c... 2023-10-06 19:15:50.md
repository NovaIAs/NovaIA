Claro! Aqui está um código complexo em Python que combina vários conceitos e recursos avançados da linguagem:

```python
import math

def calc_fatorial(n):
    if n == 0:
        return 1
    else:
        return n * calc_fatorial(n-1)

def soma_primos(n):
    soma = 0
    i = 2
    while i <= n:
        eh_primo = True
        for j in range(2, int(math.sqrt(i)) + 1):
            if i % j == 0:
                eh_primo = False
                break
        if eh_primo:
            soma += i
        i += 1
    return soma

def imprime_tabuada(n):
    for i in range(1, 11):
        print(f'{n} x {i} = {n * i}')

def main():
    try:
        numero = int(input("Digite um número: "))
        fatorial = calc_fatorial(numero)
        print(f'O fatorial de {numero} é {fatorial}')
        
        soma = soma_primos(numero)
        print(f'A soma de todos os números primos até {numero} é {soma}')
        
        imprime_tabuada(numero)
        
    except ValueError:
        print("Por favor, digite um número válido.")

if __name__ == "__main__":
    main()
```

Este código é composto por várias funções e recursos. Vamos explicar cada parte:

1. Primeiro, importamos o módulo `math` para utilizar a função `sqrt` que será usada na função `soma_primos` para encontrar a raiz quadrada de um número.
2. Em seguida, definimos a função `calc_fatorial` que recebe um número `n` e calcula o fatorial desse número utilizando recursão. Se `n` for igual a zero, retornamos 1. Caso contrário, multiplicamos `n` pelo fatorial de `n-1`.
3. A função `soma_primos` recebe um número `n` e calcula a soma de todos os números primos até esse número. Utilizamos um laço `while` para iterar de 2 até `n`. Para verificar se um número é primo, utilizamos um laço `for` que itera de 2 até a raiz quadrada do número. Se o número for divisível por qualquer um dos números do laço `for`, não é primo. Caso contrário, é primo e somamos ao total.
4. A função `imprime_tabuada` recebe um número `n` e imprime a tabuada desse número, utilizando um laço `for` que itera de 1 até 10. A cada iteração, imprimimos a multiplicação de `n` pelo valor atual do laço `for`.
5. Por fim, a função `main` é executada. Ela solicita ao usuário um número, calcula o fatorial desse número chamando a função `calc_fatorial`, calcula a soma de todos os números primos até esse número chamando a função `soma_primos`, e imprime a tabuada desse número chamando a função `imprime_tabuada`.
6. Para lidar com possíveis erros de entrada inválida, usamos um bloco `try-except` para capturar a exceção `ValueError` caso o usuário digite um valor inválido.
7. A função `main` é chamada dentro do bloco `if __name__ == "__main__":` para garantir que ela só será executada se o script for executado diretamente (não se for importado como um módulo).

Espero que este código atenda aos seus requisitos de complexidade e diferenciação!