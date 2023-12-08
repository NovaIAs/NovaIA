Claro! Aqui está um código complexo em Julia:

```julia
# Função que retorna a soma dos fatoriais até um número N
function soma_fatoriais(N)
    soma = 0  # Variável para armazenar a soma dos fatoriais
    for i in 1:N
        fatorial = 1  # Variável para armazenar o fatorial de cada número
        for j in 1:i
            fatorial *= j  # Cálculo do fatorial
        end
        soma += fatorial  # Adição do fatorial à soma
    end
    return soma
end

# Função que verifica se um número é primo
function is_primo(num)
    if num < 2
        return false
    end
    for i in 2:isqrt(num)
        if num % i == 0
            return false
        end
    end
    return true
end

# Função principal
function main()
    println("Calculando a soma dos fatoriais...")
    N = 10
    soma = soma_fatoriais(N)
    println("A soma dos fatoriais até $N é $soma")

    println("\nVerificando se alguns números são primos...")
    numeros = [10, 15, 23, 37, 42]
    for num in numeros
        if is_primo(num)
            println("$num é primo")
        else
            println("$num não é primo")
        end
    end
end

# Chamada da função principal
main()
```

Nesse código, temos duas funções principais: `soma_fatoriais` e `is_primo`. 

A função `soma_fatoriais` recebe um número `N` e retorna a soma dos fatoriais de todos os números de 1 até `N`. Ela utiliza dois loops `for` aninhados para calcular o fatorial de cada número e adicioná-lo à variável `soma`. No final, retorna o valor da soma.

A função `is_primo` recebe um número `num` e verifica se ele é primo. Caso o número seja menor que 2, retorna falso. Caso contrário, faz um loop `for` de 2 até a raiz quadrada do número (`isqrt(num)`) e verifica se o número é divisível por algum dos valores do loop. Caso seja divisível, retorna falso. Caso contrário, retorna verdadeiro.

A função `main` é a função principal do programa. Nela, é definido um valor `N` e calculada a soma dos fatoriais até esse número utilizando a função `soma_fatoriais`. Em seguida, é feita uma verificação de primos para alguns números utilizando a função `is_primo`. Os resultados são impressos no console.

Por fim, a função `main` é chamada para iniciar a execução do programa.