Claro, vou gerar um código complexo em Julia para você. Aqui está:

```
# Função para verificar se um número é primo
function ePrimo(n)
    if n < 2
        return false
    end
    for i in 2:Int(sqrt(n))
        if n % i == 0
            return false
        end
    end
    return true
end

# Função para calcular a soma dos dígitos de um número
function somaDigitos(n)
    soma = 0
    while n > 0
        soma += n % 10
        n = div(n, 10)
    end
    return soma
end

# Função principal para encontrar a soma dos dígitos de números primos
function somaDigitosPrimos(n)
    soma = 0
    contador = 0
    numero = 2
    
    while contador < n
        if ePrimo(numero)
            soma += somaDigitos(numero)
            contador += 1
        end
        numero += 1
    end
    
    return soma
end

# Exemplo de uso da função principal
n = 10
resultado = somaDigitosPrimos(n)
println("A soma dos dígitos dos primeiros $n números primos é $resultado.")
```

Este código em Julia contém três funções: `ePrimo`, `somaDigitos` e `somaDigitosPrimos`.

A função `ePrimo` verifica se um número é primo. Ela utiliza um loop `for` para verificar se o número é divisível por algum número entre 2 e a raiz quadrada do próprio número. Se encontrar um divisor, retorna `false`. Caso contrário, retorna `true`.

A função `somaDigitos` calcula a soma dos dígitos de um número. Ela utiliza um loop `while` para extrair os dígitos do número dividindo-o por 10 sucessivamente. A cada iteração, soma o dígito extraído à variável `soma`. Por fim, retorna a soma.

A função `somaDigitosPrimos` é a função principal do programa. Ela recebe um parâmetro `n` que representa a quantidade de números primos cuja soma dos dígitos será calculada. A função utiliza um loop `while` para encontrar os `n` números primos, somando a soma de seus dígitos à variável `soma`. A variável `contador` é utilizada para controlar o número de números primos encontrados. A cada iteração, incrementa o número que está sendo verificado até encontrar todos os números primos solicitados.

No exemplo de uso apresentado no final do código, `n` é definido como 10. A função `somaDigitosPrimos` é chamada com este valor e o resultado é armazenado na variável `resultado`. Por fim, o resultado é exibido no console utilizando a função `println`. O resultado será a soma dos dígitos dos primeiros 10 números primos encontrados.