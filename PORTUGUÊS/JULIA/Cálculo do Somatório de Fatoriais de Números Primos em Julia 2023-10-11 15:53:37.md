Claro! Aqui está um código complexo em Julia:

```julia
# Função para calcular o fatorial de um número
function fatorial(n)
    if n == 0 || n == 1
        return 1
    else
        return n * fatorial(n-1)
    end
end

# Função para verificar se um número é primo
function primo(n)
    if n < 2
        return false
    end
    
    for i in 2:isqrt(n)
        if n % i == 0
            return false
        end
    end
    
    return true
end

# Função para calcular o somatório dos fatoriais dos números primos até um determinado limite
function somatorio_fatoriais_primos(limite)
    soma = 0
    
    for i in 2:limite
        if primo(i)
            soma += fatorial(i)
        end
    end
    
    return soma
end

# Função principal para executar o programa
function main()
    println("Digite um número limite: ")
    limite = parse(Int64, readline())
    
    resultado = somatorio_fatoriais_primos(limite)
    
    println("O somatório dos fatoriais dos números primos até ", limite, " é ", resultado)
end

# Chamada da função principal para executar o programa
main()
```

Este código em Julia contém várias funções. A primeira função `fatorial` é responsável por calcular o fatorial de um número. Ela utiliza uma recursão simples para calcular o fatorial de forma eficiente.

A segunda função `primo` verifica se um número é primo ou não. Ela utiliza um loop que vai de 2 até a raiz quadrada do número para verificar se existem divisores. Se algum divisor for encontrado, o número não é primo.

A terceira função `somatorio_fatoriais_primos` calcula o somatório dos fatoriais dos números primos até um determinado limite. Ela utiliza um loop que percorre todos os números de 2 até o limite e verifica se cada número é primo. Se for primo, o fatorial desse número é calculado e adicionado à soma.

Por fim, a função `main` é a função principal que solicita ao usuário um número limite, chama a função `somatorio_fatoriais_primos` passando esse limite como argumento e exibe o resultado na tela.

Esse código é um exemplo de um programa em Julia que realiza cálculos matemáticos complexos, envolvendo fatoriais e números primos.