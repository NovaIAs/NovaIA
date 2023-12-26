Claro! Aqui está um exemplo de um código complexo em Julia, que utiliza funções, estruturas de controle e manipulação de dados:

```julia
# Função que gera uma lista de números primos até um determinado limite
function encontrar_primos(limite)
    primos = []
    for num in 2:limite
        is_primo = true
        for i in 2:num-1
            if num % i == 0
                is_primo = false
                break
            end
        end
        if is_primo
            push!(primos, num)
        end
    end
    return primos
end

# Função que calcula o fatorial de um número
function calcular_fatorial(n)
    if n == 0 || n == 1
        return 1
    else
        return n * calcular_fatorial(n-1)
    end
end

# Função que verifica se um número é um palíndromo
function verificar_palindromo(num)
    num_str = string(num)
    reverse_str = reverse(num_str)
    if num_str == reverse_str
        return true
    else
        return false
    end
end

# Função principal
function principal()
    println("Bem-vindo(a) ao programa complexo em Julia!")
    println("Selecione uma opção:")
    println("1 - Gerar lista de números primos")
    println("2 - Calcular fatorial")
    println("3 - Verificar se um número é palíndromo")
    opcao = readline()
    
    if opcao == "1"
        println("Digite um limite:")
        limite = parse(Int, readline())
        primos = encontrar_primos(limite)
        println("Números primos encontrados:")
        for primo in primos
            println(primo)
        end
    elseif opcao == "2"
        println("Digite um número:")
        numero = parse(Int, readline())
        fatorial = calcular_fatorial(numero)
        println("O fatorial de $numero é $fatorial")
    elseif opcao == "3"
        println("Digite um número:")
        numero = parse(Int, readline())
        if verificar_palindromo(numero)
            println("$numero é um palíndromo")
        else
            println("$numero não é um palíndromo")
        end
    else
        println("Opção inválida!")
    end
end

# Chamada da função principal
principal()
```

Neste código, temos uma função `encontrar_primos` que recebe um limite e retorna uma lista de números primos até esse limite. Utilizamos dois loops for para verificar se cada número é primo, dividindo-o por todos os números menores que ele.

Temos também a função `calcular_fatorial` que recebe um número e retorna o seu fatorial, utilizando recursão para calcular o fatorial de forma iterativa.

A função `verificar_palindromo` verifica se um número é um palíndromo, convertendo o número em uma string, invertendo-a e comparando com a string original.

Na função principal, o usuário pode escolher entre três opções: gerar a lista de números primos, calcular o fatorial de um número ou verificar se um número é palíndromo. Dependendo da opção escolhida, o programa solicita os valores necessários ao usuário e exibe o resultado correspondente.

Espero que este exemplo atenda às suas expectativas!