Claro! Aqui está um código complexo em Julia que envolve cálculos matemáticos avançados e uso de estruturas de dados complexas.

```julia
# Função para calcular o n-ésimo número de Fibonacci
function fibonacci(n)
    if n <= 1
        return n
    else
        return fibonacci(n-1) + fibonacci(n-2)
    end
end

# Função para encontrar todos os números primos menores que um determinado limite
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

# Classe para representar um aluno
struct Aluno
    nome::String
    idade::Int
    notas::Dict{String, Float64}
end

# Função para calcular a média das notas de um aluno
function calcular_media(aluno::Aluno)
    notas = values(aluno.notas)
    return sum(notas) / length(notas)
end

# Exemplo de uso das funções e estruturas definidas acima
function main()
    # Cálculo do 50º número de Fibonacci
    fib_50 = fibonacci(50)
    println("O 50º número de Fibonacci é: $fib_50")
    
    # Encontrar todos os números primos menores que 100
    primos_ate_100 = encontrar_primos(100)
    println("Todos os números primos menores que 100: $primos_ate_100")
    
    # Criar um objeto Aluno e calcular sua média de notas
    notas_aluno = Dict("Matemática" => 8.5, "Português" => 7.0, "Ciências" => 9.0)
    aluno = Aluno("João", 15, notas_aluno)
    media_aluno = calcular_media(aluno)
    println("A média de notas do aluno ${aluno.nome} é: $media_aluno")
end

# Chamada da função principal
main()
```

Este código em Julia realiza três tarefas diferentes:

1. Calcula o 50º número de Fibonacci usando uma função recursiva.
2. Encontra todos os números primos menores que um determinado limite usando uma função de iteração e uma verificação de primalidade.
3. Demonstra o uso de uma estrutura de dados complexa representada por uma classe `Aluno`, juntamente com funções para calcular a média de notas de um aluno.

Espero que esse código atenda às suas expectativas!