Claro! Aqui está um código em Julia que realiza uma simulação de um ecossistema de animais:

```julia
# Definindo as classes dos animais
abstract type Animal end

struct Leao <: Animal
    energia::Int
    idade::Int
end

struct Zebra <: Animal
    energia::Int
    idade::Int
end

# Função para simular o ecossistema
function simulacao_ecossistema(n_zebras::Int, n_leoes::Int, duracao_simulacao::Int)
    zebras = [Zebra(100, 0) for _ in 1:n_zebras]
    leoes = [Leao(100, 0) for _ in 1:n_leoes]
    
    for _ in 1:duracao_simulacao
        for leao in leoes
            if leao.energia <= 0
                continue
            end
            
            leao.energia -= 10
            leao.idade += 1
            
            if leao.idade >= 10
                leao.energia -= 20
            end
            
            if leao.energia <= 50 && length(zebras) > 0
                zebra = zebras[rand(1:length(zebras))]
                leao.energia += 50
                leao.idade -= 1
                
                if zebra.energia <= 50
                    zebras = filter(x -> x != zebra, zebras)
                else
                    zebra.energia -= 50
                end
            end
        end
        
        for zebra in zebras
            if zebra.energia <= 0
                continue
            end
            
            zebra.energia -= 5
            zebra.idade += 1
            
            if zebra.idade >= 5
                zebra.energia += 10
            end
        end
    end
    
    println("Resultado da simulação:")
    println("Número de zebras restantes: ", length(zebras))
    println("Número de leões restantes: ", length(leoes))
end

# Executando a simulação com 50 zebras, 20 leões e duração de 100 ciclos
simulacao_ecossistema(50, 20, 100)
```

Neste código em Julia, simulamos um ecossistema com zebras e leões. Primeiro, definimos as classes `Leao` e `Zebra`, que representam os leões e zebras no ecossistema, respectivamente. Cada animal possui dois atributos: `energia` e `idade`.

Em seguida, temos a função `simulacao_ecossistema`, que recebe como parâmetros o número de zebras, o número de leões e a duração da simulação. A função cria inicialmente as listas de zebras e leões com as respectivas quantidades fornecidas.

Dentro do loop principal da simulação, para cada leão, verificamos se sua energia é menor ou igual a zero. Se for, o leão está morto e continuamos para o próximo. Caso contrário, diminuímos sua energia em 10 unidades e incrementamos sua idade em 1. Se a idade do leão for maior ou igual a 10, diminuímos sua energia em 20 unidades.

Se a energia do leão for menor ou igual a 50 e ainda houver zebras vivas, escolhemos uma zebra aleatória e incrementamos a energia do leão em 50 unidades. Também decrementamos a idade do leão em 1. Se a energia da zebra escolhida for menor ou igual a 50, removemos a zebra da lista. Caso contrário, decrementamos a energia da zebra em 50 unidades.

Para cada zebra, verificamos se sua energia é menor ou igual a zero. Se for, a zebra está morta e continuamos para a próxima. Caso contrário, diminuímos sua energia em 5 unidades e incrementamos sua idade em 1. Se a idade da zebra for maior ou igual a 5, incrementamos sua energia em 10 unidades.

No final da simulação, exibimos o número de zebras e leões restantes.

Este código é apenas uma simulação simplificada de um ecossistema e pode ser adaptado para incluir mais comportamentos e interações entre os animais.