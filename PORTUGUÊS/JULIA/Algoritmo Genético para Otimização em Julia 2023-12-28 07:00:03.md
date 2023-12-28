Claro! Aqui está um código complexo em Julia que realiza a otimização de uma função usando o algoritmo genético. O objetivo é encontrar o valor mínimo da função dentro de um determinado intervalo.

```julia
# Definindo a função a ser otimizada
function funcao(x)
    return (x[1] - 2)^2 + (x[2] + 3)^2
end

# Definindo os parâmetros do algoritmo genético
tam_populacao = 50
num_geracoes = 100
taxa_mutacao = 0.1

# Definindo o intervalo em que os valores de x devem estar
intervalo_x = (-10, 10)
intervalo_y = (-10, 10)

# Função para gerar uma população inicial aleatória
function gerar_populacao_inicial(tam_populacao, intervalo_x, intervalo_y)
    populacao = []
    for i in 1:tam_populacao
        individuo = [rand(intervalo_x), rand(intervalo_y)]
        push!(populacao, individuo)
    end
    return populacao
end

# Função para calcular o valor de aptidão de cada indivíduo
function calcular_aptidao(populacao)
    aptidoes = []
    for individuo in populacao
        aptidao = funcao(individuo)
        push!(aptidoes, aptidao)
    end
    return aptidoes
end

# Função para seleção dos pais usando torneio de 2 indivíduos
function selecionar_pais(populacao, aptidoes)
    pais = []
    for i in 1:tam_populacao
        escolhidos = sample(1:tam_populacao, 2, replace=false)
        pai1 = populacao[escolhidos[1]]
        pai2 = populacao[escolhidos[2]]
        if aptidoes[escolhidos[1]] < aptidoes[escolhidos[2]]
            push!(pais, pai1)
        else
            push!(pais, pai2)
        end
    end
    return pais
end

# Função para realizar o cruzamento entre dois pais
function cruzar(pai1, pai2)
    filho = [(pai1[1] + pai2[1]) / 2, (pai1[2] + pai2[2]) / 2]
    return filho
end

# Função para realizar a mutação em um indivíduo
function mutar(individuo, taxa_mutacao, intervalo_x, intervalo_y)
    if rand() < taxa_mutacao
        individuo[1] = rand(intervalo_x)
        individuo[2] = rand(intervalo_y)
    end
    return individuo
end

# Função para realizar a próxima geração
function proxima_geracao(populacao, aptidoes, taxa_mutacao, intervalo_x, intervalo_y)
    pais = selecionar_pais(populacao, aptidoes)
    nova_populacao = []
    for i in 1:tam_populacao
        pai1 = pais[i]
        pai2 = pais[i+1]
        filho = cruzar(pai1, pai2)
        filho = mutar(filho, taxa_mutacao, intervalo_x, intervalo_y)
        push!(nova_populacao, filho)
    end
    return nova_populacao
end

# Função principal para a execução do algoritmo genético
function algoritmo_genetico(tam_populacao, num_geracoes, taxa_mutacao, intervalo_x, intervalo_y)
    populacao = gerar_populacao_inicial(tam_populacao, intervalo_x, intervalo_y)
    for geracao in 1:num_geracoes
        aptidoes = calcular_aptidao(populacao)
        populacao = proxima_geracao(populacao, aptidoes, taxa_mutacao, intervalo_x, intervalo_y)
    end
    return populacao
end

# Executando o algoritmo genético
pop_final = algoritmo_genetico(tam_populacao, num_geracoes, taxa_mutacao, intervalo_x, intervalo_y)

# Encontrando a solução ótima
melhor_individuo = pop_final[1]
melhor_aptidao = funcao(melhor_individuo)
for individuo in pop_final
    aptidao = funcao(individuo)
    if aptidao < melhor_aptidao
        melhor_individuo = individuo
        melhor_aptidao = aptidao
    end
end

println("Melhor solução encontrada:")
println("x = ", melhor_individuo[1])
println("y = ", melhor_individuo[2])
println("Valor mínimo = ", melhor_aptidao)
```

Este código implementa um algoritmo genético para otimizar uma função de duas variáveis. Ele começa gerando uma população inicial aleatória dentro de um intervalo definido para as variáveis x e y. Em seguida, o algoritmo calcula a aptidão de cada indivíduo com base na função a ser otimizada. A seleção dos pais é realizada através de um torneio entre dois indivíduos.

Depois, o algoritmo realiza o cruzamento entre dois pais para gerar um filho. O cruzamento é feito pela média dos valores das variáveis x e y dos pais. Em seguida, é aplicada uma mutação no filho com uma determinada taxa de mutação.

O algoritmo continua gerando a próxima geração até atingir o número máximo de gerações especificado. Por fim, ele seleciona o melhor indivíduo encontrado e exibe o valor mínimo da função, junto com os valores das variáveis x e y correspondentes.

Espero que este código atenda às suas expectativas!