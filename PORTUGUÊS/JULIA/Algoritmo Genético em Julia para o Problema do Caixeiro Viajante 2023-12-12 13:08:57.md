Claro! Aqui está um código em Julia que implementa o algoritmo de otimização chamado Algoritmo Genético para resolver o problema clássico do Caixeiro Viajante (TSP), também conhecido como Problema do Vendedor Viajante.

```julia
# Importando as bibliotecas necessárias
using Random

# Definindo a função principal do algoritmo genético
function algoritmo_genetico(num_pop, num_ger, taxa_mutacao)
    # Definindo as cidades e suas coordenadas
    cidades = Dict(
        "A" => (2, 3),
        "B" => (5, 4),
        "C" => (1, 6),
        "D" => (6, 8),
        "E" => (3, 2)
    )
    
    # Gerando a população inicial
    populacao = gerar_populacao(num_pop, length(cidades))
    
    # Executando as gerações
    for geracao in 1:num_ger
        # Avaliando a aptidão de cada indivíduo
        aptidoes = avaliar_aptidao(populacao, cidades)
        
        # Selecionando os melhores indivíduos para reprodução
        selecionados = selecao(populacao, aptidoes)
        
        # Realizando o cruzamento entre os indivíduos selecionados
        filhos = cruzamento(selecionados)
        
        # Aplicando a mutação nos filhos gerados
        filhos_mutados = mutacao(filhos, taxa_mutacao)
        
        # Gerando a nova população para a próxima geração
        nova_populacao = vcat(selecionados, filhos_mutados)
        
        # Atualizando a população
        populacao = nova_populacao
    end
    
    # Obtendo o indivíduo com a melhor aptidão
    melhor_individuo = selecionados[indmax(aptidoes)]
    
    # Retornando o percurso e a distância total percorrida pelo melhor indivíduo
    return melhor_individuo, calcular_distancia(melhor_individuo, cidades)
end

# Função para gerar a população inicial
function gerar_populacao(num_pop, num_cidades)
    populacao = []
    for _ in 1:num_pop
        individuo = randperm(num_cidades)
        push!(populacao, individuo)
    end
    return populacao
end

# Função para avaliar a aptidão de cada indivíduo
function avaliar_aptidao(populacao, cidades)
    aptidoes = []
    for individuo in populacao
        distancia = calcular_distancia(individuo, cidades)
        aptidao = 1 / distancia
        push!(aptidoes, aptidao)
    end
    return aptidoes
end

# Função para calcular a distância total percorrida por um indivíduo
function calcular_distancia(individuo, cidades)
    distancia_total = 0
    for i in 1:length(individuo)-1
        cidade_atual = individuo[i]
        proxima_cidade = individuo[i+1]
        distancia = calcular_distancia_entre_cidades(cidades[cidade_atual], cidades[proxima_cidade])
        distancia_total += distancia
    end
    return distancia_total
end

# Função para calcular a distância entre duas cidades
function calcular_distancia_entre_cidades(cidade1, cidade2)
    x1, y1 = cidade1
    x2, y2 = cidade2
    distancia = sqrt((x2 - x1)^2 + (y2 - y1)^2)
    return distancia
end

# Função para selecionar os melhores indivíduos para reprodução
function selecao(populacao, aptidoes)
    ordenados = sortperm(aptidoes, rev=true)
    selecionados = populacao[ordenados[1:Int(length(populacao)/2)]]
    return selecionados
end

# Função para realizar o cruzamento entre os indivíduos selecionados
function cruzamento(selecionados)
    filhos = []
    while length(filhos) < length(selecionados)
        pai1 = selecionados[rand(1:end)]
        pai2 = selecionados[rand(1:end)]
        filho1, filho2 = realizar_cruzamento(pai1, pai2)
        push!(filhos, filho1, filho2)
    end
    return filhos
end

# Função para realizar o cruzamento entre dois indivíduos
function realizar_cruzamento(pai1, pai2)
    ponto_corte = rand(2:length(pai1)-1)
    filho1 = vcat(pai1[1:ponto_corte], pai2[ponto_corte+1:end])
    filho2 = vcat(pai2[1:ponto_corte], pai1[ponto_corte+1:end])
    return filho1, filho2
end

# Função para aplicar a mutação nos filhos gerados
function mutacao(filhos, taxa_mutacao)
    filhos_mutados = []
    for filho in filhos
        if rand() < taxa_mutacao
            posicao1, posicao2 = rand(1:length(filho), 2)
            filho[posicao1], filho[posicao2] = filho[posicao2], filho[posicao1]
        end
        push!(filhos_mutados, filho)
    end
    return filhos_mutados
end

# Chamando a função principal do algoritmo genético
percurso, distancia_total = algoritmo_genetico(100, 1000, 0.01)

# Imprimindo o percurso e a distância total percorrida
println("Percurso: ", percurso)
println("Distância Total: ", distancia_total)
```

Neste código, o algoritmo genético é usado para resolver o problema do Caixeiro Viajante. O algoritmo começa gerando uma população inicial de soluções possíveis, onde cada solução é representada por uma sequência de cidades. Em seguida, a aptidão de cada indivíduo é avaliada com base na distância total percorrida pelo caixeiro viajante ao seguir o percurso definido pela sequência de cidades. Os indivíduos com maior aptidão são selecionados para reprodução.

No cruzamento, dois pais são selecionados aleatoriamente e seus genes são combinados para gerar dois filhos. A mutação é aplicada nos filhos gerados, onde ocorre uma troca aleatória de posições entre duas cidades. Esse processo é repetido por várias gerações, até que se obtenha uma solução próxima da ótima.

No final, o algoritmo retorna o percurso e a distância total percorrida pelo melhor indivíduo encontrado. Esses valores representam a solução encontrada para o problema do Caixeiro Viajante.