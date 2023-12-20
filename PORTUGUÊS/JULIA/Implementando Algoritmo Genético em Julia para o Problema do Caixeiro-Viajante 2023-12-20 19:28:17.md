Claro! Aqui está um exemplo de código complexo em Julia que mostra como implementar um algoritmo genético para resolver o problema do caixeiro-viajante:

```julia
# Definindo as cidades
cidades = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J"]

# Distâncias entre as cidades
distancias = [
    0  29  20  21  16  31  100  12  4  31;
   29   0  15  29  28  40  72  21  29  41;
   20  15   0  15  14  25  81   9  23  27;
   21  29  15   0   4  12  92  12  25  13;
   16  28  14   4   0  16  94   9  20  16;
   31  40  25  12  16   0  95  24  36  3;
  100  72  81  92  94  95   0  90 101  99;
   12  21   9  12   9  24  90   0  15  25;
    4  29  23  25  20  36 101  15   0  35;
   31  41  27  13  16   3  99  25  35   0
]

# Função para calcular a distância total de um caminho
function calcular_distancia(caminho)
    distancia_total = 0
    for i in 1:length(caminho)-1
        cidade_atual = caminho[i]
        proxima_cidade = caminho[i+1]
        distancia_total += distancias[cidade_atual, proxima_cidade]
    end
    return distancia_total
end

# Função para gerar uma população inicial aleatória
function gerar_populacao_inicial(tamanho_populacao)
    populacao = []
    for i in 1:tamanho_populacao
        caminho = vcat(cidades[1], shuffle(cidades[2:end-1]), cidades[end])
        push!(populacao, caminho)
    end
    return populacao
end

# Função para selecionar os melhores indivíduos da população atual
function selecionar_melhores(populacao, percentual_selecao)
    tamanho_selecao = Int(round(length(populacao) * percentual_selecao))
    melhores = sort(populacao, by=x->calcular_distancia(x))[1:tamanho_selecao]
    return melhores
end

# Função para cruzar dois indivíduos
function cruzar(individuo1, individuo2)
    ponto_corte = rand(2:length(individuo1)-1)
    filho1 = vcat(individuo1[1:ponto_corte], setdiff(individuo2, individuo1[1:ponto_corte]))
    filho2 = vcat(individuo2[1:ponto_corte], setdiff(individuo1, individuo2[1:ponto_corte]))
    return filho1, filho2
end

# Função para mutar um indivíduo
function mutar(individuo, taxa_mutacao)
    if rand() < taxa_mutacao
        posicao1 = rand(2:length(individuo)-1)
        posicao2 = rand(2:length(individuo)-1)
        individuo[posicao1], individuo[posicao2] = individuo[posicao2], individuo[posicao1]
    end
    return individuo
end

# Função para gerar uma nova geração
function gerar_nova_geracao(populacao, percentual_selecao, taxa_mutacao)
    melhores = selecionar_melhores(populacao, percentual_selecao)
    nova_geracao = copy(melhores)
    while length(nova_geracao) < length(populacao)
        individuo1 = rand(melhores)
        individuo2 = rand(melhores)
        filho1, filho2 = cruzar(individuo1, individuo2)
        push!(nova_geracao, mutar(filho1, taxa_mutacao))
        push!(nova_geracao, mutar(filho2, taxa_mutacao))
    end
    return nova_geracao
end

# Parâmetros do algoritmo genético
tamanho_populacao = 100
percentual_selecao = 0.2
taxa_mutacao = 0.1
numero_geracoes = 100

# Gerando a população inicial
populacao = gerar_populacao_inicial(tamanho_populacao)

# Executando o algoritmo genético
for geracao in 1:numero_geracoes
    populacao = gerar_nova_geracao(populacao, percentual_selecao, taxa_mutacao)
end

# Obtendo o melhor caminho da última geração
melhor_caminho = sort(populacao, by=x->calcular_distancia(x))[1]

# Imprimindo o melhor caminho
println("Melhor caminho encontrado:")
for cidade in melhor_caminho
    print(cidade, " -> ")
end
println(melhor_caminho[1])

# Imprimindo a distância total do melhor caminho
println("Distância total do melhor caminho: ", calcular_distancia(melhor_caminho))
```

Neste código, implementamos um algoritmo genético para resolver o problema do caixeiro-viajante, que consiste em encontrar o caminho mais curto que visita todas as cidades uma vez e retorna à cidade inicial.

No código, começamos definindo as cidades e as distâncias entre elas. Em seguida, definimos várias funções que são utilizadas no algoritmo genético:

- A função `calcular_distancia` calcula a distância total percorrida em um determinado caminho.
- A função `gerar_populacao_inicial` gera uma população inicial aleatória de caminhos.
- A função `selecionar_melhores` seleciona os melhores indivíduos da população atual com base na distância total percorrida.
- A função `cruzar` realiza o cruzamento entre dois indivíduos para gerar filhos.
- A função `mutar` realiza uma mutação em um indivíduo, trocando aleatoriamente duas posições do caminho.
- A função `gerar_nova_geracao` gera uma nova geração a partir dos melhores indivíduos da geração atual, cruzando-os e realizando mutações.

Em seguida, definimos os parâmetros do algoritmo genético, como o tamanho da população, o percentual de seleção, a taxa de mutação e o número de gerações.

Por fim, executamos o algoritmo genético por um determinado número de gerações, obtendo o melhor caminho encontrado e imprimindo-o juntamente com a distância total percorrida.