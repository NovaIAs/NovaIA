Claro, vou gerar um código complexo em Lua, que realiza um algoritmo genético para resolver o problema do caixeiro-viajante. Esse é um problema clássico de otimização combinatória que busca encontrar o menor caminho para um viajante que precisa visitar um conjunto de cidades, passando por cada uma delas exatamente uma vez.

```lua
-- Função para calcular a distância entre duas cidades
function calcularDistancia(x1, y1, x2, y2)
    return math.sqrt((x2 - x1)^2 + (y2 - y1)^2)
end

-- Função para gerar uma população inicial de rotas
function gerarPopulacaoInicial(numCidades, numIndividuos)
    local populacao = {}
    for i = 1, numIndividuos do
        local rota = {}
        for j = 1, numCidades do
            table.insert(rota, j)
        end
        table.insert(populacao, rota)
    end
    return populacao
end

-- Função para calcular o custo de uma rota (distância total percorrida)
function calcularCustoRota(rota, cidades)
    local custo = 0
    for i = 1, #rota - 1 do
        local cidadeAtual = rota[i]
        local proximaCidade = rota[i + 1]
        custo = custo + calcularDistancia(cidades[cidadeAtual][1], cidades[cidadeAtual][2], cidades[proximaCidade][1], cidades[proximaCidade][2])
    end
    return custo
end

-- Função para realizar o cruzamento de dois indivíduos (rotas)
function cruzarIndividuos(pai1, pai2)
    local pontoDeCorte = math.random(1, #pai1)
    local filho = {}
    for i = 1, pontoDeCorte do
        filho[i] = pai1[i]
    end
    local j = pontoDeCorte + 1
    for i = 1, #pai2 do
        if not filho[i] then
            filho[i] = pai2[j]
            j = j + 1
        end
    end
    return filho
end

-- Função para realizar a mutação de um indivíduo (rota)
function mutarIndividuo(individuo)
    local indice1 = math.random(1, #individuo)
    local indice2 = math.random(1, #individuo)
    individuo[indice1], individuo[indice2] = individuo[indice2], individuo[indice1]
    return individuo
end

-- Função para selecionar os indivíduos mais aptos para a próxima geração
function selecionarIndividuos(populacao, cidades, numSelecionados)
    local selecionados = {}
    for i = 1, numSelecionados do
        local melhorIndividuo
        local melhorCusto = math.huge
        for _, individuo in ipairs(populacao) do
            local custo = calcularCustoRota(individuo, cidades)
            if custo < melhorCusto then
                melhorIndividuo = individuo
                melhorCusto = custo
            end
        end
        table.insert(selecionados, melhorIndividuo)
        for j, individuo in ipairs(populacao) do
            if individuo == melhorIndividuo then
                table.remove(populacao, j)
                break
            end
        end
    end
    return selecionados
end

-- Função principal para resolver o problema do caixeiro-viajante
function resolverCaixeiroViajante(cidades, numIndividuos, numGeracoes)
    local populacao = gerarPopulacaoInicial(#cidades, numIndividuos)
    for geracao = 1, numGeracoes do
        local selecionados = selecionarIndividuos(populacao, cidades, math.ceil(numIndividuos / 2))
        local novaPopulacao = {}
        for i = 1, #selecionados do
            local pai1 = selecionados[math.random(1, #selecionados)]
            local pai2 = selecionados[math.random(1, #selecionados)]
            local filho = cruzarIndividuos(pai1, pai2)
            if math.random() < 0.1 then
                filho = mutarIndividuo(filho)
            end
            table.insert(novaPopulacao, filho)
        end
        populacao = novaPopulacao
    end
    local melhorIndividuo
    local melhorCusto = math.huge
    for _, individuo in ipairs(populacao) do
        local custo = calcularCustoRota(individuo, cidades)
        if custo < melhorCusto then
            melhorIndividuo = individuo
            melhorCusto = custo
        end
    end
    return melhorIndividuo, melhorCusto
end

-- Exemplo de uso
local cidades = {
    {0, 0},
    {1, 1},
    {2, 2},
    {3, 3},
    {4, 4}
}

local numIndividuos = 100
local numGeracoes = 1000
local melhorRota, menorCusto = resolverCaixeiroViajante(cidades, numIndividuos, numGeracoes)

print("Melhor rota encontrada:")
for _, cidade in ipairs(melhorRota) do
    print(cidade)
end
print("Custo da rota: " .. menorCusto)
```

Neste código, utilizamos Lua para implementar um algoritmo genético para resolver o problema do caixeiro-viajante. Primeiramente, definimos algumas funções auxiliares, como calcular a distância entre duas cidades, gerar a população inicial de rotas, calcular o custo de uma rota, realizar o cruzamento de dois indivíduos, realizar a mutação de um indivíduo e selecionar os indivíduos mais aptos para a próxima geração.

Em seguida, implementamos a função principal `resolverCaixeiroViajante`, que recebe como parâmetros a lista de coordenadas das cidades, o número de indivíduos na população e o número de gerações a serem executadas. Dentro dessa função, fazemos iterações para criar novas gerações, selecionando os melhores indivíduos, realizando o cruzamento e, opcionalmente, aplicando mutações.

No exemplo de uso no final do código, definimos um conjunto de coordenadas para 5 cidades e chamamos a função `resolverCaixeiroViajante`, passando as coordenadas, o número de indivíduos e o número de gerações. O resultado é impresso no console, exibindo a melhor rota encontrada e o custo dessa rota.

Esse código é um exemplo de um algoritmo complexo em Lua que utiliza algoritmos genéticos para resolver um problema de otimização combinatória. Ele pode ser adaptado para diferentes conjuntos de cidades e também para outros problemas similares.