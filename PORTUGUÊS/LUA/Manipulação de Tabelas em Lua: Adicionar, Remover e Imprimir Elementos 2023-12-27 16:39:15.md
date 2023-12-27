local tabela = {} -- Cria uma tabela vazia

-- Função para adicionar elementos na tabela
local function adicionarElemento(elemento)
    tabela[#tabela + 1] = elemento
end

-- Função para remover elementos da tabela
local function removerElemento(elemento)
    for i, v in pairs(tabela) do
        if v == elemento then
            table.remove(tabela, i)
            break
        end
    end
end

-- Função para imprimir os elementos da tabela
local function imprimirTabela()
    for i, v in ipairs(tabela) do
        print(v)
    end
end

-- Exemplo de utilização das funções
adicionarElemento("Elemento 1")
adicionarElemento("Elemento 2")
adicionarElemento("Elemento 3")
imprimirTabela()
removerElemento("Elemento 2")
imprimirTabela()

--[[ Explicação do código:
    - O código começa criando uma tabela vazia através da variável 'tabela'.
    - Em seguida, temos a função 'adicionarElemento' que recebe um parâmetro 'elemento' e adiciona esse elemento na tabela.
    - A função 'removerElemento' recebe um parâmetro 'elemento' e percorre a tabela procurando por esse elemento. Caso o encontre, remove-o da tabela.
    - A função 'imprimirTabela' percorre a tabela e imprime cada elemento na saída padrão.
    - O exemplo de utilização das funções adiciona três elementos na tabela, imprime a tabela, remove o segundo elemento e imprime novamente a tabela.
    - No final, temos a explicação do código através dos comentários '--[[ ]]--'.
    - Esse código exemplifica o uso de tabelas e funções em Lua, mostrando como adicionar, remover e imprimir elementos em uma tabela.
--]]