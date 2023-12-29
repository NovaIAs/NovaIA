Claro! Vou criar um código complexo em LUA para você. Este código é um exemplo de um sistema de gerenciamento de tarefas. Ele permite adicionar, remover, listar e marcar tarefas como concluídas. Vou explicar o código linha por linha para facilitar o entendimento.

```lua
-- Criando uma tabela vazia para armazenar as tarefas
local tarefas = {}

-- Função para adicionar uma nova tarefa
local function adicionarTarefa()
    print("Digite a nova tarefa:")
    local novaTarefa = io.read() -- Lendo a entrada do usuário
    table.insert(tarefas, novaTarefa) -- Adicionando a nova tarefa à tabela
    print("Tarefa adicionada com sucesso!")
end

-- Função para remover uma tarefa existente
local function removerTarefa()
    print("Digite o número da tarefa que deseja remover:")
    local numeroTarefa = tonumber(io.read()) -- Convertendo a entrada do usuário para número
    if numeroTarefa ~= nil and numeroTarefa >= 1 and numeroTarefa <= #tarefas then
        table.remove(tarefas, numeroTarefa) -- Removendo a tarefa da tabela
        print("Tarefa removida com sucesso!")
    else
        print("Número de tarefa inválido!")
    end
end

-- Função para listar as tarefas
local function listarTarefas()
    print("Lista de tarefas:")
    for i, tarefa in ipairs(tarefas) do
        print(i .. ". " .. tarefa)
    end
end

-- Função para marcar uma tarefa como concluída
local function marcarConcluida()
    print("Digite o número da tarefa que deseja marcar como concluída:")
    local numeroTarefa = tonumber(io.read()) -- Convertendo a entrada do usuário para número
    if numeroTarefa ~= nil and numeroTarefa >= 1 and numeroTarefa <= #tarefas then
        tarefas[numeroTarefa] = "[CONCLUÍDA] " .. tarefas[numeroTarefa] -- Adicionando a marca de tarefa concluída
        print("Tarefa marcada como concluída!")
    else
        print("Número de tarefa inválido!")
    end
end

-- Loop principal do programa
local function main()
    local opcao
    repeat
        -- Exibindo o menu
        print("===== Gerenciador de Tarefas =====")
        print("1. Adicionar Tarefa")
        print("2. Remover Tarefa")
        print("3. Listar Tarefas")
        print("4. Marcar Tarefa como Concluída")
        print("0. Sair")
        print("===================================")
        print("Digite a opção desejada:")
        opcao = tonumber(io.read()) -- Convertendo a entrada do usuário para número

        -- Executando a opção selecionada
        if opcao == 1 then
            adicionarTarefa()
        elseif opcao == 2 then
            removerTarefa()
        elseif opcao == 3 then
            listarTarefas()
        elseif opcao == 4 then
            marcarConcluida()
        elseif opcao == 0 then
            print("Saindo...")
        else
            print("Opção inválida!")
        end
    until opcao == 0
end

-- Chamando a função principal para iniciar o programa
main()
```

Neste código, utilizamos uma tabela `tarefas` para armazenar as tarefas. Cada tarefa é adicionada como uma string na tabela. As funções `adicionarTarefa()`, `removerTarefa()`, `listarTarefas()` e `marcarConcluida()` são responsáveis por adicionar, remover, listar e marcar tarefas, respectivamente.

No loop principal `main()`, exibimos um menu de opções e aguardamos a entrada do usuário para executar a opção selecionada. O programa continua executando até que o usuário escolha a opção "0" para sair.

Espero que este código atenda às suas expectativas! Fique à vontade para realizar ajustes ou adicionar mais funcionalidades se desejar.