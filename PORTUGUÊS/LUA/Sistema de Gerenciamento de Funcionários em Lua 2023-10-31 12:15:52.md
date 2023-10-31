Claro! Aqui está um código complexo em Lua, que envolve um sistema de gerenciamento de funcionários em uma empresa:

```lua
-- Definindo a estrutura de dados para um funcionário
local funcionario = {
    nome = "",
    idade = 0,
    cargo = "",
    salario = 0.0
}

-- Função para adicionar um novo funcionário
local function adicionarFuncionario(empresa)
    local novoFuncionario = {}

    print("Digite o nome do funcionário:")
    novoFuncionario.nome = io.read()

    print("Digite a idade do funcionário:")
    novoFuncionario.idade = tonumber(io.read())

    print("Digite o cargo do funcionário:")
    novoFuncionario.cargo = io.read()

    print("Digite o salário do funcionário:")
    novoFuncionario.salario = tonumber(io.read())

    -- Adicionando o novo funcionário à lista de funcionários da empresa
    table.insert(empresa.funcionarios, novoFuncionario)
end

-- Função para listar todos os funcionários da empresa
local function listarFuncionarios(empresa)
    for i, funcionario in ipairs(empresa.funcionarios) do
        print("Funcionário " .. i .. ":")
        print("Nome: " .. funcionario.nome)
        print("Idade: " .. funcionario.idade)
        print("Cargo: " .. funcionario.cargo)
        print("Salário: " .. funcionario.salario)
        print("---------------------------------------")
    end
end

-- Função para calcular a média salarial dos funcionários da empresa
local function calcularMediaSalarial(empresa)
    local totalSalarios = 0.0

    for _, funcionario in ipairs(empresa.funcionarios) do
        totalSalarios = totalSalarios + funcionario.salario
    end

    local mediaSalarial = totalSalarios / #empresa.funcionarios

    print("A média salarial dos funcionários da empresa é: " .. mediaSalarial)
end

-- Função principal
local function main()
    -- Criando a estrutura de dados para a empresa
    local empresa = {
        nome = "Minha Empresa",
        funcionarios = {}
    }

    local opcao = 0

    while opcao ~= 4 do
        print("---------- MENU ----------")
        print("1. Adicionar funcionário")
        print("2. Listar funcionários")
        print("3. Calcular média salarial")
        print("4. Sair")
        print("---------------------------")
        print("Digite o número da opção desejada:")

        opcao = tonumber(io.read())

        if opcao == 1 then
            adicionarFuncionario(empresa)
        elseif opcao == 2 then
            listarFuncionarios(empresa)
        elseif opcao == 3 then
            calcularMediaSalarial(empresa)
        elseif opcao == 4 then
            print("Encerrando o programa...")
        else
            print("Opção inválida. Tente novamente.")
        end
    end
end

main()
```

Este código em Lua é um exemplo de um sistema simples de gerenciamento de funcionários em uma empresa. Ele permite adicionar novos funcionários, listar todos os funcionários e calcular a média salarial dos mesmos.

A estrutura de dados utilizada é um objeto `empresa`, que contém o nome da empresa e uma lista de funcionários. Cada funcionário é representado por um objeto com campos como nome, idade, cargo e salário.

A função `adicionarFuncionario` solicita ao usuário que digite as informações do novo funcionário e as armazena em um novo objeto. Em seguida, esse objeto é adicionado à lista de funcionários da empresa.

A função `listarFuncionarios` itera sobre a lista de funcionários da empresa e imprime as informações de cada um na tela.

A função `calcularMediaSalarial` calcula a média salarial dos funcionários da empresa, somando todos os salários e dividindo pelo número de funcionários.

A função `main` é a função principal que inicia o programa. Ela exibe um menu com opções para adicionar funcionário, listar funcionários, calcular média salarial ou sair. O usuário pode escolher uma opção digitando o número correspondente.

Esse código é apenas um exemplo básico de um sistema de gerenciamento de funcionários em Lua. Você pode modificá-lo e adicionar mais funcionalidades de acordo com suas necessidades.