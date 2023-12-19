Claro, vou gerar um código complexo em LUA para você. Este código é um exemplo de um sistema de gerenciamento de funcionários em uma empresa, onde é possível adicionar, remover, listar e buscar funcionários. 

```lua
-- Definição da tabela para armazenar os funcionários
local funcionarios = {}

-- Função para adicionar um novo funcionário
function adicionarFuncionario(nome, idade, cargo)
  local funcionario = {
    nome = nome,
    idade = idade,
    cargo = cargo
  }
  table.insert(funcionarios, funcionario)
end

-- Função para remover um funcionário pelo nome
function removerFuncionario(nome)
  for i, funcionario in ipairs(funcionarios) do
    if funcionario.nome == nome then
      table.remove(funcionarios, i)
      return true
    end
  end
  return false
end

-- Função para listar todos os funcionários
function listarFuncionarios()
  for i, funcionario in ipairs(funcionarios) do
    print("Nome: " .. funcionario.nome .. ", Idade: " .. funcionario.idade .. ", Cargo: " .. funcionario.cargo)
  end
end

-- Função para buscar um funcionário pelo nome
function buscarFuncionario(nome)
  for i, funcionario in ipairs(funcionarios) do
    if funcionario.nome == nome then
      return funcionario
    end
  end
  return nil
end

-- Exemplo de uso das funções
adicionarFuncionario("João", 30, "Desenvolvedor")
adicionarFuncionario("Maria", 28, "Analista de Dados")
adicionarFuncionario("José", 35, "Gerente de Projetos")

listarFuncionarios()

print("")

removerFuncionario("Maria")

listarFuncionarios()

print("")

local funcionario = buscarFuncionario("João")
if funcionario ~= nil then
  print("Funcionário encontrado: " .. funcionario.nome)
else
  print("Funcionário não encontrado.")
end
```

Este código em LUA implementa um sistema básico de gerenciamento de funcionários. Ele utiliza uma tabela chamada `funcionarios` para armazenar os funcionários cadastrados. 

As principais funções implementadas são:

- `adicionarFuncionario(nome, idade, cargo)`: recebe as informações de nome, idade e cargo de um funcionário e adiciona-o à tabela `funcionarios`.

- `removerFuncionario(nome)`: recebe o nome de um funcionário e remove-o da tabela `funcionarios`, se existir.

- `listarFuncionarios()`: percorre a tabela `funcionarios` e imprime na tela as informações de todos os funcionários.

- `buscarFuncionario(nome)`: recebe o nome de um funcionário e retorna o funcionário correspondente, se existir.

O exemplo de uso das funções está logo abaixo da implementação. Nele, três funcionários são adicionados à tabela `funcionarios`, em seguida é feita a listagem dos funcionários, é removido o funcionário "Maria" e novamente é feita a listagem dos funcionários. Por fim, é feita uma busca pelo funcionário "João" e é impressa na tela uma mensagem informando se o funcionário foi encontrado ou não.

Espero que este código atenda às suas expectativas. Se você tiver alguma dúvida sobre o código ou precisar de mais informações, por favor, me avise!