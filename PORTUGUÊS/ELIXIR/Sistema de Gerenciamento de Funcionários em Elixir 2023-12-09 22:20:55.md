Claro, vou gerar um código complexo em Elixir para você. Este código é um exemplo de um sistema de gerenciamento de funcionários em uma empresa.

```elixir
defmodule Empresa do
  defstruct nome: "", funcionarios: %{}

  def novo_funcionario(empresa, nome, cargo, salario) do
    funcionario = %Funcionario{nome: nome, cargo: cargo, salario: salario}
    funcionarios = Map.put(empresa.funcionarios, nome, funcionario)
    %Empresa{empresa | funcionarios: funcionarios}
  end

  def remover_funcionario(empresa, nome) do
    funcionarios = Map.delete(empresa.funcionarios, nome)
    %Empresa{empresa | funcionarios: funcionarios}
  end

  def alterar_cargo(empresa, nome, novo_cargo) do
    funcionario = Map.get(empresa.funcionarios, nome)
    funcionario = %{funcionario | cargo: novo_cargo}
    funcionarios = Map.put(empresa.funcionarios, nome, funcionario)
    %Empresa{empresa | funcionarios: funcionarios}
  end

  def aumentar_salario(empresa, nome, aumento) do
    funcionario = Map.get(empresa.funcionarios, nome)
    novo_salario = funcionario.salario + aumento
    funcionario = %{funcionario | salario: novo_salario}
    funcionarios = Map.put(empresa.funcionarios, nome, funcionario)
    %Empresa{empresa | funcionarios: funcionarios}
  end

  def listar_funcionarios(empresa) do
    for {nome, funcionario} <- empresa.funcionarios do
      IO.puts("#{nome} - #{funcionario.cargo}: R$ #{funcionario.salario}")
    end
  end
end

defmodule Funcionario do
  defstruct nome: "", cargo: "", salario: 0.0
end

# Exemplo de uso do sistema

empresa = %Empresa{nome: "Minha Empresa"}

empresa = Empresa.novo_funcionario(empresa, "João", "Desenvolvedor", 3000.0)
empresa = Empresa.novo_funcionario(empresa, "Maria", "Analista", 4000.0)
empresa = Empresa.novo_funcionario(empresa, "Pedro", "Gerente", 6000.0)

empresa = Empresa.aumentar_salario(empresa, "João", 500.0)
empresa = Empresa.alterar_cargo(empresa, "Maria", "Senior")
empresa = Empresa.remover_funcionario(empresa, "Pedro")

Empresa.listar_funcionarios(empresa)
```

Este código em Elixir implementa um sistema de gerenciamento de funcionários em uma empresa. A estrutura principal é definida no módulo `Empresa`, que tem os campos `nome` e `funcionarios`. O campo `funcionarios` é um mapa que armazena os funcionários da empresa, onde a chave é o nome do funcionário e o valor é uma struct `Funcionario`, que contém os campos `nome`, `cargo` e `salario`.

O código possui funções para realizar as seguintes operações:

- `novo_funcionario(empresa, nome, cargo, salario)`: adiciona um novo funcionário à empresa.
- `remover_funcionario(empresa, nome)`: remove um funcionário da empresa.
- `alterar_cargo(empresa, nome, novo_cargo)`: altera o cargo de um funcionário.
- `aumentar_salario(empresa, nome, aumento)`: aumenta o salário de um funcionário.
- `listar_funcionarios(empresa)`: lista todos os funcionários da empresa, exibindo seu nome, cargo e salário.

No exemplo de uso do sistema, é criada uma empresa chamada "Minha Empresa" e são realizadas algumas operações, como adicionar funcionários, alterar cargos, aumentar salários e remover funcionários. Por fim, é listada a lista atualizada de funcionários da empresa.

Espero que este código atenda às suas expectativas! Se precisar de mais ajuda ou de algum código adicional, é só pedir.