Certamente! Aqui está um exemplo de um código complexo em Lua:

```lua
-- Definindo uma função recursiva para calcular o fatorial de um número
function fatorial(n)
    if n == 0 then
        return 1
    else
        return n * fatorial(n - 1)
    end
end

-- Criando uma tabela para armazenar dados de funcionários
local funcionarios = {
    { nome = "João", salario = 2500 },
    { nome = "Maria", salario = 3000 },
    { nome = "Pedro", salario = 2000 },
    -- Adicione quantos funcionários desejar
}

-- Calculando o salário médio dos funcionários
local totalSalarios = 0
for i, funcionario in ipairs(funcionarios) do
    totalSalarios = totalSalarios + funcionario.salario
end
local salarioMedio = totalSalarios / #funcionarios

-- Imprimindo o resultado
print("O salário médio dos funcionários é: " .. salarioMedio)

-- Ordenando a tabela de funcionários por salário em ordem crescente
table.sort(funcionarios, function(a, b)
    return a.salario < b.salario
end)

-- Imprimindo a tabela ordenada
print("Funcionários em ordem crescente de salário:")
for i, funcionario in ipairs(funcionarios) do
    print(funcionario.nome .. ": " .. funcionario.salario)
end

-- Criando uma classe para representar um retângulo
local Retangulo = {}
Retangulo.__index = Retangulo

function Retangulo.new(largura, altura)
    local self = setmetatable({}, Retangulo)
    self.largura = largura
    self.altura = altura
    return self
end

function Retangulo:area()
    return self.largura * self.altura
end

-- Criando uma instância da classe Retangulo
local retangulo = Retangulo.new(5, 10)

-- Calculando e imprimindo a área do retângulo
print("A área do retângulo é: " .. retangulo:area())
```

Este código em Lua apresenta diversas funcionalidades e conceitos, incluindo:

1. Uma função recursiva para calcular o fatorial de um número.
2. O uso de uma tabela para armazenar dados de funcionários.
3. O cálculo do salário médio dos funcionários.
4. A ordenação da tabela de funcionários por salário em ordem crescente.
5. A criação de uma classe chamada "Retangulo" com métodos para calcular a área.
6. A criação de uma instância da classe "Retangulo" e cálculo da área.

Espero que este exemplo lhe forneça uma visão abrangente das possibilidades e complexidades que podem ser exploradas em Lua.