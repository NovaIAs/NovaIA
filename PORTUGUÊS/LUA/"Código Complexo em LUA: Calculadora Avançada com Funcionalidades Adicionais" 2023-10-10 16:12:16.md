Claro! Aqui está um exemplo de código complexo em LUA que envolve a criação de uma calculadora avançada com funcionalidades adicionais:

```lua
-- Função para verificar se um número é primo
function isPrime(n)
    if n <= 1 then
        return false
    end
    for i = 2, math.sqrt(n) do
        if n % i == 0 then
            return false
        end
    end
    return true
end

-- Função para calcular o fatorial de um número
function factorial(n)
    if n <= 1 then
        return 1
    end
    return n * factorial(n - 1)
end

-- Função para calcular o valor absoluto de um número
function absoluteValue(n)
    if n < 0 then
        return -n
    end
    return n
end

-- Função para calcular a área de um círculo
function calculateCircleArea(radius)
    return math.pi * radius^2
end

-- Função para calcular a média de uma lista de números
function calculateAverage(...)
    local sum = 0
    local count = 0
    for i, num in ipairs{...} do
        sum = sum + num
        count = count + 1
    end
    return sum / count
end

-- Programa principal
print("Bem-vindo(a) à Calculadora Avançada!")
print("Escolha uma opção:")
print("1. Verificar se um número é primo")
print("2. Calcular o fatorial de um número")
print("3. Calcular o valor absoluto de um número")
print("4. Calcular a área de um círculo")
print("5. Calcular a média de uma lista de números")
local choice = tonumber(io.read())

if choice == 1 then
    print("Digite um número:")
    local number = tonumber(io.read())
    if isPrime(number) then
        print(number .. " é primo!")
    else
        print(number .. " não é primo!")
    end
elseif choice == 2 then
    print("Digite um número:")
    local number = tonumber(io.read())
    print("O fatorial de " .. number .. " é " .. factorial(number))
elseif choice == 3 then
    print("Digite um número:")
    local number = tonumber(io.read())
    print("O valor absoluto de " .. number .. " é " .. absoluteValue(number))
elseif choice == 4 then
    print("Digite o raio do círculo:")
    local radius = tonumber(io.read())
    print("A área do círculo é " .. calculateCircleArea(radius))
elseif choice == 5 then
    print("Digite uma lista de números separados por espaço:")
    local input = io.read()
    local numbers = {}
    for num in input:gmatch("%S+") do
        table.insert(numbers, tonumber(num))
    end
    print("A média dos números é " .. calculateAverage(table.unpack(numbers)))
else
    print("Opção inválida!")
end
```

Este código em LUA implementa uma calculadora avançada que oferece cinco funcionalidades diferentes: verificar se um número é primo, calcular o fatorial de um número, calcular o valor absoluto de um número, calcular a área de um círculo e calcular a média de uma lista de números.

Ao executar o programa, o usuário é apresentado com um menu de opções e é solicitado a escolher uma delas. Dependendo da escolha, o programa solicita os dados necessários e realiza o cálculo correspondente, exibindo o resultado na saída. O código utiliza funções auxiliares para implementar cada uma das funcionalidades.

É importante ressaltar que este é apenas um exemplo de código complexo em LUA e que existem inúmeras outras possibilidades de implementação, dependendo dos requisitos e necessidades do projeto.