Claro! Vou criar um código complexo em Lua para você. 

```
-- Função recursiva para calcular o fatorial de um número
function fatorial(n)
  if n <= 1 then
    return 1
  else
    return n * fatorial(n - 1)
  end
end

-- Função para verificar se um número é primo
function ehPrimo(n)
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

-- Função para calcular o número de Fibonacci de um índice dado
function fibonacci(n)
  if n == 0 then
    return 0
  elseif n == 1 then
    return 1
  else
    return fibonacci(n - 1) + fibonacci(n - 2)
  end
end

print("Digite um número para calcular seu fatorial:")
local num = io.read("*n")
print("O fatorial de " .. num .. " é " .. fatorial(num))

print("Digite um número para verificar se é primo:")
num = io.read("*n")
if ehPrimo(num) then
  print(num .. " é primo")
else
  print(num .. " não é primo")
end

print("Digite um índice para calcular o número de Fibonacci:")
local indice = io.read("*n")
print("O número de Fibonacci de índice " .. indice .. " é " .. fibonacci(indice))
```

Esse código em Lua possui três funções: `fatorial`, `ehPrimo` e `fibonacci`. 

A função `fatorial` é uma função recursiva que calcula o fatorial de um número dado. Ela utiliza uma estrutura de controle `if-else` para determinar se o número é menor ou igual a 1. Caso seja, retorna 1. Caso contrário, calcula o fatorial multiplicando o número pela chamada recursiva da função com o número decrementado em 1.

A função `ehPrimo` verifica se um número dado é primo. Ela utiliza uma estrutura de controle `if` para verificar se o número é menor ou igual a 1. Caso seja, retorna `false`. Caso contrário, itera de 2 até a raiz quadrada do número e verifica se algum deles divide o número sem deixar resto. Se encontrar algum divisor, retorna `false`. Caso contrário, retorna `true`.

A função `fibonacci` calcula o número de Fibonacci de um índice dado. Utiliza uma estrutura de controle `if-elseif-else` para verificar se o índice é 0 ou 1, retornando 0 ou 1, respectivamente. Caso contrário, calcula o número de Fibonacci somando as chamadas recursivas com os índices decrementados em 1 e 2.

Após a definição das funções, o código solicita ao usuário três números diferentes para realizar os cálculos. Em seguida, exibe os resultados na tela.