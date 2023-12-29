Claro! Aqui está um exemplo de um código complexo em Lua:

```lua
-- Essa função calcula o fatorial de um número
function fatorial(n)
  if n == 0 then
    return 1
  else
    return n * fatorial(n-1)
  end
end

-- Essa função verifica se um número é primo
function verificaPrimo(num)
  if num <= 1 then
    return false
  elseif num <= 3 then
    return true
  elseif num % 2 == 0 or num % 3 == 0 then
    return false
  end

  local i = 5
  while i * i <= num do
    if num % i == 0 or num % (i + 2) == 0 then
      return false
    end
    i = i + 6
  end

  return true
end

-- Essa função retorna o n-ésimo número de Fibonacci
function fibonacci(n)
  if n <= 0 then
    return 0
  elseif n == 1 then
    return 1
  else
    return fibonacci(n-1) + fibonacci(n-2)
  end
end

-- Exemplo de uso das funções
local numero = 10
print("O fatorial de ", numero, " é: ", fatorial(numero))

local numeroPrimo = 17
if verificaPrimo(numeroPrimo) then
  print(numeroPrimo, " é primo!")
else
  print(numeroPrimo, " não é primo.")
end

local n = 8
print("O ", n, "º número de Fibonacci é: ", fibonacci(n))
```

Neste código, há três funções diferentes: `fatorial`, `verificaPrimo` e `fibonacci`. 

A função `fatorial` recebe um número `n` e retorna o fatorial desse número, utilizando recursão. 

A função `verificaPrimo` recebe um número `num` e retorna `true` se o número for primo, ou `false` caso contrário. Essa função utiliza o algoritmo conhecido como Crivo de Eratóstenes para verificar se um número é primo.

A função `fibonacci` retorna o n-ésimo número da sequência de Fibonacci. Ela também utiliza recursão para calcular o número.

No final do código, é feito um exemplo de uso das funções, onde são mostrados o fatorial de um número, se um número é primo ou não, e o n-ésimo número de Fibonacci.

Espero que isso atenda às suas necessidades!