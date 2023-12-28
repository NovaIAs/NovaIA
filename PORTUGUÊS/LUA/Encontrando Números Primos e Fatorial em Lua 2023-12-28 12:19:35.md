Claro! Aqui está um exemplo de um código complexo em Lua que usa várias funcionalidades da linguagem:

```lua
-- Função para calcular o fatorial de um número
function fatorial(n)
  if n == 0 then
    return 1
  else
    return n * fatorial(n-1)
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

-- Função para encontrar os números primos no intervalo de 1 a n
function encontrarPrimos(n)
  local primos = {}

  for i = 1, n do
    if ehPrimo(i) then
      table.insert(primos, i)
    end
  end

  return primos
end

-- Função para exibir os números primos encontrados
function exibirPrimos(primos)
  print("Números primos encontrados:")
  for i, primo in ipairs(primos) do
    print(primo)
  end
end

-- Função principal
function main()
  print("Digite um número:")
  local numero = tonumber(io.read())

  if numero then
    local resultado = fatorial(numero)
    print("O fatorial de " .. numero .. " é " .. resultado)

    local primos = encontrarPrimos(numero)
    exibirPrimos(primos)
  else
    print("Número inválido!")
  end
end

-- Chamada da função principal
main()
```

Este código em Lua possui várias funcionalidades. Primeiro, temos a função `fatorial` que calcula o fatorial de um número usando recursão. Em seguida, temos a função `ehPrimo` que verifica se um número é primo. Ela utiliza um loop `for` para verificar se o número é divisível por algum outro número até a raiz quadrada dele.

A próxima função, `encontrarPrimos`, utiliza a função `ehPrimo` para encontrar todos os números primos no intervalo de 1 a `n`. Os números primos encontrados são armazenados em uma tabela.

Por fim, temos a função `exibirPrimos`, que exibe os números primos encontrados na tela. Ela utiliza um loop `for` para percorrer a tabela de primos e imprimir cada número.

A função `main` é a função principal do programa. Ela solicita ao usuário que digite um número, calcula o fatorial desse número usando a função `fatorial` e encontra os números primos no intervalo de 1 a `n` utilizando a função `encontrarPrimos`. Em seguida, os resultados são exibidos na tela utilizando a função `exibirPrimos`.

Para executar o código, basta rodar o script em um interpretador Lua. Ao executar, o programa solicitará que você digite um número. Em seguida, ele calculará o fatorial desse número e exibirá os números primos encontrados no intervalo de 1 a `n`.