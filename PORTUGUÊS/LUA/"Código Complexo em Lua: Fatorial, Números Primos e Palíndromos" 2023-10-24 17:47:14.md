Claro! Aqui está um código complexo em Lua que realiza uma série de tarefas e é explicado em detalhes:

```lua
-- Função para calcular o fatorial de um número
function fatorial(n)
  if n == 0 then
    return 1
  else
    return n * fatorial(n - 1)
  end
end

-- Função para verificar se um número é primo
function isPrimo(num)
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

-- Função para verificar se uma string é um palíndromo
function isPalindromo(str)
  str = str:lower()
  str = str:gsub("%s+", "") -- Remover espaços em branco
  
  local reversedStr = string.reverse(str)
  
  if str == reversedStr then
    return true
  else
    return false
  end
end

-- Função principal que chama as outras funções e mostra os resultados
function main()
  local numero = 5
  local resultadoFatorial = fatorial(numero)
  
  print("O fatorial de " .. numero .. " é: " .. resultadoFatorial)
  
  local numeroPrimo = 7
  if isPrimo(numeroPrimo) then
    print(numeroPrimo .. " é um número primo")
  else
    print(numeroPrimo .. " não é um número primo")
  end
  
  local palavra = "Ovo"
  if isPalindromo(palavra) then
    print(palavra .. " é um palíndromo")
  else
    print(palavra .. " não é um palíndromo")
  end
end

-- Chamada da função principal
main()
```

Neste código, temos três funções principais:

1. A função `fatorial` calcula o fatorial de um número usando recursão.
2. A função `isPrimo` verifica se um número é primo ou não, utilizando o algoritmo de divisão iterativa.
3. A função `isPalindromo` verifica se uma string é um palíndromo, removendo espaços em branco e comparando com a versão invertida da string.

A função `main` é a função principal que chama as outras funções e mostra os resultados na saída. No exemplo, são realizadas as seguintes tarefas:

1. Calcula o fatorial do número 5 e exibe o resultado.
2. Verifica se o número 7 é primo e exibe o resultado.
3. Verifica se a palavra "Ovo" é um palíndromo e exibe o resultado.

Espero que isso tenha atendido à sua solicitação de um código complexo em Lua!