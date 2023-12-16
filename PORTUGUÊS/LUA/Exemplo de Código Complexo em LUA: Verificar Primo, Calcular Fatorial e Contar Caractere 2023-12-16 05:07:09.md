Claro! Aqui está um exemplo de um código complexo em LUA, que utiliza múltiplas funções e estruturas de controle para realizar uma tarefa específica:

```lua
-- Função para verificar se um número é primo
function verificarPrimo(numero)
  if numero <= 1 then
    return false
  elseif numero <= 3 then
    return true
  elseif numero % 2 == 0 or numero % 3 == 0 then
    return false
  end
  
  local i = 5
  while i * i <= numero do
    if numero % i == 0 or numero % (i + 2) == 0 then
      return false
    end
    i = i + 6
  end
  
  return true
end

-- Função para calcular o fatorial de um número
function calcularFatorial(numero)
  if numero == 0 then
    return 1
  else
    return numero * calcularFatorial(numero - 1)
  end
end

-- Função para contar quantas vezes um determinado caractere aparece em uma string
function contarCaractere(string, caractere)
  local count = 0
  for i = 1, #string do
    if string:sub(i, i) == caractere then
      count = count + 1
    end
  end
  return count
end

-- Função principal
function main()
  print("Digite um número:")
  local numero = tonumber(io.read())
  
  -- Verifica se o número é primo
  if verificarPrimo(numero) then
    print("O número é primo!")
  else
    print("O número não é primo!")
  end
  
  -- Calcula o fatorial do número
  local fatorial = calcularFatorial(numero)
  print("O fatorial do número é: " .. fatorial)
  
  -- Solicita uma string e um caractere ao usuário
  print("Digite uma string:")
  local string = io.read()
  print("Digite um caractere:")
  local caractere = io.read()
  
  -- Conta quantas vezes o caractere aparece na string
  local count = contarCaractere(string, caractere)
  print("O caractere '" .. caractere .. "' aparece " .. count .. " vezes na string.")
end

-- Chama a função principal
main()
```

Neste exemplo, há três funções diferentes: `verificarPrimo`, `calcularFatorial` e `contarCaractere`. 

A função `verificarPrimo` verifica se um número é primo, utilizando um algoritmo conhecido como Crivo de Eratóstenes. Ela retorna `true` se o número for primo e `false` caso contrário.

A função `calcularFatorial` calcula o fatorial de um número utilizando recursão. Ela retorna o valor do fatorial.

A função `contarCaractere` conta quantas vezes um determinado caractere aparece em uma string. Ela percorre cada caractere da string e incrementa um contador sempre que encontra o caractere desejado. Ela retorna a contagem resultante.

A função `main` é a função principal do código, responsável por interagir com o usuário. Ela solicita um número ao usuário e verifica se é primo, calcula o fatorial desse número e também solicita uma string e um caractere para contar quantas vezes o caractere aparece na string.

Por fim, a função `main` é chamada para iniciar a execução do código.

Espero que isso tenha atendido às suas expectativas! Lembre-se de que estou sempre disponível para gerar qualquer código de programação que você precisar.