Claro! Aqui está um exemplo de um código complexo em LUA, que utiliza vários conceitos e recursos da linguagem:

```lua
-- Função para verificar se um número é primo
function isPrimo(num)
  if num <= 1 then
    return false
  end
  
  for i = 2, math.sqrt(num) do
    if num % i == 0 then
      return false
    end
  end
  
  return true
end

-- Função para gerar os números primos entre 1 e um número limite
function gerarPrimos(limite)
  local primos = {}
  
  for i = 1, limite do
    if isPrimo(i) then
      table.insert(primos, i)
    end
  end
  
  return primos
end

-- Função para calcular o fatorial de um número
function calcularFatorial(num)
  if num == 0 then
    return 1
  else
    return num * calcularFatorial(num - 1)
  end
end

-- Função para inverter uma string
function inverterString(str)
  local resultado = ""
  
  for i = #str, 1, -1 do
    resultado = resultado .. string.sub(str, i, i)
  end
  
  return resultado
end

-- Função para cifrar uma string utilizando a Cifra de César
function cifrarString(str, chave)
  local resultado = ""
  
  for i = 1, #str do
    local char = string.byte(str, i)
    
    if char >= 65 and char <= 90 then  -- Letra maiúscula
      char = (char - 65 + chave) % 26 + 65
    elseif char >= 97 and char <= 122 then  -- Letra minúscula
      char = (char - 97 + chave) % 26 + 97
    end
    
    resultado = resultado .. string.char(char)
  end
  
  return resultado
end

-- Exemplo de uso das funções
local limite = 100
local primos = gerarPrimos(limite)

print("Números primos entre 1 e " .. limite .. ":")
for i, primo in ipairs(primos) do
  print(primo)
end

local num = 5
print("Fatorial de " .. num .. ": " .. calcularFatorial(num))

local str = "lua"
print("String invertida: " .. inverterString(str))

local mensagem = "Olá, mundo!"
local chave = 3
local mensagemCifrada = cifrarString(mensagem, chave)
print("Mensagem cifrada: " .. mensagemCifrada)
```

Explicação do Código:
1. A função `isPrimo` recebe um número como parâmetro e verifica se ele é primo ou não. Ela utiliza um loop `for` para testar se o número é divisível por algum número entre 2 e a raiz quadrada do próprio número. Retorna `true` se for primo e `false` caso contrário.
2. A função `gerarPrimos` recebe um número limite como parâmetro e retorna uma tabela contendo todos os números primos entre 1 e esse limite. Utiliza a função `isPrimo` para verificar se cada número é primo ou não.
3. A função `calcularFatorial` recebe um número como parâmetro e retorna o fatorial desse número. Utiliza uma chamada recursiva para calcular o fatorial.
4. A função `inverterString` recebe uma string como parâmetro e retorna uma nova string contendo os caracteres da string original em ordem inversa. Utiliza um loop `for` para percorrer a string original de trás para frente e concatenar os caracteres na nova string.
5. A função `cifrarString` recebe uma string e uma chave como parâmetros, e retorna uma nova string cifrada utilizando a Cifra de César. Percorre cada caractere da string original, desloca o seu valor ASCII pela chave fornecida, e então converte o novo valor ASCII de volta para caractere, concatenando-o na nova string. Ela suporta tanto letras maiúsculas quanto minúsculas.
6. O código de exemplo utiliza as funções anteriores para gerar os números primos entre 1 e um limite fornecido, calcular o fatorial de um número, inverter uma string e cifrar uma mensagem utilizando a Cifra de César. Os resultados são impressos no console.

Espero que este código atenda às suas expectativas!