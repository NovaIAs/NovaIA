Claro! Aqui está um código complexo em Lua:

```lua
-- Função para calcular o fatorial de um número
function fatorial(numero)
  if numero <= 1 then
    return 1
  else
    return numero * fatorial(numero - 1)
  end
end

-- Função para verificar se um número é primo
function ehPrimo(numero)
  if numero <= 1 then
    return false
  end
  
  for i = 2, math.sqrt(numero) do
    if numero % i == 0 then
      return false
    end
  end
  
  return true
end

-- Função para imprimir os números primos e seus fatoriais
function imprimirNumerosPrimosEFatoriais(limite)
  for i = 2, limite do
    if ehPrimo(i) then
      print("Número primo:", i)
      print("Fatorial:", fatorial(i))
      print("------------------------")
    end
  end
end

-- Definindo o limite para encontrar os números primos e seus fatoriais
local limite = 20

-- Chamando a função para imprimir os números primos e seus fatoriais
imprimirNumerosPrimosEFatoriais(limite)
```

Nesse código, temos algumas funções e um programa principal. A primeira função é o `fatorial`, que recebe um número como parâmetro e retorna seu fatorial. Essa função utiliza a recursividade para calcular o fatorial.

A segunda função é o `ehPrimo`, que verifica se um número é primo ou não. Ela utiliza um loop para testar se o número é divisível por algum número entre 2 e a raiz quadrada do próprio número. Se for divisível, significa que não é primo.

A terceira função é o `imprimirNumerosPrimosEFatoriais`, que recebe um limite como parâmetro. Essa função percorre os números de 2 até o limite e verifica se cada número é primo. Se for, imprime o número primo e seu fatorial.

No programa principal, definimos o limite como 20 e chamamos a função `imprimirNumerosPrimosEFatoriais` passando esse limite. Assim, serão impressos na tela os números primos de 2 a 20 e seus respectivos fatoriais.

Esse código demonstra o uso de funções, loops, condicionais e chamadas de função. Ele é capaz de calcular os fatoriais de números primos até um determinado limite, fornecendo um exemplo de uso desses conceitos em Lua.