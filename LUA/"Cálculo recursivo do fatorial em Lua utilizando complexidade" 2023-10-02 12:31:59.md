Claro! Aqui está um exemplo de código complexo em Lua, que utiliza recursividade para calcular o fatorial de um número:

```lua
-- Função recursiva para calcular o fatorial
function fatorial(n)
  if n == 0 then
    return 1
  else
    return n * fatorial(n-1)
  end
end

-- Função principal
function main()
  print("Digite um número inteiro positivo:")
  local numero = tonumber(io.read())

  if numero ~= nil and numero >= 0 and math.floor(numero) == numero then
    local resultado = fatorial(numero)
    print("O fatorial de " .. numero .. " é " .. resultado)
  else
    print("Entrada inválida. Por favor, digite um número inteiro positivo.")
  end
end

-- Chamada da função principal
main()
```

Neste código, a função `fatorial` é definida de forma recursiva, onde o caso base é quando `n` é igual a zero, retornando 1. Caso contrário, a função chama a si mesma com `n-1` e multiplica o resultado por `n`. A função principal `main` solicita ao usuário que digite um número inteiro positivo, verifica se a entrada é válida e, em seguida, chama a função `fatorial` para calcular o fatorial do número digitado. O resultado é exibido no console.