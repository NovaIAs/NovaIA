Claro! Aqui está um exemplo de um código em Lua que calcula a soma de todos os números pares de 1 a 100 e exibe o resultado:

```lua
-- Função para verificar se um número é par
function ehPar(numero)
    return numero % 2 == 0
end

-- Variável para armazenar a soma dos números pares
local soma = 0

-- Loop de 1 a 100
for i = 1, 100 do
    if ehPar(i) then
        soma = soma + i
    end
end

-- Exibindo a soma dos números pares
print("A soma dos números pares de 1 a 100 é: " .. soma)
```

Neste código em Lua, começamos definindo uma função chamada `ehPar` que verifica se um número é par. Essa função retorna `true` se o número for par e `false` caso contrário.

Em seguida, criamos uma variável `soma` e a inicializamos com o valor 0. Essa variável será usada para armazenar a soma dos números pares.

Em seguida, usamos um loop `for` para percorrer os números de 1 a 100. Dentro do loop, verificamos se o número atual é par usando a função `ehPar`. Se for par, somamos o número à variável `soma`.

Após a conclusão do loop, exibimos a soma dos números pares usando a função `print`. O resultado será mostrado no console ou na saída do programa.

Espero que esse código em Lua atenda às suas expectativas!