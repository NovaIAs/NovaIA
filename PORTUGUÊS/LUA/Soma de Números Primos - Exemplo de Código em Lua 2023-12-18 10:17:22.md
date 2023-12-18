Claro! Abaixo está um exemplo de código complexo em Lua que realiza a soma de números primos de 1 até um determinado valor N:

```lua
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

-- Função para somar os números primos até N
function somarNumerosPrimos(N)
    local soma = 0
    
    for i = 2, N do
        if ehPrimo(i) then
            soma = soma + i
        end
    end
    
    return soma
end

-- Exemplo de uso
local valorN = 100
local resultado = somarNumerosPrimos(valorN)
print("A soma dos números primos até " .. valorN .. " é: " .. resultado)
```

Nesse código, temos duas funções principais: "ehPrimo" e "somarNumerosPrimos". 

A função "ehPrimo" recebe um número como parâmetro e verifica se ele é primo ou não, retornando true ou false, respectivamente. A função utiliza um loop para dividir o número por todos os valores de 2 até a raiz quadrada do número, verificando se há algum divisor. Se houver algum divisor, o número não é primo e a função retorna false. Caso contrário, o número é primo e a função retorna true.

A função "somarNumerosPrimos" recebe um valor N como parâmetro e retorna a soma de todos os números primos de 1 até N. Essa função utiliza um loop para percorrer todos os números de 2 até N e chama a função "ehPrimo" para verificar se cada número é primo. Se o número for primo, ele é adicionado à variável "soma". No final, a função retorna a soma total.

O código também inclui um exemplo de uso, onde definimos o valor de "valorN" como 100. Em seguida, chamamos a função "somarNumerosPrimos" passando esse valor como parâmetro e armazenamos o resultado na variável "resultado". Por fim, imprimimos a mensagem com o valor de N e o resultado da soma dos números primos até N usando a função "print".