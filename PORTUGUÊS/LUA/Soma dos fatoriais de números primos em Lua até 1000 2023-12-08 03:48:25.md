Claro! Aqui está um código complexo em Lua, explicando cada parte:

```lua
-- Função que retorna o fatorial de um número inteiro não negativo
local function fatorial(numero)
    -- Verifica se o número é zero ou um
    if numero == 0 or numero == 1 then 
        return 1
    else
        -- Calcula o fatorial recursivamente
        return numero * fatorial(numero - 1)
    end
end

-- Função que verifica se um número é primo
local function verificarPrimo(numero)
    -- Verifica se o número é menor ou igual a um
    if numero <= 1 then
        return false
    elseif numero == 2 then
        return true
    else
        -- Verifica se o número é divisível por algum número até a raiz quadrada dele
        for i = 2, math.sqrt(numero) do
            if numero % i == 0 then
                return false
            end
        end
        return true
    end
end

-- Função principal do programa
local function main()
    -- Variável que irá armazenar a soma dos fatoriais dos números primos
    local somaFatoriaisPrimos = 0

    -- Loop para percorrer os números de 1 a 1000
    for i = 1, 1000 do
        -- Verifica se o número é primo
        if verificarPrimo(i) then
            -- Calcula o fatorial do número
            local fatorialNumero = fatorial(i)
            
            -- Adiciona o fatorial do número à soma
            somaFatoriaisPrimos = somaFatoriaisPrimos + fatorialNumero
        end
    end

    -- Imprime a soma dos fatoriais dos números primos
    print("A soma dos fatoriais dos números primos de 1 a 1000 é: " .. somaFatoriaisPrimos)
end

-- Chama a função principal do programa
main()
```

Neste código, primeiramente criamos duas funções auxiliares, `fatorial` e `verificarPrimo`. A função `fatorial` recebe um número inteiro não negativo como parâmetro e retorna o fatorial desse número, calculado de forma recursiva.

A função `verificarPrimo` recebe um número como parâmetro e verifica se ele é primo. Caso o número seja menor ou igual a 1, a função retorna `false`. Se o número for igual a 2, retorna `true`. Caso contrário, a função verifica se o número é divisível por algum número entre 2 e a raiz quadrada do número. Se encontrar algum divisor, retorna `false`; caso contrário, retorna `true`.

Na função `main`, criamos uma variável `somaFatoriaisPrimos` para armazenar a soma dos fatoriais dos números primos de 1 a 1000. Em seguida, percorremos um loop de 1 a 1000 e verificamos se cada número é primo utilizando a função `verificarPrimo`. Se for primo, calculamos o fatorial do número utilizando a função `fatorial` e adicionamos esse valor à `somaFatoriaisPrimos`.

Por fim, imprimimos o resultado da soma utilizando a função `print`.

Esse código exemplifica a utilização de funções, recursividade e loops em Lua, além de demonstrar a aplicação dos conceitos de fatorial e números primos. Ele pode ser adaptado para tratar intervalos diferentes ou realizar outras operações utilizando essa mesma estrutura básica.