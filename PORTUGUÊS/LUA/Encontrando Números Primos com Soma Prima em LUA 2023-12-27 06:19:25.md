Claro! Aqui está um exemplo de código complexo em LUA:

```lua
-- Função para verificar se um número é primo
function ehPrimo(numero)
    if numero < 2 then
        return false
    end

    for i = 2, math.sqrt(numero) do
        if numero % i == 0 then
            return false
        end
    end

    return true
end

-- Função para calcular a soma dos dígitos de um número
function somaDigitos(numero)
    local soma = 0
    while numero > 0 do
        soma = soma + (numero % 10)
        numero = math.floor(numero / 10)
    end
    return soma
end

-- Função para encontrar números primos cuja soma dos dígitos também é primo
function encontrarNumerosPrimosComSomaPrima(inicio, fim)
    local numerosPrimosComSomaPrima = {}
    for i = inicio, fim do
        if ehPrimo(i) and ehPrimo(somaDigitos(i)) then
            table.insert(numerosPrimosComSomaPrima, i)
        end
    end
    return numerosPrimosComSomaPrima
end

-- Chamada da função principal
local inicio = 1
local fim = 1000
local numerosPrimosComSomaPrima = encontrarNumerosPrimosComSomaPrima(inicio, fim)

-- Exibição dos resultados
print("Números primos cuja soma dos dígitos também é primo:")
for i, numero in ipairs(numerosPrimosComSomaPrima) do
    print(i, numero)
end
```

Neste código, temos a definição de três funções: `ehPrimo`, `somaDigitos` e `encontrarNumerosPrimosComSomaPrima`. 

A função `ehPrimo` verifica se um número é primo, retornando `true` se for e `false` caso contrário. Ela utiliza um laço de repetição `for` para iterar de 2 até a raiz quadrada do número, verificando se ele é divisível por algum dos valores no intervalo. 

A função `somaDigitos` calcula a soma dos dígitos de um número. Ela utiliza um laço de repetição `while` para extrair cada dígito do número, somando-os à variável `soma`.

A função `encontrarNumerosPrimosComSomaPrima` recebe dois parâmetros, `inicio` e `fim`, que definem o intervalo de números a serem verificados. Ela utiliza as funções anteriores para encontrar números primos cuja soma dos dígitos também é primo. Os números encontrados são armazenados em uma tabela e retornados no final da função.

Por fim, temos a chamada da função `encontrarNumerosPrimosComSomaPrima` com os valores `inicio = 1` e `fim = 1000`. Os números primos encontrados são exibidos no console.