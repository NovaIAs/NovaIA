Claro! A seguir está um exemplo de código em Lua que utiliza recursão para encontrar o reverso de uma string:

```lua
-- Função para inverter uma string recursivamente
local function inverterString(str)
    -- Verificar se a string está vazia ou possui apenas um caractere
    if str == "" or #str == 1 then
        return str
    else
        -- Chamar a função novamente, passando a string sem o último caractere
        -- e concatenando-o com o resultado da função
        return inverterString(str:sub(2)) .. str:sub(1, 1)
    end
end

-- Função para ler uma string do usuário
local function lerString()
    print("Digite uma string:")
    return io.read()
end

-- Principal
local str = lerString()
local inverso = inverterString(str)

print("A string invertida é: " .. inverso)
```

Explicação do código:

1. Começamos definindo uma função chamada `inverterString` que recebe uma string como parâmetro.
2. Dentro dessa função, verificamos se a string está vazia ou possui apenas um caractere. Nesse caso, retornamos a própria string, pois não há necessidade de inverter.
3. Caso contrário, chamamos a função `inverterString` novamente, passando a string sem o último caractere (obtido através da função `sub(2)`) e concatenando-o com o primeiro caractere da string original (obtido através da função `sub(1, 1)`). Essa concatenação é realizada usando o operador de concatenação (`..`).
4. A função continua chamando a si mesma até que a condição de parada seja atingida, e então começa a retornar os resultados na ordem inversa da chamada original.
5. Em seguida, definimos uma função chamada `lerString`, que exibe uma mensagem para o usuário e lê uma string digitada pelo usuário usando a função `io.read()`.
6. No bloco principal, chamamos a função `lerString` para obter uma string do usuário e armazenamos o resultado em `str`.
7. Em seguida, chamamos a função `inverterString` passando a string digitada pelo usuário como parâmetro e armazenamos o resultado em `inverso`.
8. Por fim, exibimos a string invertida na saída padrão usando a função `print`.

Espero que este código tenha lhe dado uma visão do potencial da linguagem Lua e da recursão. Lembre-se de praticar escrevendo e modificando o código para melhorar suas habilidades de programação.