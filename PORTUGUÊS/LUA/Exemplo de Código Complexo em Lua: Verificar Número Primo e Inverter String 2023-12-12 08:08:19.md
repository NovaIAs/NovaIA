Claro! Aqui está um exemplo de um código complexo em Lua, que utiliza operações matemáticas, estruturas de controle de fluxo e manipulação de strings:

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

-- Função para inverter uma string
function inverterString(str)
    local resultado = ''
    
    for i = #str, 1, -1 do
        resultado = resultado .. string.sub(str, i, i)
    end
    
    return resultado
end

-- Função principal
function main()
    print("Digite um número:")
    local numero = tonumber(io.read())
    
    if ehPrimo(numero) then
        print(numero .. " é um número primo!")
    else
        print(numero .. " não é um número primo.")
    end
    
    print("Digite uma frase:")
    local frase = io.read()
    
    local fraseInvertida = inverterString(frase)
    print("Frase invertida: " .. fraseInvertida)
    
    local palavras = {}
    for palavra in frase:gmatch("%S+") do
        table.insert(palavras, palavra)
    end
    
    print("Palavras encontradas:")
    for i = 1, #palavras do
        print(palavras[i])
    end
    
    if #palavras > 0 then
        print("A última palavra é: " .. palavras[#palavras])
    end
end

-- Executa a função principal
main()
```

Neste código, a função `ehPrimo` verifica se um número é primo. Ela utiliza um loop `for` para verificar se o número é divisível por algum número menor que a sua raiz quadrada. Se for, o número não é primo.

A função `inverterString` recebe uma string e retorna a mesma string invertida. Ela utiliza um loop `for` para percorrer a string de trás para frente, concatenando os caracteres em uma nova string.

A função `main` é a função principal do programa. Ela solicita ao usuário um número e verifica se é primo. Em seguida, solicita uma frase e a inverte utilizando a função `inverterString`. Depois, é feita a separação das palavras da frase utilizando o padrão `%S+` e são exibidas na sequência. Por fim, é exibida a última palavra, caso exista.

Espero que este exemplo atenda às suas expectativas! Lembre-se de que você pode modificar, adaptar e expandir o código conforme necessário.