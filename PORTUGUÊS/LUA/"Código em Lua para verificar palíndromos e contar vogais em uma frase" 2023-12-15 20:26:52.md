Claro! Aqui está um código complexo em Lua que envolve o uso de funções, manipulação de strings e estruturas de controle:

```lua
-- Função para verificar se uma palavra é um palíndromo
function verificarPalindromo(palavra)
    -- Remove os espaços em branco da palavra
    palavra = string.gsub(palavra, "%s+", "")
    
    -- Converte a palavra para minúsculas
    palavra = string.lower(palavra)
    
    -- Inverte a palavra
    local palavraInvertida = ""
    for i = string.len(palavra), 1, -1 do
        palavraInvertida = palavraInvertida .. string.sub(palavra, i, i)
    end
    
    -- Verifica se a palavra é um palíndromo
    if palavra == palavraInvertida then
        return true
    else
        return false
    end
end

-- Função para contar o número de vogais em uma frase
function contarVogais(frase)
    -- Converte a frase para minúsculas
    frase = string.lower(frase)
    
    -- Define as vogais
    local vogais = {["a"] = true, ["e"] = true, ["i"] = true, ["o"] = true, ["u"] = true}
    
    -- Inicializa o contador de vogais
    local contador = 0
    
    -- Percorre a frase contando as vogais
    for i = 1, string.len(frase) do
        local letra = string.sub(frase, i, i)
        if vogais[letra] then
            contador = contador + 1
        end
    end
    
    -- Retorna o número de vogais
    return contador
end

-- Função principal
function main()
    print("Digite uma palavra:")
    local palavra = io.read()  -- Lê uma palavra digitada pelo usuário
    
    if verificarPalindromo(palavra) then
        print("A palavra é um palíndromo!")
    else
        print("A palavra não é um palíndromo.")
    end
    
    print("Digite uma frase:")
    local frase = io.read()  -- Lê uma frase digitada pelo usuário
    
    local numVogais = contarVogais(frase)
    print("A frase contém " .. numVogais .. " vogais.")
end

-- Chama a função principal
main()
```

Neste código, temos três funções: `verificarPalindromo`, `contarVogais` e `main`. A função `verificarPalindromo` recebe uma palavra como parâmetro e verifica se ela é um palíndromo, ou seja, se pode ser lida da mesma forma da esquerda para a direita e vice-versa. A função remove os espaços em branco da palavra, converte-a para minúsculas e inverte-a para comparar com a palavra original.

A função `contarVogais` recebe uma frase como parâmetro e conta o número de vogais presentes na frase. A função converte a frase para minúsculas e utiliza uma tabela `vogais` para verificar se cada caractere é uma vogal. A cada vogal encontrada, o contador é incrementado.

A função `main` é a função principal do programa. Ela solicita ao usuário que digite uma palavra e uma frase, chama as funções `verificarPalindromo` e `contarVogais` e exibe o resultado na tela.

Espero que este código atenda às suas expectativas!