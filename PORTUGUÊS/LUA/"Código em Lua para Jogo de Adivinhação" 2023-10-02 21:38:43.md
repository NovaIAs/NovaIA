Claro! Aqui está um código complexo em Lua que cria um jogo de adivinhação onde o jogador deve adivinhar um número entre 1 e 100:

```lua
-- Função para gerar um número aleatório
function gerarNumeroAleatorio(min, max)
    math.randomseed(os.time())
    return math.random(min, max)
end

-- Função para ler o palpite do jogador
function lerPalpite()
    io.write("Digite um número entre 1 e 100: ")
    return tonumber(io.read())
end

-- Função principal do jogo
function jogarAdivinhacao()
    local numeroAleatorio = gerarNumeroAleatorio(1, 100)
    local palpite = lerPalpite()
    local tentativas = 1

    while palpite ~= numeroAleatorio do
        if palpite < numeroAleatorio then
            print("Tente um número maior!")
        else
            print("Tente um número menor!")
        end

        palpite = lerPalpite()
        tentativas = tentativas + 1
    end

    print("Parabéns! Você acertou o número em " .. tentativas .. " tentativas.")
end

-- Chama a função principal para iniciar o jogo
jogarAdivinhacao()
```

Explicação do código:

1. A função `gerarNumeroAleatorio` é usada para gerar um número aleatório entre um valor mínimo e máximo. Ela utiliza a função `math.random` para gerar o número aleatório e `math.randomseed(os.time())` para garantir que os números sejam realmente aleatórios a cada vez que o programa é executado.

2. A função `lerPalpite` é usada para ler o palpite do jogador. Ela utiliza a função `io.write` para exibir uma mensagem solicitando o palpite e `io.read` para ler a entrada do usuário. Em seguida, a função `tonumber` é usada para converter a entrada do usuário em um número.

3. A função `jogarAdivinhacao` é a função principal do jogo. Ela começa gerando um número aleatório chamando a função `gerarNumeroAleatorio` e armazenando o resultado na variável `numeroAleatorio`. Em seguida, ela lê o palpite do jogador chamando a função `lerPalpite` e armazena o resultado na variável `palpite`. A variável `tentativas` é inicializada com o valor 1.

4. O jogo entra em um loop `while` que continua executando até que o palpite do jogador seja igual ao número aleatório. Dentro do loop, o programa verifica se o palpite é menor ou maior que o número aleatório e imprime uma mensagem correspondente.

5. Dentro do loop, o programa lê um novo palpite do jogador chamando a função `lerPalpite` e atualiza a variável `tentativas` incrementando 1.

6. Quando o jogador acerta o número, o programa imprime uma mensagem de parabéns juntamente com o número de tentativas que o jogador levou para acertar.

7. Por fim, a função `jogarAdivinhacao` é chamada para iniciar o jogo.