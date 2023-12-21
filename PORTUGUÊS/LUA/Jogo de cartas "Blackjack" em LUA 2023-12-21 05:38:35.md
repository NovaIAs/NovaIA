Claro! Aqui está um código complexo em LUA que utiliza conceitos avançados e é um programa de jogo de cartas chamado "Blackjack":

```lua
-- Função para criar um baralho com todas as cartas
function criarBaralho()
    local baralho = {}
    local naipes = {"Copas", "Espadas", "Ouros", "Paus"}
    local valores = {"Ás", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Valete", "Dama", "Rei"}
    
    for i = 1, #naipes do
        for j = 1, #valores do
            local carta = {valor = valores[j], naipe = naipes[i]}
            table.insert(baralho, carta)
        end
    end
    
    return baralho
end

-- Função para embaralhar o baralho
function embaralharBaralho(baralho)
    for i = 1, #baralho do
        local j = math.random(i, #baralho)
        baralho[i], baralho[j] = baralho[j], baralho[i]
    end
end

-- Função para calcular o valor de uma mão de cartas
function calcularValorMao(mao)
    local valorMao = 0
    local temAs = false
    
    for i = 1, #mao do
        local valorCarta = 0
        
        if mao[i].valor == "Ás" then
            valorCarta = 11
            temAs = true
        elseif mao[i].valor == "Valete" or mao[i].valor == "Dama" or mao[i].valor == "Rei" then
            valorCarta = 10
        else
            valorCarta = tonumber(mao[i].valor)
        end
        
        valorMao = valorMao + valorCarta
    end
    
    if temAs and valorMao > 21 then
        valorMao = valorMao - 10
    end
    
    return valorMao
end

-- Função para mostrar uma mão de cartas
function mostrarMao(mao)
    local descricaoMao = ""
    
    for i = 1, #mao do
        descricaoMao = descricaoMao .. mao[i].valor .. " de " .. mao[i].naipe .. ", "
    end
    
    return descricaoMao:sub(1, -3)
end

-- Função principal do jogo
function jogarBlackjack()
    local baralho = criarBaralho()
    embaralharBaralho(baralho)
    
    local maoJogador = {}
    local maoCroupier = {}
    
    table.insert(maoJogador, table.remove(baralho, 1))
    table.insert(maoCroupier, table.remove(baralho, 1))
    table.insert(maoJogador, table.remove(baralho, 1))
    table.insert(maoCroupier, table.remove(baralho, 1))
    
    print("Jogador: " .. mostrarMao(maoJogador))
    print("Croupier: " .. maoCroupier[1].valor .. " de " .. maoCroupier[1].naipe)
    
    while true do
        local opcao
        
        repeat
            print("Escolha uma opção:")
            print("1 - Pedir carta")
            print("2 - Parar")
            io.write("Opção: ")
            opcao = io.read()
        until opcao == "1" or opcao == "2"
        
        if opcao == "1" then
            table.insert(maoJogador, table.remove(baralho, 1))
            print("Jogador: " .. mostrarMao(maoJogador))
            
            if calcularValorMao(maoJogador) > 21 then
                print("Você estourou! Croupier venceu.")
                return
            end
        else
            break
        end
    end
    
    print("Croupier: " .. mostrarMao(maoCroupier))
    
    while calcularValorMao(maoCroupier) < 17 do
        table.insert(maoCroupier, table.remove(baralho, 1))
        print("Croupier: " .. mostrarMao(maoCroupier))
        
        if calcularValorMao(maoCroupier) > 21 then
            print("Croupier estourou! Jogador venceu.")
            return
        end
    end
    
    local valorMaoJogador = calcularValorMao(maoJogador)
    local valorMaoCroupier = calcularValorMao(maoCroupier)
    
    if valorMaoJogador > valorMaoCroupier then
        print("Jogador venceu!")
    elseif valorMaoJogador < valorMaoCroupier then
        print("Croupier venceu!")
    else
        print("Empate!")
    end
end

-- Início do jogo
jogarBlackjack()
```

Este código implementa todas as mecânicas do jogo "Blackjack". Ele começa definindo várias funções como `criarBaralho` para criar um baralho com todas as cartas, `embaralharBaralho` para embaralhar o baralho, `calcularValorMao` para calcular o valor de uma mão de cartas, `mostrarMao` para mostrar uma mão de cartas, e finalmente a função `jogarBlackjack` que implementa a lógica principal do jogo.

O jogo começa criando um baralho, embaralhando-o e distribuindo duas cartas para o jogador e para o croupier. Em seguida, mostra as cartas do jogador e apenas uma carta do croupier.

Depois, entra num loop onde o jogador pode escolher entre pedir uma nova carta ou parar. Se o jogador pedir uma nova carta, ela é adicionada à sua mão e é mostrada na tela. Se o valor das cartas do jogador ultrapassar 21, o jogador perde.

Quando o jogador decide parar, é a vez do croupier jogar. O croupier continua pedindo cartas até que o valor total de suas cartas seja igual ou maior que 17. Se o valor das cartas do croupier ultrapassar 21, o croupier perde.

Por fim, o programa compara os valores das mãos do jogador e do croupier para determinar o vencedor ou se houve empate.

Espero que esse código atenda às suas expectativas!