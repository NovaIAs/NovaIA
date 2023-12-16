Claro! Aqui está um exemplo de código em Julia que utiliza programação orientada a objetos para criar uma simulação de um jogo de RPG:

```julia
# Definindo a classe Personagem
abstract type Personagem end

# Definindo a classe Jogador que herda de Personagem
mutable struct Jogador <: Personagem
    nome::String
    nivel::Int
    pontos_vida::Int
    pontos_ataque::Int
end

# Definindo a classe Inimigo que herda de Personagem
mutable struct Inimigo <: Personagem
    nome::String
    pontos_vida::Int
    pontos_ataque::Int
end

# Definindo a função para o jogador atacar um inimigo
function ataque_jogador(jogador::Jogador, inimigo::Inimigo)
    dano = jogador.pontos_ataque
    inimigo.pontos_vida -= dano
    println("O jogador ${jogador.nome} atacou ${inimigo.nome} causando $dano de dano.")
    if inimigo.pontos_vida <= 0
        println("O inimigo ${inimigo.nome} foi derrotado!")
    end
end

# Definindo a função para o inimigo atacar o jogador
function ataque_inimigo(inimigo::Inimigo, jogador::Jogador)
    dano = inimigo.pontos_ataque
    jogador.pontos_vida -= dano
    println("O inimigo ${inimigo.nome} atacou o jogador ${jogador.nome} causando $dano de dano.")
    if jogador.pontos_vida <= 0
        println("O jogador ${jogador.nome} foi derrotado!")
    end
end

# Criando instâncias do jogador e inimigo
jogador1 = Jogador("Herói", 1, 100, 10)
inimigo1 = Inimigo("Monstro", 50, 5)

# Realizando os ataques até que alguém seja derrotado
while jogador1.pontos_vida > 0 && inimigo1.pontos_vida > 0
    ataque_jogador(jogador1, inimigo1)
    if inimigo1.pontos_vida > 0
        ataque_inimigo(inimigo1, jogador1)
    end
end
```

Neste código, definimos duas classes, `Jogador` e `Inimigo`, que representam os personagens do jogo. O jogador e o inimigo têm atributos como nome, pontos de vida e pontos de ataque. Também implementamos duas funções, `ataque_jogador` e `ataque_inimigo`, que realizam os ataques entre o jogador e o inimigo.

No código principal, criamos uma instância do jogador e uma do inimigo, e em seguida, iniciamos um loop onde os ataques são realizados até que um dos personagens seja derrotado (ou seja, tenha pontos de vida igual ou menor que zero).

A cada ataque, exibimos uma mensagem informando qual personagem atacou e qual foi o dano causado. Quando um dos personagens é derrotado, exibimos uma mensagem informando a derrota.

Espero que este exemplo atenda às suas expectativas! Fique à vontade para personalizá-lo e adicionar mais funcionalidades ao jogo.