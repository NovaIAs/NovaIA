Claro! Aqui está um código complexo em Groovy que realiza uma simulação de um jogo de RPG chamado "Batalha Épica". Neste jogo, existem vários personagens com habilidades diferentes que batalham entre si. O código cria uma classe para cada personagem e implementa várias funcionalidades, como atacar, defender, usar habilidades especiais e calcular o dano causado.

```groovy
class Personagem {
    String nome
    int pontosVida
    int pontosAtaque
    int pontosDefesa
    
    Personagem(String nome, int pontosVida, int pontosAtaque, int pontosDefesa) {
        this.nome = nome
        this.pontosVida = pontosVida
        this.pontosAtaque = pontosAtaque
        this.pontosDefesa = pontosDefesa
    }
    
    void atacar(Personagem alvo) {
        int dano = calcularDano(alvo)
        alvo.receberDano(dano)
        println("${nome} atacou ${alvo.nome} e causou ${dano} de dano!")
    }
    
    void receberDano(int dano) {
        int danoRecebido = dano - pontosDefesa
        if (danoRecebido > 0) {
            pontosVida -= danoRecebido
            println("${nome} recebeu ${danoRecebido} de dano!")
        } else {
            println("${nome} se defendeu do ataque!")
        }
    }
    
    int calcularDano(Personagem alvo) {
        return pontosAtaque - alvo.pontosDefesa
    }
    
    void usarHabilidadeEspecial(Personagem alvo) {
        // Lógica para habilidade especial
    }
}

class Guerreiro extends Personagem {
    Guerreiro(String nome) {
        super(nome, 100, 20, 10)
    }
    
    void usarHabilidadeEspecial(Personagem alvo) {
        int dano = calcularDano(alvo) * 2
        alvo.receberDano(dano)
        println("${nome} usou a habilidade especial e causou ${dano} de dano!")
    }
}

class Mago extends Personagem {
    Mago(String nome) {
        super(nome, 80, 30, 5)
    }
    
    void usarHabilidadeEspecial(Personagem alvo) {
        int cura = 20
        pontosVida += cura
        println("${nome} usou a habilidade especial e se curou em ${cura} pontos de vida!")
    }
}

// Criação dos personagens
def guerreiro = new Guerreiro("Conan")
def mago = new Mago("Merlin")

// Simulação de uma batalha entre o guerreiro e o mago
guerreiro.atacar(mago)
mago.usarHabilidadeEspecial(guerreiro)
guerreiro.atacar(mago)

// Saída do resultado da batalha
println("Pontos de vida do ${guerreiro.nome}: ${guerreiro.pontosVida}")
println("Pontos de vida do ${mago.nome}: ${mago.pontosVida}")
```

Neste código, criamos uma classe `Personagem` que serve como uma classe base para os personagens do jogo. Em seguida, criamos as subclasses `Guerreiro` e `Mago`, que herdam da classe `Personagem` e implementam habilidades especiais específicas.

No exemplo de simulação de batalha, o guerreiro "Conan" ataca o mago "Merlin" e causa dano de acordo com seus pontos de ataque e defesa. Em seguida, o mago usa sua habilidade especial para se curar um pouco, e o guerreiro ataca novamente.

No final, exibimos os pontos de vida restantes de cada personagem. Este código pode ser adaptado e expandido para incluir mais personagens, habilidades e funcionalidades adicionais.