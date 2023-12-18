Claro! Vou gerar um código complexo em TypeScript para você. Este código é uma simulação de um jogo de RPG onde os jogadores podem criar e evoluir seus personagens. Vou explicar o código linha por linha para que você possa entender melhor o que está acontecendo.

```typescript
// Definição da classe Personagem
class Personagem {
    nome: string;
    nivel: number;
    pontosDeVida: number;
    
    constructor(nome: string) {
        this.nome = nome;
        this.nivel = 1;
        this.pontosDeVida = 100;
    }

    // Método para exibir as informações do personagem
    exibirInformacoes() {
        console.log(`Nome: ${this.nome}`);
        console.log(`Nível: ${this.nivel}`);
        console.log(`Pontos de Vida: ${this.pontosDeVida}`);
    }

    // Método para o personagem atacar um inimigo
    atacar(inimigo: Personagem) {
        console.log(`${this.nome} atacou ${inimigo.nome}!`);
        inimigo.receberDano(20);
    }

    // Método para o personagem receber dano
    receberDano(dano: number) {
        this.pontosDeVida -= dano;
        console.log(`${this.nome} recebeu ${dano} pontos de dano!`);
        if (this.pontosDeVida <= 0) {
            console.log(`${this.nome} foi derrotado!`);
        }
    }

    // Método para o personagem ganhar experiência e subir de nível
    ganharExperiencia(experiencia: number) {
        console.log(`${this.nome} ganhou ${experiencia} pontos de experiência!`);
        if (experiencia >= 100) {
            this.nivel++;
            console.log(`${this.nome} subiu para o nível ${this.nivel}!`);
            this.pontosDeVida += 50;
            console.log(`${this.nome} recuperou 50 pontos de vida!`);
        }
    }
}

// Criação dos personagens
const jogador1 = new Personagem("Herói");
const jogador2 = new Personagem("Vilão");

// Exibição das informações iniciais dos personagens
jogador1.exibirInformacoes();
jogador2.exibirInformacoes();

// Jogador1 ataca Jogador2
jogador1.atacar(jogador2);

// Exibição das informações atualizadas dos personagens após o ataque
jogador1.exibirInformacoes();
jogador2.exibirInformacoes();

// Jogador2 ganha experiência suficiente para subir de nível
jogador2.ganharExperiencia(150);

// Exibição das informações atualizadas dos personagens após ganhar experiência
jogador1.exibirInformacoes();
jogador2.exibirInformacoes();
```

Explicação do código:

1. Definimos a classe `Personagem` com as propriedades `nome`, `nivel` e `pontosDeVida`.
2. O construtor da classe inicializa as propriedades do personagem com valores padrão.
3. O método `exibirInformacoes` exibe as informações do personagem no console.
4. O método `atacar` recebe um outro personagem como parâmetro e o ataca, reduzindo seus pontos de vida.
5. O método `receberDano` recebe um valor de dano e reduz os pontos de vida do personagem.
6. O método `ganharExperiencia` recebe um valor de experiência e, se esse valor for maior ou igual a 100, o personagem sobe de nível, aumenta seus pontos de vida e exibe uma mensagem no console.
7. Criamos dois personagens, `jogador1` e `jogador2`, usando a classe `Personagem`.
8. Exibimos as informações iniciais dos personagens.
9. O `jogador1` ataca o `jogador2`.
10. Exibimos as informações atualizadas dos personagens após o ataque.
11. O `jogador2` ganha experiência suficiente para subir de nível.
12. Exibimos as informações atualizadas dos personagens após ganhar experiência.

Espero que esse código atenda às suas expectativas!