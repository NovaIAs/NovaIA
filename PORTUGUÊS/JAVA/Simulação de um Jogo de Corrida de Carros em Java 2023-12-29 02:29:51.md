Claro! Aqui está um exemplo de código em Java que realiza uma simulação de um jogo de corrida de carros:

```java
import java.util.Random;

public class JogoCorrida {
    public static void main(String[] args) {
        Carro carro1 = new Carro("Carro A", 120);
        Carro carro2 = new Carro("Carro B", 110);
        
        pistaDeCorrida(carro1, carro2);
        
        if (carro1.getPosicao() > carro2.getPosicao()) {
            System.out.println("O " + carro1.getNome() + " venceu a corrida!");
        } else if (carro2.getPosicao() > carro1.getPosicao()) {
            System.out.println("O " + carro2.getNome() + " venceu a corrida!");
        } else {
            System.out.println("A corrida terminou em empate!");
        }
    }
    
    public static void pistaDeCorrida(Carro carro1, Carro carro2) {
        int distanciaTotal = 1000;
        
        while (carro1.getPosicao() < distanciaTotal && carro2.getPosicao() < distanciaTotal) {
            carro1.acelerar();
            carro2.acelerar();
            
            Random random = new Random();
            int obstaculo = random.nextInt(100) + 1;
            
            if (obstaculo <= 10) {
                System.out.println("Houve um obstáculo na pista!");
                int penalidade = random.nextInt(100) + 1;
                
                if (penalidade <= 50) {
                    carro1.frear();
                    System.out.println(carro1.getNome() + " freou devido ao obstáculo.");
                } else {
                    carro2.frear();
                    System.out.println(carro2.getNome() + " freou devido ao obstáculo.");
                }
            }
            
            System.out.println("Posição do " + carro1.getNome() + ": " + carro1.getPosicao());
            System.out.println("Posição do " + carro2.getNome() + ": " + carro2.getPosicao());
            System.out.println("----------------------------------------------");
        }
    }
}

class Carro {
    private String nome;
    private int velocidadeMaxima;
    private int posicao;
    
    public Carro(String nome, int velocidadeMaxima) {
        this.nome = nome;
        this.velocidadeMaxima = velocidadeMaxima;
        this.posicao = 0;
    }
    
    public String getNome() {
        return nome;
    }
    
    public int getPosicao() {
        return posicao;
    }
    
    public void acelerar() {
        Random random = new Random();
        int velocidade = random.nextInt(velocidadeMaxima) + 1;
        posicao += velocidade;
    }
    
    public void frear() {
        posicao -= 10;
        if (posicao < 0) {
            posicao = 0;
        }
    }
}
```

Neste código, temos a classe `JogoCorrida` que contém o método `main` para iniciar o jogo. É criado um objeto para representar cada carro e, em seguida, é chamado o método `pistaDeCorrida` para simular a corrida.

No método `pistaDeCorrida`, é definida uma distância total de 1000 unidades, e um loop é executado até que um dos carros alcance essa distância. A cada iteração, os carros aceleram, e é gerado um número aleatório para representar a possibilidade de um obstáculo na pista. Caso haja um obstáculo, um dos carros é escolhido aleatoriamente para frear.

Após cada iteração do loop, a posição atual dos carros é exibida, permitindo acompanhar o progresso da corrida. Ao final, é verificado qual carro chegou mais longe e exibida a mensagem correspondente.

A classe `Carro` representa cada veículo do jogo e possui atributos como nome, velocidade máxima e posição atual. Os métodos `acelerar` e `frear` são responsáveis por atualizar a posição do carro de acordo com sua velocidade e frear em caso de obstáculo, respectivamente.