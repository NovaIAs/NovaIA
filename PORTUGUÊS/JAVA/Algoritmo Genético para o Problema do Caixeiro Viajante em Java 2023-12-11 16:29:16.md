Claro! Aqui está um código em Java que implementa um algoritmo genético para resolver o problema do caixeiro viajante. O caixeiro viajante é um problema clássico de otimização, no qual o objetivo é encontrar o caminho mais curto que um vendedor ambulante deve percorrer, passando por todas as cidades uma única vez.

```java
import java.util.ArrayList;
import java.util.Random;

public class AlgoritmoGenetico {

    private int numCidades;
    private ArrayList<Cromossomo> populacao;
    private int tamanhoPopulacao;
    private int geracoes;
    private double taxaCrossover;
    private double taxaMutacao;

    public AlgoritmoGenetico(int numCidades, int tamanhoPopulacao, int geracoes, double taxaCrossover, double taxaMutacao) {
        this.numCidades = numCidades;
        this.tamanhoPopulacao = tamanhoPopulacao;
        this.geracoes = geracoes;
        this.taxaCrossover = taxaCrossover;
        this.taxaMutacao = taxaMutacao;
    }

    public void executar() {
        // Inicializa a população inicial
        populacao = new ArrayList<>();
        for (int i = 0; i < tamanhoPopulacao; i++) {
            populacao.add(new Cromossomo(numCidades));
        }

        // Executa as gerações
        for (int i = 0; i < geracoes; i++) {
            // Avalia a aptidão de cada cromossomo
            avaliarAptidao();

            // Seleciona os pais para crossover
            ArrayList<Cromossomo> pais = selecao();

            // Realiza o crossover para gerar a nova geração
            ArrayList<Cromossomo> novaGeracao = crossover(pais);

            // Realiza a mutação na nova geração
            mutacao(novaGeracao);

            // Substitui a população antiga pela nova geração
            populacao = novaGeracao;
        }

        // Avalia a aptidão final
        avaliarAptidao();

        // Obtém o melhor caminho da última geração
        Cromossomo melhorCromossomo = obterMelhorCromossomo();
        int[] melhorCaminho = melhorCromossomo.getCaminho();

        // Imprime o melhor caminho e sua aptidão
        System.out.println("Melhor caminho encontrado: ");
        for (int cidade : melhorCaminho) {
            System.out.print(cidade + " ");
        }
        System.out.println();
        System.out.println("Aptidão: " + melhorCromossomo.getAptidao());
    }

    private void avaliarAptidao() {
        for (Cromossomo cromossomo : populacao) {
            cromossomo.calcularAptidao();
        }
    }

    private ArrayList<Cromossomo> selecao() {
        ArrayList<Cromossomo> pais = new ArrayList<>();

        // Seleciona os pais por torneio
        for (int i = 0; i < tamanhoPopulacao; i++) {
            Cromossomo pai1 = torneio();
            Cromossomo pai2 = torneio();

            pais.add(pai1);
            pais.add(pai2);
        }

        return pais;
    }

    private Cromossomo torneio() {
        Random random = new Random();
        int indice1 = random.nextInt(tamanhoPopulacao);
        int indice2 = random.nextInt(tamanhoPopulacao);

        Cromossomo cromossomo1 = populacao.get(indice1);
        Cromossomo cromossomo2 = populacao.get(indice2);

        if (cromossomo1.getAptidao() < cromossomo2.getAptidao()) {
            return cromossomo1;
        } else {
            return cromossomo2;
        }
    }

    private ArrayList<Cromossomo> crossover(ArrayList<Cromossomo> pais) {
        ArrayList<Cromossomo> novaGeracao = new ArrayList<>();

        for (int i = 0; i < tamanhoPopulacao / 2; i++) {
            Cromossomo pai1 = pais.get(i * 2);
            Cromossomo pai2 = pais.get(i * 2 + 1);

            // Realiza o crossover com base na taxa de crossover
            if (Math.random() < taxaCrossover) {
                int pontoCorte = (int) (Math.random() * numCidades);

                int[] filho1 = new int[numCidades];
                int[] filho2 = new int[numCidades];

                for (int j = 0; j < pontoCorte; j++) {
                    filho1[j] = pai1.getCaminho()[j];
                    filho2[j] = pai2.getCaminho()[j];
                }

                for (int j = pontoCorte; j < numCidades; j++) {
                    filho1[j] = pai2.getCaminho()[j];
                    filho2[j] = pai1.getCaminho()[j];
                }

                novaGeracao.add(new Cromossomo(filho1));
                novaGeracao.add(new Cromossomo(filho2));
            } else {
                novaGeracao.add(pai1);
                novaGeracao.add(pai2);
            }
        }

        return novaGeracao;
    }

    private void mutacao(ArrayList<Cromossomo> novaGeracao) {
        for (Cromossomo cromossomo : novaGeracao) {
            // Realiza a mutação com base na taxa de mutação
            if (Math.random() < taxaMutacao) {
                int indice1 = (int) (Math.random() * numCidades);
                int indice2 = (int) (Math.random() * numCidades);

                int temp = cromossomo.getCaminho()[indice1];
                cromossomo.getCaminho()[indice1] = cromossomo.getCaminho()[indice2];
                cromossomo.getCaminho()[indice2] = temp;
            }
        }
    }

    private Cromossomo obterMelhorCromossomo() {
        Cromossomo melhorCromossomo = populacao.get(0);

        for (Cromossomo cromossomo : populacao) {
            if (cromossomo.getAptidao() < melhorCromossomo.getAptidao()) {
                melhorCromossomo = cromossomo;
            }
        }

        return melhorCromossomo;
    }

    public static void main(String[] args) {
        int numCidades = 10;
        int tamanhoPopulacao = 50;
        int geracoes = 100;
        double taxaCrossover = 0.8;
        double taxaMutacao = 0.2;

        AlgoritmoGenetico ag = new AlgoritmoGenetico(numCidades, tamanhoPopulacao, geracoes, taxaCrossover, taxaMutacao);
        ag.executar();
    }
}

class Cromossomo {
    private int numCidades;
    private int[] caminho;
    private double aptidao;

    public Cromossomo(int numCidades) {
        this.numCidades = numCidades;
        this.caminho = gerarCaminhoAleatorio();
        this.aptidao = 0;
    }

    public Cromossomo(int[] caminho) {
        this.numCidades = caminho.length;
        this.caminho = caminho;
        this.aptidao = 0;
    }

    public int[] getCaminho() {
        return caminho;
    }

    public double getAptidao() {
        return aptidao;
    }

    public void calcularAptidao() {
        // Calcula a distância total percorrida no caminho
        double distanciaTotal = 0;
        for (int i = 0; i < numCidades - 1; i++) {
            int cidadeAtual = caminho[i];
            int proximaCidade = caminho[i + 1];

            distanciaTotal += calcularDistancia(cidadeAtual, proximaCidade);
        }

        // Calcula a distância do último para o primeiro ponto
        distanciaTotal += calcularDistancia(caminho[numCidades - 1], caminho[0]);

        // Calcula a aptidão como o inverso da distância total
        aptidao = 1.0 / distanciaTotal;
    }

    private int[] gerarCaminhoAleatorio() {
        int[] caminhoAleatorio = new int[numCidades];

        for (int i = 0; i < numCidades; i++) {
            caminhoAleatorio[i] = i;
        }

        Random random = new Random();
        for (int i = numCidades - 1; i > 0; i--) {
            int j = random.nextInt(i + 1);

            int temp = caminhoAleatorio[i];
            caminhoAleatorio[i] = caminhoAleatorio[j];
            caminhoAleatorio[j] = temp;
        }

        return caminhoAleatorio;
    }

    private double calcularDistancia(int cidade1, int cidade2) {
        // Implemente aqui o cálculo da distância entre duas cidades
        // (pode ser utilizado o Teorema de Pitágoras para cidades com coordenadas no plano)
        return 0;
    }
}
```

Este código implementa um algoritmo genético para resolver o problema do caixeiro viajante em Java. O algoritmo começa gerando uma população inicial de cromossomos, onde cada cromossomo representa um possível caminho que o caixeiro viajante pode percorrer. A aptidão de cada cromossomo é calculada como a inversa da distância total percorrida no caminho.

O algoritmo então executa várias gerações, selecionando os pais para crossover com base em um torneio probabilístico, realizando o crossover entre os pais selecionados, aplicando uma taxa de mutação nos filhos resultantes e substituindo a população antiga pela nova geração. Após o término das gerações, o algoritmo retorna o melhor caminho encontrado e sua aptidão.

É importante notar que, neste código, falta a implementação do método `calcularDistancia`, que calcula a distância entre duas cidades. Este método deve ser implementado de acordo com o contexto do problema do caixeiro viajante que você deseja resolver.