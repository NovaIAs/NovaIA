Claro! Aqui está um código complexo em Java que envolve a resolução de um problema matemático conhecido como "Problema do Caixeiro Viajante". Neste problema, um caixeiro precisa visitar várias cidades, retornando à cidade inicial, percorrendo a menor distância possível.

```java
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ProblemaCaixeiroViajante {
    private int[][] matrizDistancias;
    private int numeroCidades;
    private List<Integer> melhorCaminho;

    public ProblemaCaixeiroViajante(int[][] matrizDistancias) {
        this.matrizDistancias = matrizDistancias;
        this.numeroCidades = matrizDistancias.length;
        this.melhorCaminho = new ArrayList<>();
    }

    public List<Integer> encontrarMelhorCaminho() {
        List<Integer> cidadesVisitadas = new ArrayList<>();
        for (int i = 0; i < numeroCidades; i++) {
            cidadesVisitadas.add(i);
        }
        melhorCaminho.add(0);
        encontrarCaminhoRecursivamente(cidadesVisitadas, 0, 0);
        return melhorCaminho;
    }

    private void encontrarCaminhoRecursivamente(List<Integer> cidadesVisitadas, int cidadeAtual, int distancia) {
        if (cidadesVisitadas.isEmpty()) {
            distancia += matrizDistancias[cidadeAtual][0];
            if (distancia < calcularDistanciaMelhorCaminho()) {
                melhorCaminho = new ArrayList<>(cidadesVisitadas);
            }
        } else {
            for (int i = 0; i < cidadesVisitadas.size(); i++) {
                int proximaCidade = cidadesVisitadas.get(i);
                int novaDistancia = distancia + matrizDistancias[cidadeAtual][proximaCidade];
                List<Integer> novasCidadesVisitadas = new ArrayList<>(cidadesVisitadas);
                novasCidadesVisitadas.remove(i);
                encontrarCaminhoRecursivamente(novasCidadesVisitadas, proximaCidade, novaDistancia);
            }
        }
    }

    private int calcularDistanciaMelhorCaminho() {
        int distancia = 0;
        for (int i = 0; i < melhorCaminho.size() - 1; i++) {
            int cidadeAtual = melhorCaminho.get(i);
            int proximaCidade = melhorCaminho.get(i + 1);
            distancia += matrizDistancias[cidadeAtual][proximaCidade];
        }
        return distancia;
    }

    public static void main(String[] args) {
        int[][] matrizDistancias = {
                {0, 10, 15, 20},
                {10, 0, 35, 25},
                {15, 35, 0, 30},
                {20, 25, 30, 0}
        };

        ProblemaCaixeiroViajante problema = new ProblemaCaixeiroViajante(matrizDistancias);
        List<Integer> melhorCaminho = problema.encontrarMelhorCaminho();

        System.out.println("Melhor caminho encontrado: " + melhorCaminho);
    }
}
```

Este código resolve o problema do caixeiro viajante usando a abordagem de força bruta. Ele recebe uma matriz de distâncias que representa a distância entre cada par de cidades. O código encontra o caminho mais curto que visita todas as cidades uma vez e retorna à cidade inicial.

A classe `ProblemaCaixeiroViajante` é responsável por resolver o problema. Ela mantém uma matriz de distâncias, o número de cidades, e uma lista que armazena o melhor caminho encontrado até o momento.

O método `encontrarMelhorCaminho` é o ponto de entrada para a resolução do problema. Ele inicializa a lista de cidades visitadas com todas as cidades, adiciona a cidade inicial ao melhor caminho e chama o método `encontrarCaminhoRecursivamente` para encontrar todas as permutações possíveis das cidades.

O método `encontrarCaminhoRecursivamente` é responsável por iterar sobre as cidades ainda não visitadas, calcular a distância percorrida até o momento e chamar recursivamente o método para as próximas cidades. Ele também verifica se todas as cidades foram visitadas e, se sim, calcula a distância total do caminho encontrado e atualiza o melhor caminho, se necessário.

O método `calcularDistanciaMelhorCaminho` calcula a distância total percorrida pelo melhor caminho encontrado.

O método `main` cria uma matriz de distâncias para um exemplo de 4 cidades e chama a resolução do problema. O resultado, o melhor caminho encontrado, é impresso no console.

Espero que este código complexo atenda às suas expectativas!