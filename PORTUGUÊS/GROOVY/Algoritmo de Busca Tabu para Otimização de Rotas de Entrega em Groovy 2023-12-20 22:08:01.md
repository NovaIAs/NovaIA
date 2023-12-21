Claro! Aqui está um código complexo em Groovy que resolve um problema de otimização de rotas de entrega. Este código usa o algoritmo de busca tabu para encontrar a melhor combinação de rotas para minimizar o tempo total de entrega.

```
import java.util.ArrayList

class Rota {
    String origem
    String destino
    int tempo
}

class BuscaTabu {
    int tamanhoListaTabu
    int iteracoesMaximas
    ArrayList<Rota> rotas
    ArrayList<ArrayList<Rota>> listaTabu

    BuscaTabu(int tamanhoListaTabu, int iteracoesMaximas, ArrayList<Rota> rotas) {
        this.tamanhoListaTabu = tamanhoListaTabu
        this.iteracoesMaximas = iteracoesMaximas
        this.rotas = rotas
        this.listaTabu = new ArrayList<ArrayList<Rota>>()
    }

    ArrayList<Rota> encontrarMelhorCombinaçãoRotas() {
        ArrayList<Rota> melhorCombinação = new ArrayList<Rota>(rotas)
        int melhorTempo = calcularTempoTotal(melhorCombinação)

        for (int iteracao = 0; iteracao < iteracoesMaximas; iteracao++) {
            ArrayList<Rota> novaCombinação = melhorCombinação
            ArrayList<Rota> melhorVizinho = null
            int tempoVizinho = Integer.MAX_VALUE

            for (Rota rota : rotas) {
                if (!listaTabu.contains(novaCombinação) || calcularTempoTotal(novaCombinação) < tempoVizinho) {
                    melhorVizinho = novaCombinação
                    tempoVizinho = calcularTempoTotal(novaCombinação)
                }
            }

            listaTabu.add(melhorVizinho)
            if (listaTabu.size() > tamanhoListaTabu) {
                listaTabu.remove(0)
            }

            if (tempoVizinho < melhorTempo) {
                melhorCombinação = melhorVizinho
                melhorTempo = tempoVizinho
            }
        }

        return melhorCombinação
    }

    int calcularTempoTotal(ArrayList<Rota> combinacao) {
        int tempoTotal = 0
        for (Rota rota : combinacao) {
            tempoTotal += rota.tempo
        }
        return tempoTotal
    }
}

// Exemplo de utilização
ArrayList<Rota> rotas = new ArrayList<Rota>()

// Adicionar rotas
rotas.add(new Rota(origem: "A", destino: "B", tempo: 5))
rotas.add(new Rota(origem: "B", destino: "C", tempo: 10))
rotas.add(new Rota(origem: "C", destino: "D", tempo: 8))
// ...

BuscaTabu buscaTabu = new BuscaTabu(tamanhoListaTabu: 10, iteracoesMaximas: 1000, rotas: rotas)
ArrayList<Rota> melhorCombinação = buscaTabu.encontrarMelhorCombinaçãoRotas()

println "Melhor combinação de rotas:"
for (Rota rota : melhorCombinação) {
    println "${rota.origem} -> ${rota.destino} (${rota.tempo} minutos)"
}
```

Neste código, a classe `Rota` é usada para representar uma rota entre duas localidades, com um tempo de viagem associado. A classe `BuscaTabu` é responsável por executar a busca tabu para encontrar a melhor combinação de rotas.

O método `encontrarMelhorCombinaçãoRotas` realiza a busca tabu, iterando um número máximo de iterações. A cada iteração, o código verifica se uma nova combinação de rotas é melhor do que a combinação atual, mantendo uma lista tabu para evitar retornar a combinações já visitadas.

O método `calcularTempoTotal` calcula o tempo total de uma combinação de rotas, somando os tempos de todas as rotas.

O exemplo de utilização cria algumas rotas de exemplo e instancia um objeto `BuscaTabu`, passando os parâmetros desejados. Em seguida, ele chama o método `encontrarMelhorCombinaçãoRotas` para encontrar a melhor combinação de rotas. Por fim, ele imprime a melhor combinação encontrada.

Este código é apenas um exemplo, e sua complexidade pode variar dependendo da quantidade de rotas e da lógica específica do problema que você deseja resolver.