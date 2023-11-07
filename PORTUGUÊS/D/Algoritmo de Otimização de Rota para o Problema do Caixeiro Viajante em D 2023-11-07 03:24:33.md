Claro! Aqui está um código complexo em D que utiliza um algoritmo de otimização para resolver o problema do caixeiro viajante. O código é bastante extenso e foi escrito em português para facilitar a compreensão:

```d
import std.stdio;
import std.algorithm;
import std.random;

struct Cidade {
    string nome;
    double latitude;
    double longitude;
}

double distanciaEntreCidades(Cidade cidade1, Cidade cidade2) {
    double raioTerra = 6371; // raio médio da Terra em quilômetros
    double lat1 = cidade1.latitude.toRadians;
    double lon1 = cidade1.longitude.toRadians;
    double lat2 = cidade2.latitude.toRadians;
    double lon2 = cidade2.longitude.toRadians;

    double dlon = lon2 - lon1;
    double dlat = lat2 - lat1;

    double a = Math.sin(dlat / 2) * Math.sin(dlat / 2) + Math.cos(lat1) * Math.cos(lat2) * Math.sin(dlon / 2) * Math.sin(dlon / 2);
    double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

    double distancia = raioTerra * c;
    return distancia;
}

double calcularDistanciaTotal(Cidade[] caminho) {
    double distanciaTotal = 0;

    for (int i = 0; i < caminho.length - 1; i++) {
        distanciaTotal += distanciaEntreCidades(caminho[i], caminho[i + 1]);
    }

    distanciaTotal += distanciaEntreCidades(caminho[caminho.length - 1], caminho[0]);
    return distanciaTotal;
}

Cidade[] otimizarRota(Cidade[] rota) {
    Cidade[] melhorRota = rota.dup;
    double melhorDistancia = calcularDistanciaTotal(rota);

    for (int i = 0; i < 1000; i++) {
        Cidade[] novaRota = rota.dup;
        novaRota.swap(randomUniform(0, rota.length), randomUniform(0, rota.length));

        double novaDistancia = calcularDistanciaTotal(novaRota);

        if (novaDistancia < melhorDistancia) {
            melhorRota = novaRota.dup;
            melhorDistancia = novaDistancia;
        }
    }

    return melhorRota;
}

void main() {
    Cidade[] cidades = [
        Cidade("São Paulo", -23.550520, -46.633308),
        Cidade("Rio de Janeiro", -22.906847, -43.172897),
        Cidade("Belo Horizonte", -19.922731, -43.945094),
        Cidade("Brasília", -15.794229, -47.882166),
        Cidade("Salvador", -12.971598, -38.501854),
        Cidade("Fortaleza", -3.718394, -38.543395),
    ];

    Cidade[] rotaInicial = cidades.dup;
    randomShuffle(rotaInicial);

    writeln("Rota Inicial:");
    foreach (cidade; rotaInicial) {
        writeln(cidade.nome);
    }

    Cidade[] melhorRota = otimizarRota(rotaInicial);

    writeln("\nMelhor Rota:");
    foreach (cidade; melhorRota) {
        writeln(cidade.nome);
    }

    writeln("\nDistância Total: ", calcularDistanciaTotal(melhorRota), " km");
}
```

Este código implementa o algoritmo do caixeiro viajante utilizando a linguagem D. Primeiro, definimos uma struct `Cidade` que armazena o nome, a latitude e a longitude de cada cidade. Em seguida, temos a função `distanciaEntreCidades`, que calcula a distância em quilômetros entre duas cidades com base em suas coordenadas geográficas.

A função `calcularDistanciaTotal` recebe uma rota (um array de cidades) e calcula a distância total percorrida ao visitar todas as cidades na ordem especificada. A função `otimizarRota` utiliza um algoritmo de busca local para tentar encontrar uma rota mais curta, realizando trocas aleatórias de cidades e atualizando a rota caso seja encontrada uma distância menor.

No `main`, definimos um array de cidades de exemplo e criamos uma rota inicial aleatória. Em seguida, chamamos a função `otimizarRota` para obter uma rota mais curta e exibimos a rota final, bem como a distância total percorrida.

Este código é apenas um exemplo de uma solução complexa em D, mas existem muitas outras possibilidades de códigos complexos que podem ser criados nesta linguagem. Fique à vontade para explorar e experimentar diferentes algoritmos e funcionalidades!