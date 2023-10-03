Claro! Aqui está um código complexo em PHP que utiliza o conceito de algoritmo genético para otimizar a solução de um problema de otimização. Neste caso, vamos resolver o problema clássico do "Caixeiro Viajante".

```php
<?php

// Definir as cidades do problema
$cidades = [
    "A", "B", "C", "D", "E", "F", "G"
];

// Definir as distâncias entre as cidades
$distancias = [
    [0, 2, 9, 10, 7, 4, 8],
    [2, 0, 6, 4, 3, 2, 1],
    [9, 6, 0, 8, 5, 4, 3],
    [10, 4, 8, 0, 6, 7, 5],
    [7, 3, 5, 6, 0, 2, 4],
    [4, 2, 4, 7, 2, 0, 6],
    [8, 1, 3, 5, 4, 6, 0]
];

// Função para calcular a distância total de um caminho
function calcularDistancia($caminho) {
    global $distancias;
    $distanciaTotal = 0;
    $numCidades = count($caminho);
    
    for ($i = 0; $i < $numCidades - 1; $i++) {
        $cidadeAtual = $caminho[$i];
        $proximaCidade = $caminho[$i + 1];
        $distanciaTotal += $distancias[$cidadeAtual][$proximaCidade];
    }
    
    // Considerar também a distância de volta à cidade inicial
    $distanciaTotal += $distancias[$caminho[$numCidades - 1]][$caminho[0]];
    
    return $distanciaTotal;
}

// Função para gerar uma população inicial de possíveis caminhos
function gerarPopulacaoInicial($numIndividuos, $cidades) {
    $populacao = [];
    
    for ($i = 0; $i < $numIndividuos; $i++) {
        $caminhoAleatorio = $cidades;
        shuffle($caminhoAleatorio);
        $populacao[] = $caminhoAleatorio;
    }
    
    return $populacao;
}

// Função para selecionar os melhores indivíduos da população
function selecao($populacao, $numSelecionados) {
    $individuosSelecionados = [];
    
    usort($populacao, function($a, $b) {
        return calcularDistancia($a) - calcularDistancia($b);
    });
    
    for ($i = 0; $i < $numSelecionados; $i++) {
        $individuosSelecionados[] = $populacao[$i];
    }
    
    return $individuosSelecionados;
}

// Função para cruzar dois indivíduos da população
function cruzamento($individuo1, $individuo2) {
    $pontoCorte = rand(1, count($individuo1) - 2);
    
    $filho1 = array_slice($individuo1, 0, $pontoCorte);
    $filho2 = array_slice($individuo2, 0, $pontoCorte);
    
    foreach ($individuo2 as $cidade) {
        if (!in_array($cidade, $filho1)) {
            $filho1[] = $cidade;
        }
    }
    
    foreach ($individuo1 as $cidade) {
        if (!in_array($cidade, $filho2)) {
            $filho2[] = $cidade;
        }
    }
    
    return [$filho1, $filho2];
}

// Função para mutar um indivíduo da população
function mutacao($individuo) {
    $posicao1 = rand(0, count($individuo) - 1);
    $posicao2 = rand(0, count($individuo) - 1);
    
    $temp = $individuo[$posicao1];
    $individuo[$posicao1] = $individuo[$posicao2];
    $individuo[$posicao2] = $temp;
    
    return $individuo;
}

// Algoritmo genético para resolver o problema do Caixeiro Viajante
function resolverCaixeiroViajante($numGeracoes, $populacaoInicial) {
    $populacao = $populacaoInicial;
    
    for ($geracao = 0; $geracao < $numGeracoes; $geracao++) {
        $selecionados = selecao($populacao, 2);
        list($filho1, $filho2) = cruzamento($selecionados[0], $selecionados[1]);
        
        $populacao[] = mutacao($filho1);
        $populacao[] = mutacao($filho2);
        
        if (count($populacao) > 10) {
            array_pop($populacao);
        }
        
        echo "Geração: " . ($geracao + 1) . "\n";
        echo "Melhor caminho: " . implode(" -> ", $selecionados[0]) . "\n";
        echo "Distância: " . calcularDistancia($selecionados[0]) . "\n";
        echo "------------------------\n";
    }
    
    $melhorCaminho = selecao($populacao, 1)[0];
    
    echo "Melhor caminho encontrado: " . implode(" -> ", $melhorCaminho) . "\n";
    echo "Distância: " . calcularDistancia($melhorCaminho) . "\n";
}

// Executar o algoritmo com uma população inicial de 5 indivíduos e 100 gerações
$resolverCaixeiroViajante(100, gerarPopulacaoInicial(5, $cidades));

?>
```

Neste código, o algoritmo genético é utilizado para encontrar o caminho mais curto para um caixeiro viajante visitar todas as cidades. O objetivo é minimizar a distância total percorrida pelo caixeiro.

Primeiro, definimos as cidades do problema e as distâncias entre elas. Em seguida, temos funções para calcular a distância total de um caminho, gerar uma população inicial de possíveis caminhos, selecionar os melhores indivíduos da população, cruzar dois indivíduos, mutar um indivíduo e o algoritmo genético em si.

O algoritmo genético consiste em repetir o processo de seleção, cruzamento e mutação por um número definido de gerações. A cada geração, é exibido o melhor caminho encontrado até o momento e sua distância. Ao final, é exibido o melhor caminho encontrado em todas as gerações.

Espero que este código atenda às suas expectativas e demonstre um exemplo complexo em PHP.