Claro! Aqui está um código em PHP complexo e diferenciado para você:

```php
<?php

// Classe para realizar a busca em um grafo
class BuscaEmGrafo
{
    private $grafo;
    private $visitados;
    
    public function __construct($grafo)
    {
        $this->grafo = $grafo;
        $this->visitados = array();
    }
    
    // Método para realizar a busca em profundidade no grafo
    public function buscaEmProfundidade($vertice)
    {
        $this->visitados[$vertice] = true;
        echo "Visitando o vértice: " . $vertice . "\n";
        
        foreach ($this->grafo[$vertice] as $vizinho) {
            if (!isset($this->visitados[$vizinho])) {
                $this->buscaEmProfundidade($vizinho);
            }
        }
    }
}

// Grafo de exemplo
$grafo = array(
    'A' => array('B', 'C'),
    'B' => array('A', 'D'),
    'C' => array('A', 'E'),
    'D' => array('B'),
    'E' => array('C', 'F'),
    'F' => array('E')
);

// Criação de um objeto de busca em grafo
$buscaEmGrafo = new BuscaEmGrafo($grafo);

// Realiza a busca em profundidade a partir do vértice 'A'
$buscaEmGrafo->buscaEmProfundidade('A');
```

Explicação do código:

Neste código em PHP, criamos uma classe chamada `BuscaEmGrafo` que é responsável por realizar uma busca em profundidade em um grafo. O construtor da classe recebe o grafo como parâmetro e inicializa a lista de vértices visitados.

A classe possui um método chamado `buscaEmProfundidade` que recebe um vértice como parâmetro e realiza a busca em profundidade a partir deste vértice. A medida que os vértices são visitados, eles são marcados como visitados e exibidos na tela.

Após a definição da classe, criamos um grafo de exemplo com algumas arestas e vértices. Em seguida, criamos um objeto da classe `BuscaEmGrafo` passando o grafo de exemplo como parâmetro.

Por fim, chamamos o método `buscaEmProfundidade` do objeto, passando o vértice 'A' como parâmetro. Isso irá iniciar a busca em profundidade a partir do vértice 'A' e exibir na tela os vértices visitados.

Espero que este código atenda às suas expectativas!