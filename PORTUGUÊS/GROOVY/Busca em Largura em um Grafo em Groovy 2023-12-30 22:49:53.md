Claro! Aqui está um código em Groovy que realiza uma busca em largura em um grafo:

```groovy
// Classe que representa um nó no grafo
class No {
    String valor
    List<No> vizinhos

    No(String valor) {
        this.valor = valor
        this.vizinhos = []
    }

    void adicionarVizinho(No vizinho) {
        vizinhos.add(vizinho)
    }
}

// Função que realiza a busca em largura
def buscaEmLargura(No raiz) {
    def fila = [raiz]
    def visitados = [raiz]
    def resultado = []

    while (fila) {
        def noAtual = fila.remove(0)
        resultado.add(noAtual.valor)

        for (vizinho in noAtual.vizinhos) {
            if (!visitados.contains(vizinho)) {
                fila.add(vizinho)
                visitados.add(vizinho)
            }
        }
    }

    return resultado
}

// Criando os nós do grafo
def a = new No("A")
def b = new No("B")
def c = new No("C")
def d = new No("D")
def e = new No("E")
def f = new No("F")

// Adicionando os vizinhos
a.adicionarVizinho(b)
a.adicionarVizinho(c)
b.adicionarVizinho(d)
c.adicionarVizinho(d)
c.adicionarVizinho(e)
d.adicionarVizinho(e)
e.adicionarVizinho(f)

// Realizando a busca em largura a partir do nó A
def resultado = buscaEmLargura(a)

// Imprimindo o resultado
println "Resultado da busca em largura:"
println resultado.join(" -> ")
```

Explicação do código:

- Começamos definindo uma classe chamada `No`, que representa um nó no grafo. Cada nó possui um valor e uma lista de vizinhos.
- Em seguida, temos a função `buscaEmLargura`, que recebe um nó raiz e realiza a busca em largura. Essa função utiliza uma fila para armazenar os nós a serem visitados, uma lista de nós visitados e uma lista para armazenar o resultado da busca.
- Dentro do loop principal da busca em largura, removemos o primeiro nó da fila, adicionamos o valor desse nó ao resultado e percorremos todos os vizinhos desse nó. Se um vizinho ainda não foi visitado, adicionamos ele à fila e à lista de visitados.
- Por fim, criamos os nós do grafo, adicionamos os vizinhos e realizamos a busca em largura a partir do nó A.
- O resultado da busca em largura é impresso no console.

Espero que esse código atenda às suas expectativas!