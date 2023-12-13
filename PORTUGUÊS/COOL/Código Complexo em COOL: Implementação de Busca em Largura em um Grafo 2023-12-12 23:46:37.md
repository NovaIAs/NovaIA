Claro! Aqui está um código complexo em COOL, que usa uma estrutura de dados avançada e implementa um algoritmo de busca em largura:

class Fila {
    var fila : List<Int> <- nil

    method adicionar(elemento : Int) : SELF_TYPE {
        fila <- fila.concat(elemento)
        self
    }

    method remover() : Int {
        if fila.empty() then
            error "A fila está vazia!"
        else {
            let primeiro_elemento <- fila.head()
            fila <- fila.tail()
            primeiro_elemento
        }
    }

    method vazia() : Bool {
        fila.empty()
    }
}

class Grafo {
    var vertices : List<String>
    var arestas : List<List<Int>>

    method adicionar_vertice(nome : String) : SELF_TYPE {
        vertices <- vertices.concat(nome)
        arestas <- arestas.concat(nil)
        self
    }

    method adicionar_aresta(origem : String, destino : String) : SELF_TYPE {
        let origem_index : Int <- vertices.find_index(origem).abort_if_none("O vértice de origem não existe!")
        let destino_index : Int <- vertices.find_index(destino).abort_if_none("O vértice de destino não existe!")

        if arestas.at(origem_index).isvoid() then {
            let nova_lista_arestas : List<Int> <- new List<Int>.empty()
            arestas.at_put(origem_index, nova_lista_arestas)
        }

        if arestas.at(destino_index).isvoid() then {
            let nova_lista_arestas : List<Int> <- new List<Int>.empty()
            arestas.at_put(destino_index, nova_lista_arestas)
        }

        arestas.at(origem_index).concat(destino_index)
        self
    }

    method busca_largura(origem : String) : List<String> {
        let origem_index : Int <- vertices.find_index(origem).abort_if_none("O vértice de origem não existe!")

        var visitados : List<Bool> <- new List<Bool>.empty()
        var fila : Fila <- new Fila
        var resultado : List<String> <- new List<String>.empty()

        for i : Int <- 0  to vertices.length() - 1 loop
            visitados <- visitados.concat(false)
        pool

        fila.adicionar(origem_index)
        visitados.at_put(origem_index, true)

        while not fila.vazia() loop
            let vertice_atual : Int <- fila.remover()
            resultado <- resultado.concat(vertices.at(vertice_atual))

            let lista_adjacente : List<Int> <- arestas.at(vertice_atual).abort_if_none("Erro ao obter lista de adjacência!")

            for adjacente : Int <- 0 to lista_adjacente.length() - 1 loop
                let adjacente_index : Int <- lista_adjacente.at(adjacente)
                if not visitados.at(adjacente_index) then
                    fila.adicionar(adjacente_index)
                    visitados.at_put(adjacente_index, true)
                pool
            pool

        resultado
    }
}

let grafo : Grafo <- new Grafo
grafo.adicionar_vertice("A")
    .adicionar_vertice("B")
    .adicionar_vertice("C")
    .adicionar_vertice("D")
    .adicionar_vertice("E")
    .adicionar_aresta("A", "B")
    .adicionar_aresta("A", "C")
    .adicionar_aresta("B", "D")
    .adicionar_aresta("C", "D")
    .adicionar_aresta("D", "E")

let resultado : List<String> <- grafo.busca_largura("A")

IO.out_string("Resultado da busca em largura: ")
for vertice : String <- resultado loop
    IO.out_string(vertice + " ")
pool
IO.out_string("\n")