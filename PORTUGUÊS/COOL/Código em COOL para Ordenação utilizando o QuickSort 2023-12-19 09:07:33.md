Claro! Aqui está um código em COOL que realiza um algoritmo de ordenação chamado QuickSort:

```
class Ordenacao {

    method main() : Object {
        let lista : List <- [7, 2, 1, 6, 8, 5, 3, 4] :> List;
        out_string("Lista original: ");
        self.imprimirLista(lista);

        lista <- self.quickSort(lista, 0, lista.length() - 1);

        out_string("\nLista ordenada: ");
        self.imprimirLista(lista);

        out_string("\n");
    };

    method quickSort(lista : List, inicio : Int, fim : Int) : List {
        if inicio < fim then {
            let pivo : Int <- self.partition(lista, inicio, fim);
            lista <- self.quickSort(lista, inicio, pivo - 1);
            lista <- self.quickSort(lista, pivo + 1, fim);
        };
        lista;
    };

    method partition(lista : List, inicio : Int, fim : Int) : Int {
        let pivo : Int <- lista[fim];
        let i : Int <- inicio - 1;

        for j : Int <- inicio while j <= fim - 1 loop {
            if lista[j] <= pivo then {
                i <- i + 1;
                lista.swap(i, j);
            };
        };

        lista.swap(i + 1, fim);
        i + 1;
    };

    method imprimirLista(lista : List) : Object {
        let i : Int <- 0;

        while i < lista.length() loop {
            out_int(lista[i]);
            out_string(" ");
            i <- i + 1;
        };
    };
};

```

Este código em COOL implementa o algoritmo de ordenação Quicksort. Ele começa definindo uma classe chamada "Ordenacao" que possui um método principal "main". Dentro do método "main", uma lista de números é inicializada e exibida na tela. Em seguida, essa lista é ordenada usando o algoritmo Quicksort chamando o método "quickSort". Após a ordenação, a lista ordenada é exibida novamente.

O método "quickSort" é uma função recursiva que divide a lista em sublistas menores, definindo um pivo e reorganizando os elementos com base no pivo. O método "partition" é responsável por realizar a partição da lista e retornar a posição correta do pivo após a ordenação parcial.

Por fim, o método "imprimirLista" é utilizado para exibir a lista na tela.