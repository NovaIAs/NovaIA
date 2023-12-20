Claro! Aqui está um exemplo de código em COOL que faz a ordenação de um vetor de números inteiros usando o algoritmo de ordenação por inserção:

```
class Main inherits IO {

  (* Método auxiliar para imprimir o vetor *)
  method printArray(arr : Array of Int) : Object {
    var i : Int;
    for i <- 0 to arr.length - 1 loop
      out_string(arr[i].str);
      out_string(" ");
    pool;
    out_string("\n");
  };

  (* Método auxiliar para ordenar o vetor usando o algoritmo de ordenação por inserção *)
  method insertionSort(arr : Array of Int) : Object {
    var i, j, key : Int;
    for i <- 1 to arr.length - 1 loop
      key <- arr[i];
      j <- i - 1;
      while j >= 0 && arr[j] > key loop
        arr[j + 1] <- arr[j];
        j <- j - 1;
      pool;
      arr[j + 1] <- key;
    pool;
    self.printArray(arr);
  };

  (* Método principal *)
  main() : Object {
    var arr : Array of Int;
    arr <- Array.make(5, 0); (* Cria um vetor de tamanho 5 preenchido com zeros *)
    arr[0] <- 5;
    arr[1] <- 2;
    arr[2] <- 4;
    arr[3] <- 6;
    arr[4] <- 1;
    self.insertionSort(arr); (* Chama o método de ordenação *)
    out_string("Vetor ordenado: ");
    self.printArray(arr); (* Imprime o vetor ordenado *)
  };
};
```

Neste exemplo, temos uma classe `Main` que herda da classe `IO`, que é uma classe padrão em COOL que permite a entrada e saída de dados. A classe `Main` contém três métodos: `printArray`, `insertionSort` e `main`.

O método `printArray` recebe um vetor de números inteiros como parâmetro e imprime seus elementos na saída padrão.

O método `insertionSort` recebe um vetor de números inteiros como parâmetro e utiliza o algoritmo de ordenação por inserção para ordenar os elementos do vetor em ordem crescente. A cada iteração do algoritmo, o vetor é impresso na saída padrão.

O método `main` é o ponto de entrada do programa. Ele cria um vetor de tamanho 5 e preenche com alguns valores. Em seguida, chama o método `insertionSort` para ordenar o vetor e imprime o vetor ordenado.

Espero que este exemplo atenda às suas expectativas! Se precisar de mais alguma coisa, estou à disposição.