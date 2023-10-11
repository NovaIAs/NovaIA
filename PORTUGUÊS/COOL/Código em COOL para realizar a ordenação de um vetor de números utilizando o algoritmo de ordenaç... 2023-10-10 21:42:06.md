Claro! Aqui está um código em COOL que realiza a ordenação de um vetor de números utilizando o algoritmo de ordenação por inserção:

```
class Ordenacao {

    // Função para ordenar um vetor utilizando o algoritmo de inserção
    method ordenar(vetor : Object[] : Int) : Object[] {

        let n : Int <- vetor.length;
        let i : Int;
        let j : Int;
        let chave : Int;

        // Percorre o vetor a partir do segundo elemento
        for i <- 1 to n-1 loop
            chave <- vetor[i];
            j <- i - 1;

            // Move os elementos maiores que a chave para a direita
            while j >= 0 && vetor[j] > chave loop
                vetor[j+1] <- vetor[j];
                j <- j - 1;
            pool;

            vetor[j+1] <- chave;
        pool;

        return vetor;
    };
};

// Teste da função de ordenação
let vetor : Object[] <- [5, 2, 8, 1, 9];
let ordenacao : Ordenacao <- new Ordenacao;
let vetorOrdenado : Object[] <- ordenacao.ordenar(vetor);

// Imprime o vetor ordenado
let i : Int;
for i <- 0 to vetorOrdenado.length-1 loop
    out_string(vetorOrdenado[i].asString());
pool;
```

Neste código, temos uma classe chamada `Ordenacao` que contém um método chamado `ordenar`. Esse método recebe um vetor de números como parâmetro e retorna o vetor ordenado.

O algoritmo utilizado é o de ordenação por inserção. Ele funciona percorrendo o vetor a partir do segundo elemento e, para cada elemento, o algoritmo insere o elemento em sua posição correta no subvetor já ordenado. Para isso, ele compara o elemento com os elementos anteriores e move os elementos maiores para a direita até encontrar a posição correta.

O código inicia criando um vetor de números não ordenados. Em seguida, cria uma instância da classe `Ordenacao` e chama o método `ordenar` passando o vetor como argumento. O vetor retornado pela função é armazenado na variável `vetorOrdenado`.

Por fim, o código imprime o vetor ordenado utilizando um laço de repetição `for`.

Espero que este código atenda às suas expectativas! Se você tiver mais alguma solicitação, estou aqui para ajudar.