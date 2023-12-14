Aqui está um código complexo em Scala que envolve a manipulação de listas e a implementação de funções recursivas:

```scala
// Função que retorna a soma de todos os elementos de uma lista de inteiros
def somarLista(lista: List[Int]): Int = {
  lista match {
    case Nil => 0 // Caso a lista esteja vazia, retorna 0
    case head :: tail => head + somarLista(tail) // Caso contrário, soma o primeiro elemento com a soma do restante da lista
  }
}

// Função que retorna uma nova lista com todos os elementos multiplicados por um fator
def multiplicarLista(lista: List[Int], fator: Int): List[Int] = {
  lista match {
    case Nil => Nil // Caso a lista esteja vazia, retorna uma lista vazia
    case head :: tail => (head * fator) :: multiplicarLista(tail, fator) // Caso contrário, multiplica o primeiro elemento pelo fator e chama recursivamente a função para o restante da lista
  }
}

// Função que retorna a lista invertida
def inverterLista(lista: List[Int]): List[Int] = {
  lista match {
    case Nil => Nil // Caso a lista esteja vazia, retorna uma lista vazia
    case head :: tail => inverterLista(tail) ::: List(head) // Caso contrário, chama recursivamente a função para o restante da lista e concatena com uma lista contendo apenas o primeiro elemento
  }
}

// Exemplo de uso das funções
val lista = List(1, 2, 3, 4, 5)

println("Lista original: " + lista)
println("Soma dos elementos: " + somarLista(lista))
println("Lista multiplicada por 2: " + multiplicarLista(lista, 2))
println("Lista invertida: " + inverterLista(lista))
```

Neste código, temos três funções principais: `somarLista`, `multiplicarLista` e `inverterLista`. 

A função `somarLista` recebe uma lista de inteiros e utiliza casamento de padrões (`match`) para tratar dois casos: quando a lista está vazia (`Nil`) e quando a lista possui elementos (`head :: tail`). No primeiro caso, a função retorna 0. No segundo caso, a função soma o primeiro elemento da lista (`head`) com a chamada recursiva da função para o restante da lista (`tail`).

A função `multiplicarLista` recebe uma lista de inteiros e um fator multiplicador. Ela também utiliza casamento de padrões para tratar os casos quando a lista está vazia e quando possui elementos. No caso em que a lista está vazia, a função retorna uma lista vazia. No caso em que a lista possui elementos, a função multiplica o primeiro elemento pelo fator e chama recursivamente a função para o restante da lista.

A função `inverterLista` recebe uma lista de inteiros e utiliza casamento de padrões para tratar os casos de lista vazia e de lista com elementos. No caso de lista vazia, a função retorna uma lista vazia. No caso de lista com elementos, a função chama recursivamente a função para o restante da lista e concatena com uma lista contendo apenas o primeiro elemento.

O código também contém um exemplo de uso das funções, onde uma lista é criada e as três funções são aplicadas a ela. O resultado é impresso no console.