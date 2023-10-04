Claro! Aqui está um código complexo em Groovy que usa vários recursos avançados da linguagem:

```
import groovy.transform.CompileStatic

@CompileStatic
class ComplexoCodigoGroovy {

    static void main(String[] args) {
        def lista = [1, 2, 3, 4, 5]
        
        def somaPar = lista.findAll { it % 2 == 0 }.sum()
        println("Soma dos números pares da lista: $somaPar")
        
        def multiplicacaoImpar = lista.findAll { it % 2 != 0 }.inject(1) { acc, num -> acc * num }
        println("Multiplicação dos números ímpares da lista: $multiplicacaoImpar")
        
        def listaOrdenada = lista.sort { a, b -> b <=> a }
        println("Lista ordenada em ordem decrescente: $listaOrdenada")
        
        def resultado = calcularFatorial(5)
        println("O fatorial de 5 é: $resultado")
    }
    
    static int calcularFatorial(int num) {
        if (num == 0 || num == 1) {
            return 1
        }
        return num * calcularFatorial(num - 1)
    }
}
```

Este código em Groovy demonstra várias funcionalidades interessantes. Primeiro, ele cria uma lista de números de 1 a 5. Em seguida, ele encontra a soma dos números pares da lista usando o método `findAll` e `sum`.

Depois, ele encontra a multiplicação dos números ímpares da lista usando o método `findAll` e `inject`. O método `inject` é usado para aplicar uma operação em cada elemento da lista e acumular um resultado.

Em seguida, ele ordena a lista em ordem decrescente usando o método `sort` e uma closure que compara os elementos.

Por fim, ele calcula o fatorial de um número usando uma função recursiva `calcularFatorial`. A função verifica se o número é 0 ou 1 e retorna 1 nesses casos. Para outros números, ela chama a si mesma com um número menor e multiplica pelo número atual.

Espero que esse código atenda às suas necessidades!