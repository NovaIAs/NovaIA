Claro! Aqui está um exemplo de um código complexo em C# que envolve a manipulação de listas encadeadas:

```csharp
using System;

namespace ListaEncadeada
{
    class Program
    {
        static void Main(string[] args)
        {
            ListaEncadeada lista = new ListaEncadeada();

            // Inserindo elementos na lista
            lista.Inserir(10);
            lista.Inserir(5);
            lista.Inserir(7);
            lista.Inserir(3);
            lista.Inserir(8);

            // Imprimindo a lista
            Console.WriteLine("Lista original:");
            lista.Imprimir();

            // Removendo um elemento da lista
            lista.Remover(7);

            // Imprimindo a lista após a remoção
            Console.WriteLine("\nLista após remover o elemento 7:");
            lista.Imprimir();

            // Verificando se um elemento existe na lista
            Console.WriteLine("\nO elemento 5 existe na lista? " + lista.Buscar(5));

            Console.ReadLine();
        }
    }

    class No
    {
        public int Valor { get; set; }
        public No Proximo { get; set; }

        public No(int valor)
        {
            Valor = valor;
            Proximo = null;
        }
    }

    class ListaEncadeada
    {
        private No inicio;

        public ListaEncadeada()
        {
            inicio = null;
        }

        public void Inserir(int valor)
        {
            No novoNo = new No(valor);

            if (inicio == null)
            {
                inicio = novoNo;
            }
            else
            {
                No atual = inicio;

                while (atual.Proximo != null)
                {
                    atual = atual.Proximo;
                }

                atual.Proximo = novoNo;
            }
        }

        public void Remover(int valor)
        {
            if (inicio != null)
            {
                if (inicio.Valor == valor)
                {
                    inicio = inicio.Proximo;
                }
                else
                {
                    No atual = inicio;

                    while (atual.Proximo != null && atual.Proximo.Valor != valor)
                    {
                        atual = atual.Proximo;
                    }

                    if (atual.Proximo != null)
                    {
                        atual.Proximo = atual.Proximo.Proximo;
                    }
                }
            }
        }

        public bool Buscar(int valor)
        {
            No atual = inicio;

            while (atual != null)
            {
                if (atual.Valor == valor)
                {
                    return true;
                }

                atual = atual.Proximo;
            }

            return false;
        }

        public void Imprimir()
        {
            No atual = inicio;

            while (atual != null)
            {
                Console.Write(atual.Valor + " ");
                atual = atual.Proximo;
            }

            Console.WriteLine();
        }
    }
}
```

Neste exemplo, implementei uma lista encadeada em C#. Uma lista encadeada é uma estrutura de dados em que cada elemento possui uma referência para o próximo elemento da lista.

O programa começa criando a classe `No`, que representa um nó da lista. Cada nó possui um valor e uma referência para o próximo nó.

Em seguida, a classe `ListaEncadeada` é implementada. Essa classe contém o nó inicial da lista (`inicio`), que é inicializado como `null` no construtor.

A classe `ListaEncadeada` tem métodos para inserir, remover, buscar e imprimir elementos na lista. O método `Inserir` insere um novo nó no final da lista, percorrendo a lista até encontrar o último nó e atribuindo a referência do próximo nó ao novo nó.

O método `Remover` remove um nó da lista, verificando se o nó a ser removido é o nó inicial ou se está em algum lugar no meio da lista. Se for o nó inicial, a referência do próximo nó é atribuída ao `inicio`. Caso contrário, o método percorre a lista até encontrar o nó a ser removido e atualiza a referência do próximo nó.

O método `Buscar` verifica se um determinado valor existe na lista, percorrendo a lista e comparando cada valor com o valor informado.

Por fim, o método `Imprimir` percorre a lista e imprime cada valor na tela.

No `Main`, um exemplo de uso da lista encadeada é demonstrado. Elementos são inseridos na lista usando o método `Inserir`, em seguida, a lista é impressa usando o método `Imprimir`. Depois, um elemento é removido da lista usando o método `Remover` e a lista é impressa novamente. Por fim, o método `Buscar` é usado para verificar se um elemento existe na lista.