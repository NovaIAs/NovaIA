Claro! Abaixo está um código em C# que gera uma representação visual de uma árvore binária de busca. A árvore é preenchida com nós aleatórios e, em seguida, é exibida na forma de uma estrutura de árvore no console.

```csharp
using System;

namespace ArvoreBinaria
{
    class Program
    {
        class No
        {
            public int valor;
            public No esquerda;
            public No direita;

            public No(int valor)
            {
                this.valor = valor;
                esquerda = null;
                direita = null;
            }
        }

        class ArvoreBinaria
        {
            private No raiz;

            public ArvoreBinaria()
            {
                raiz = null;
            }

            public void Inserir(int valor)
            {
                raiz = InserirRecursivo(raiz, valor);
            }

            private No InserirRecursivo(No atual, int valor)
            {
                if (atual == null)
                {
                    return new No(valor);
                }

                if (valor < atual.valor)
                {
                    atual.esquerda = InserirRecursivo(atual.esquerda, valor);
                }
                else if (valor > atual.valor)
                {
                    atual.direita = InserirRecursivo(atual.direita, valor);
                }

                return atual;
            }

            public void ExibirArvore()
            {
                ExibirRecursivo(raiz, 0);
            }

            private void ExibirRecursivo(No atual, int nivel)
            {
                if (atual != null)
                {
                    ExibirRecursivo(atual.direita, nivel + 1);
                    Console.WriteLine();
                    for (int i = 0; i < nivel; i++)
                    {
                        Console.Write("    ");
                    }
                    Console.Write(atual.valor);
                    ExibirRecursivo(atual.esquerda, nivel + 1);
                }
            }
        }

        static void Main(string[] args)
        {
            ArvoreBinaria arvore = new ArvoreBinaria();

            Random random = new Random();
            for (int i = 0; i < 10; i++)
            {
                int valor = random.Next(1, 101);
                arvore.Inserir(valor);
            }

            Console.WriteLine("Árvore Binária de Busca:");
            arvore.ExibirArvore();

            Console.ReadLine();
        }
    }
}
```

Neste código, começamos definindo duas classes: `No` e `ArvoreBinaria`. A classe `No` representa um nó individual da árvore binária de busca e armazena um valor inteiro, além das referências para os nós filhos esquerdo e direito. A classe `ArvoreBinaria` é responsável por criar a estrutura da árvore binária, inserir nós e exibir a árvore no console.

Dentro da classe `ArvoreBinaria`, temos o método `Inserir` que recebe um valor inteiro e insere um novo nó na árvore de forma recursiva. O método `InserirRecursivo` é chamado internamente pelo método `Inserir` e percorre a árvore até encontrar a posição correta para inserir o novo nó, levando em consideração se o valor é menor ou maior que o valor atual do nó.

O método `ExibirArvore` percorre a árvore de forma recursiva e exibe os nós no formato de uma estrutura de árvore no console. O método `ExibirRecursivo` é chamado internamente e utiliza uma abordagem de visita em ordem invertida para imprimir os nós da árvore.

No método `Main`, criamos uma instância da classe `ArvoreBinaria` chamada `arvore` e, em seguida, utilizamos um loop para inserir 10 nós aleatórios na árvore. Por fim, chamamos o método `ExibirArvore` para exibir a árvore completa no console.