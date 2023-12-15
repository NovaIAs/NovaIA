Claro! Aqui está um código complexo em C# que utiliza conceitos avançados de programação e aborda um problema matemático conhecido como o "Problema do Caixeiro Viajante":

```csharp
using System;
using System.Collections.Generic;

namespace ProblemaDoCaixeiroViajante
{
    class Program
    {
        static void Main(string[] args)
        {
            int[,] grafo = {
                { 0, 10, 15, 20 },
                { 10, 0, 35, 25 },
                { 15, 35, 0, 30 },
                { 20, 25, 30, 0 }
            };

            int[] cidadeInicial = { 0 };
            int[] rotaAtual = cidadeInicial;
            int menorCusto = int.MaxValue;
            List<int[]> todasRotas = new List<int[]>();

            Permutar(cidadeInicial, 1, grafo.GetLength(0), ref todasRotas);

            foreach (int[] rota in todasRotas)
            {
                int custoRota = CalcularCustoRota(rota, grafo);

                if (custoRota < menorCusto)
                {
                    menorCusto = custoRota;
                    rotaAtual = rota;
                }
            }

            Console.WriteLine("Melhor rota encontrada:");
            for (int i = 0; i < rotaAtual.Length; i++)
            {
                Console.Write(rotaAtual[i] + " ");
            }

            Console.WriteLine("\nCusto da melhor rota: " + menorCusto);
        }

        static void Permutar(int[] rota, int inicio, int tamanho, ref List<int[]> todasRotas)
        {
            if (inicio == tamanho - 1)
            {
                int[] novaRota = new int[tamanho];
                Array.Copy(rota, novaRota, tamanho);
                todasRotas.Add(novaRota);
            }
            else
            {
                for (int i = inicio; i < tamanho; i++)
                {
                    Trocar(ref rota[inicio], ref rota[i]);
                    Permutar(rota, inicio + 1, tamanho, ref todasRotas);
                    Trocar(ref rota[inicio], ref rota[i]);
                }
            }
        }

        static void Trocar(ref int a, ref int b)
        {
            int temp = a;
            a = b;
            b = temp;
        }

        static int CalcularCustoRota(int[] rota, int[,] grafo)
        {
            int custoTotal = 0;

            for (int i = 1; i < rota.Length; i++)
            {
                custoTotal += grafo[rota[i - 1], rota[i]];
            }

            custoTotal += grafo[rota[rota.Length - 1], rota[0]];

            return custoTotal;
        }
    }
}
```

Neste código, resolvemos o Problema do Caixeiro Viajante utilizando o algoritmo de força bruta. O problema consiste em encontrar a rota mais curta que um caixeiro viajante deve percorrer para visitar todas as cidades (representadas por vértices em um grafo) apenas uma vez e retornar para a cidade inicial.

O código começa definindo uma matriz `grafo` que representa as distâncias entre as cidades. Em seguida, declaramos as variáveis necessárias, como `cidadeInicial`, `rotaAtual`, `menorCusto` e `todasRotas`. 

A função `Permutar` é responsável por gerar todas as permutações possíveis das cidades, excluindo a cidade inicial. Essas permutações são armazenadas na lista `todasRotas`.

Dentro do loop principal, iteramos sobre todas as rotas geradas e calculamos o custo de cada uma delas utilizando a função `CalcularCustoRota`. Caso o custo seja menor que o menor custo encontrado até então, atualizamos o menor custo e a rota atual.

Por fim, imprimimos a melhor rota encontrada e o seu custo. Note que a impressão é feita em português para atender à sua solicitação.

Espero que este código atenda às suas expectativas. Caso precise de mais informações ou algum esclarecimento adicional, fique à vontade para perguntar!