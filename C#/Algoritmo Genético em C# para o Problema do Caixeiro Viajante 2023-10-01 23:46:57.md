Claro! Aqui está um código complexo em C# que implementa um algoritmo genético para resolver o problema do caixeiro viajante, uma famosa questão de otimização de rotas:

```csharp
using System;
using System.Collections.Generic;

namespace AlgoritmoGenetico
{
    class Program
    {
        static void Main(string[] args)
        {
            // Definir as cidades e suas coordenadas
            List<Cidade> cidades = new List<Cidade>()
            {
                new Cidade("A", 2, 3),
                new Cidade("B", 5, 8),
                new Cidade("C", 10, 7),
                new Cidade("D", 8, 4),
                new Cidade("E", 1, 6),
            };

            // Configurações do algoritmo genético
            int tamanhoPopulacao = 100;
            int numeroGeracoes = 1000;
            double taxaMutacao = 0.2;

            // Inicializar a população inicial
            Populacao populacao = new Populacao(tamanhoPopulacao, cidades);

            for (int geracao = 0; geracao < numeroGeracoes; geracao++)
            {
                populacao = AlgoritmoGenetico.Evoluir(populacao, taxaMutacao);
            }

            // Obter o melhor indivíduo da última geração
            Individuo melhorIndividuo = populacao.ObterMelhorIndividuo();

            Console.WriteLine("Melhor caminho encontrado:");
            Console.WriteLine(string.Join(" -> ", melhorIndividuo.CidadesVisitadas));
            Console.WriteLine("Distância total: " + melhorIndividuo.CalcularDistanciaTotal());
        }
    }

    class Cidade
    {
        public string Nome { get; }
        public int X { get; }
        public int Y { get; }

        public Cidade(string nome, int x, int y)
        {
            Nome = nome;
            X = x;
            Y = y;
        }

        public double CalcularDistancia(Cidade outraCidade)
        {
            int difX = Math.Abs(X - outraCidade.X);
            int difY = Math.Abs(Y - outraCidade.Y);
            return Math.Sqrt(difX * difX + difY * difY);
        }
    }

    class Individuo
    {
        public List<Cidade> CidadesVisitadas { get; }

        public Individuo(List<Cidade> cidades)
        {
            CidadesVisitadas = new List<Cidade>(cidades);
            CidadesVisitadas.Shuffle();
        }

        public double CalcularDistanciaTotal()
        {
            double distanciaTotal = 0;

            for (int i = 0; i < CidadesVisitadas.Count - 1; i++)
            {
                distanciaTotal += CidadesVisitadas[i].CalcularDistancia(CidadesVisitadas[i + 1]);
            }

            distanciaTotal += CidadesVisitadas[CidadesVisitadas.Count - 1].CalcularDistancia(CidadesVisitadas[0]);

            return distanciaTotal;
        }

        public Individuo Cruzar(Individuo outroIndividuo)
        {
            int pontoCorte = new Random().Next(1, CidadesVisitadas.Count - 1);

            List<Cidade> novasCidades = new List<Cidade>(CidadesVisitadas.GetRange(0, pontoCorte));

            foreach (Cidade cidade in outroIndividuo.CidadesVisitadas)
            {
                if (!novasCidades.Contains(cidade))
                {
                    novasCidades.Add(cidade);
                }
            }

            return new Individuo(novasCidades);
        }

        public void Mutar(double taxaMutacao)
        {
            for (int i = 0; i < CidadesVisitadas.Count; i++)
            {
                if (new Random().NextDouble() < taxaMutacao)
                {
                    int posicaoTroca = new Random().Next(CidadesVisitadas.Count);
                    Cidade temp = CidadesVisitadas[i];
                    CidadesVisitadas[i] = CidadesVisitadas[posicaoTroca];
                    CidadesVisitadas[posicaoTroca] = temp;
                }
            }
        }
    }

    class Populacao
    {
        public List<Individuo> Individuos { get; }

        public Populacao(int tamanhoPopulacao, List<Cidade> cidades)
        {
            Individuos = new List<Individuo>();

            for (int i = 0; i < tamanhoPopulacao; i++)
            {
                Individuos.Add(new Individuo(cidades));
            }
        }

        public Individuo ObterMelhorIndividuo()
        {
            double menorDistancia = double.MaxValue;
            Individuo melhorIndividuo = null;

            foreach (Individuo individuo in Individuos)
            {
                double distancia = individuo.CalcularDistanciaTotal();

                if (distancia < menorDistancia)
                {
                    menorDistancia = distancia;
                    melhorIndividuo = individuo;
                }
            }

            return melhorIndividuo;
        }
    }

    static class AlgoritmoGenetico
    {
        public static Populacao Evoluir(Populacao populacao, double taxaMutacao)
        {
            Populacao novaPopulacao = new Populacao(populacao.Individuos.Count, populacao.Individuos[0].CidadesVisitadas);

            for (int i = 0; i < populacao.Individuos.Count; i++)
            {
                Individuo pai = SelecionarIndividuo(populacao);
                Individuo mae = SelecionarIndividuo(populacao);

                Individuo filho = pai.Cruzar(mae);

                filho.Mutar(taxaMutacao);

                novaPopulacao.Individuos[i] = filho;
            }

            return novaPopulacao;
        }

        private static Individuo SelecionarIndividuo(Populacao populacao)
        {
            int indice = new Random().Next(populacao.Individuos.Count);
            return populacao.Individuos[indice];
        }
    }

    static class Extensions
    {
        public static void Shuffle<T>(this IList<T> list)
        {
            Random rng = new Random();
            int n = list.Count;

            while (n > 1)
            {
                n--;
                int k = rng.Next(n + 1);
                T value = list[k];
                list[k] = list[n];
                list[n] = value;
            }
        }
    }
}
```

Este código implementa um algoritmo genético para resolver o problema do caixeiro viajante, onde um viajante precisa visitar um conjunto de cidades e retornar ao ponto de partida, encontrando a rota mais curta possível.

O algoritmo começa definindo um conjunto de cidades representadas por suas coordenadas X e Y. Em seguida, são configuradas as opções do algoritmo genético, como o tamanho da população, o número de gerações e a taxa de mutação.

A população inicial é criada, onde cada indivíduo representa uma possível solução para o problema. Durante as gerações, o algoritmo seleciona os melhores indivíduos, realiza cruzamentos entre eles e aplica mutações para explorar novas soluções.

No final, o algoritmo retorna o melhor indivíduo encontrado, ou seja, a rota mais curta para o caixeiro viajante percorrer. Ele exibe essa rota e a distância total percorrida.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.