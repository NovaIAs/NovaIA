Claro! Abaixo está um exemplo de um código complexo em C#, que utiliza uma abordagem de algoritmo genético para resolver um problema clássico de otimização, conhecido como o "Problema da Mochila".

```csharp
using System;
using System.Collections.Generic;

class Item
{
    public string Nome { get; set; }
    public int Peso { get; set; }
    public int Valor { get; set; }

    public Item(string nome, int peso, int valor)
    {
        Nome = nome;
        Peso = peso;
        Valor = valor;
    }
}

class AlgoritmoGenetico
{
    private List<Item> itens;
    private int capacidadeMochila;
    private int tamanhoPopulacao;
    private double taxaMutacao;

    public AlgoritmoGenetico(List<Item> itens, int capacidadeMochila, int tamanhoPopulacao, double taxaMutacao)
    {
        this.itens = itens;
        this.capacidadeMochila = capacidadeMochila;
        this.tamanhoPopulacao = tamanhoPopulacao;
        this.taxaMutacao = taxaMutacao;
    }

    public List<Item> Executar()
    {
        List<List<int>> populacao = GerarPopulacaoInicial();
        List<int> fitnessPopulacao = CalcularFitness(populacao);

        for (int geracao = 0; geracao < 100; geracao++)
        {
            List<List<int>> novaPopulacao = new List<List<int>>();

            for (int i = 0; i < tamanhoPopulacao; i++)
            {
                List<int> pai1 = SelecionarPai(populacao, fitnessPopulacao);
                List<int> pai2 = SelecionarPai(populacao, fitnessPopulacao);

                List<int> filho = Cruzar(pai1, pai2);
                filho = Mutar(filho);

                novaPopulacao.Add(filho);
            }

            populacao = novaPopulacao;
            fitnessPopulacao = CalcularFitness(populacao);
        }

        int indiceMelhorIndividuo = fitnessPopulacao.IndexOf(fitnessPopulacao.Max());
        return ConverterIndividuoParaItens(populacao[indiceMelhorIndividuo]);
    }

    private List<List<int>> GerarPopulacaoInicial()
    {
        List<List<int>> populacao = new List<List<int>>();

        for (int i = 0; i < tamanhoPopulacao; i++)
        {
            List<int> individuo = new List<int>();

            for (int j = 0; j < itens.Count; j++)
            {
                individuo.Add(new Random().Next(0, 2));
            }

            populacao.Add(individuo);
        }

        return populacao;
    }

    private List<int> CalcularFitness(List<List<int>> populacao)
    {
        List<int> fitnessPopulacao = new List<int>();

        foreach (var individuo in populacao)
        {
            int peso = 0;
            int valor = 0;

            for (int i = 0; i < individuo.Count; i++)
            {
                if (individuo[i] == 1)
                {
                    peso += itens[i].Peso;
                    valor += itens[i].Valor;
                }
            }

            if (peso > capacidadeMochila)
            {
                valor = 0;
            }

            fitnessPopulacao.Add(valor);
        }

        return fitnessPopulacao;
    }

    private List<int> SelecionarPai(List<List<int>> populacao, List<int> fitnessPopulacao)
    {
        List<int> pai = new List<int>();

        for (int i = 0; i < 2; i++)
        {
            int indiceSorteado = new Random().Next(0, populacao.Count);
            pai = populacao[indiceSorteado];
        }

        return pai;
    }

    private List<int> Cruzar(List<int> pai1, List<int> pai2)
    {
        int pontoCorte = new Random().Next(1, pai1.Count - 1);
        List<int> filho = new List<int>();

        for (int i = 0; i < pai1.Count; i++)
        {
            if (i < pontoCorte)
            {
                filho.Add(pai1[i]);
            }
            else
            {
                filho.Add(pai2[i]);
            }
        }

        return filho;
    }

    private List<int> Mutar(List<int> individuo)
    {
        for (int i = 0; i < individuo.Count; i++)
        {
            if (new Random().NextDouble() < taxaMutacao)
            {
                individuo[i] = 1 - individuo[i];
            }
        }

        return individuo;
    }

    private List<Item> ConverterIndividuoParaItens(List<int> individuo)
    {
        List<Item> itensSelecionados = new List<Item>();

        for (int i = 0; i < individuo.Count; i++)
        {
            if (individuo[i] == 1)
            {
                itensSelecionados.Add(itens[i]);
            }
        }

        return itensSelecionados;
    }
}

class Program
{
    static void Main(string[] args)
    {
        List<Item> itens = new List<Item>
        {
            new Item("Item 1", 10, 60),
            new Item("Item 2", 20, 100),
            new Item("Item 3", 30, 120),
            new Item("Item 4", 40, 200),
            new Item("Item 5", 50, 240)
        };

        AlgoritmoGenetico algoritmoGenetico = new AlgoritmoGenetico(itens, 100, 50, 0.01);
        List<Item> melhoresItens = algoritmoGenetico.Executar();

        Console.WriteLine("Melhores itens selecionados:");

        foreach (var item in melhoresItens)
        {
            Console.WriteLine("Nome: " + item.Nome + ", Peso: " + item.Peso + ", Valor: " + item.Valor);
        }
    }
}
```

Neste exemplo, implementei um algoritmo genético para resolver o "Problema da Mochila". O objetivo é escolher os melhores itens para colocar em uma mochila, considerando a capacidade máxima da mochila e maximizando o valor total dos itens.

O algoritmo começa gerando uma população inicial de indivíduos (soluções candidatas), onde cada indivíduo é representado por um vetor de bits, em que cada bit indica a presença ou ausência de um determinado item na mochila. Em seguida, o algoritmo itera por várias gerações, aplicando a seleção dos pais, crossover (cruzamento) e mutação em cada geração. A seleção dos pais é feita de forma aleatória, com base na aptidão (fitness) dos indivíduos. O crossover é realizado a partir de um ponto de corte aleatório, combinando os genes dos pais para gerar um novo indivíduo. A mutação ocorre com uma probabilidade definida e altera aleatoriamente alguns genes.

O algoritmo continua iterando por um número fixo de gerações e, ao final, retorna a melhor solução encontrada, ou seja, o conjunto de itens que gera o maior valor dentro da capacidade da mochila.

No exemplo, criei uma classe `Item` para representar cada item com suas propriedades (nome, peso e valor). A classe `AlgoritmoGenetico` é responsável por realizar o algoritmo genético, com métodos para gerar a população inicial, calcular o fitness dos indivíduos, selecionar os pais, realizar o crossover, a mutação e converter o resultado final em itens selecionados. Na classe `Program`, faço a chamada ao algoritmo genético, passando os itens disponíveis, a capacidade da mochila, o tamanho da população e a taxa de mutação. Ao final, imprimo os melhores itens selecionados.

Esse código é apenas um exemplo complexo que utiliza algoritmos genéticos, mas há muitas outras abordagens e problemas que podem ser resolvidos através da programação.