Claro! Aqui está um exemplo de código em D que utiliza um algoritmo genético para resolver o problema do caixeiro viajante (TSP - Traveling Salesman Problem) de forma otimizada:

```d
import std.algorithm;
import std.random;
import std.stdio;

struct City
{
    int x, y;
}

int[][] createDistanceMatrix(City[] cities)
{
    int numCities = cities.length;
    int[][] distances = new int[][](numCities, numCities);

    foreach (i, ref city1; cities)
    {
        foreach (j, ref city2; cities)
        {
            distances[i][j] = abs(city1.x - city2.x) + abs(city1.y - city2.y);
        }
    }

    return distances;
}

int calculateRouteDistance(int[] route, int[][] distances)
{
    int distance = 0;

    for (int i = 0; i < route.length - 1; i++)
    {
        distance += distances[route[i]][route[i + 1]];
    }

    return distance;
}

int[] generateRandomRoute(int numCities)
{
    int[] route = new int[](numCities);

    foreach (i, ref city; route)
    {
        city = i;
    }

    return route.shuffle();
}

int[] crossover(int[] parent1, int[] parent2)
{
    int numCities = parent1.length;

    int startIndex = uniform(0, numCities / 2);
    int endIndex = uniform(startIndex + 1, numCities);

    int[] child = new int[](numCities);
    child[startIndex .. endIndex] = parent1[startIndex .. endIndex];

    foreach (i, ref city; child)
    {
        if (!child[startIndex .. endIndex].contains(city))
        {
            int index = 0;

            while (child[index] != -1)
            {
                index++;
            }

            child[index] = city;
        }
    }

    return child;
}

int[] mutate(int[] route)
{
    int numCities = route.length;

    int index1 = uniform(0, numCities);
    int index2 = uniform(0, numCities);

    route[index1] = route[index2];
    route[index2] = route[index1];

    return route;
}

int[] solveTSP(City[] cities, int populationSize, int numGenerations)
{
    int numCities = cities.length;
    int[][] distances = createDistanceMatrix(cities);

    int[][] population = new int[][](populationSize, numCities);

    foreach (i, ref individual; population)
    {
        individual = generateRandomRoute(numCities);
    }

    int[] bestRoute = population[0];
    int bestDistance = calculateRouteDistance(bestRoute, distances);

    for (int generation = 0; generation < numGenerations; generation++)
    {
        int[][] newPopulation = new int[][](populationSize, numCities);

        foreach (i, ref individual; newPopulation)
        {
            int[] parent1 = population[uniform(0, populationSize)];
            int[] parent2 = population[uniform(0, populationSize)];

            int[] child = crossover(parent1, parent2);

            if (uniform(0, 100) < 5)
            {
                child = mutate(child);
            }

            individual = child;
        }

        population = newPopulation;

        foreach (i, ref individual; population)
        {
            int distance = calculateRouteDistance(individual, distances);

            if (distance < bestDistance)
            {
                bestRoute = individual;
                bestDistance = distance;
            }
        }
    }

    return bestRoute;
}

void main()
{
    City[] cities =
    [
        City(0, 0),    // Cidade 0
        City(1, 2),    // Cidade 1
        City(3, 1),    // Cidade 2
        City(4, 3),    // Cidade 3
        City(2, 4)     // Cidade 4
    ];

    int populationSize = 100;
    int numGenerations = 1000;

    int[] bestRoute = solveTSP(cities, populationSize, numGenerations);

    writeln("Melhor rota encontrada: ", bestRoute);
}
```

Neste código, implementei um algoritmo genético para resolver o problema do caixeiro viajante. O algoritmo começa criando uma matriz de distâncias entre as cidades fornecidas. Em seguida, gera uma população inicial onde cada indivíduo é uma rota aleatória que visita todas as cidades. A cada geração, são selecionados dois pais aleatórios da população atual para realizar um cruzamento (crossover), gerando um novo indivíduo. Também há uma chance de mutação para cada novo indivíduo gerado. A nova população é então atualizada e o processo se repete até o número máximo de gerações ser atingido. Ao final, é retornado o melhor indivíduo encontrado durante o processo, ou seja, a melhor rota encontrada para o problema do caixeiro viajante.

Espero que este código atenda às suas expectativas! Se você tiver mais alguma solicitação, estou à disposição.