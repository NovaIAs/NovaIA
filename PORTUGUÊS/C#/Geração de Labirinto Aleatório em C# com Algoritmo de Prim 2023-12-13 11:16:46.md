Claro! Aqui está um exemplo de código complexo em C# que gera um labirinto utilizando o algoritmo de Prim:

```csharp
using System;
using System.Collections.Generic;

class Program
{
    static int[,] GenerateMaze(int width, int height)
    {
        int[,] maze = new int[width, height];

        for (int i = 0; i < width; i++)
        {
            for (int j = 0; j < height; j++)
            {
                maze[i, j] = 1; // Paredes
            }
        }

        Random random = new Random();
        int startX = random.Next(0, width);
        int startY = random.Next(0, height);
        maze[startX, startY] = 0; // Início

        List<Tuple<int, int>> walls = new List<Tuple<int, int>>();
        walls.Add(new Tuple<int, int>(startX, startY));

        while (walls.Count > 0)
        {
            int index = random.Next(0, walls.Count);
            Tuple<int, int> current = walls[index];
            walls.RemoveAt(index);

            int currentX = current.Item1;
            int currentY = current.Item2;

            List<Tuple<int, int>> neighbors = new List<Tuple<int, int>>();
            if (currentX > 1)
                neighbors.Add(new Tuple<int, int>(currentX - 2, currentY));
            if (currentX < width - 2)
                neighbors.Add(new Tuple<int, int>(currentX + 2, currentY));
            if (currentY > 1)
                neighbors.Add(new Tuple<int, int>(currentX, currentY - 2));
            if (currentY < height - 2)
                neighbors.Add(new Tuple<int, int>(currentX, currentY + 2));

            foreach (Tuple<int, int> neighbor in neighbors)
            {
                int neighborX = neighbor.Item1;
                int neighborY = neighbor.Item2;

                if (maze[neighborX, neighborY] == 1)
                {
                    maze[neighborX, neighborY] = 0;
                    maze[currentX + (neighborX - currentX) / 2, currentY + (neighborY - currentY) / 2] = 0;
                    walls.Add(new Tuple<int, int>(neighborX, neighborY));
                }
            }
        }

        return maze;
    }

    static void Main()
    {
        int width = 21;
        int height = 21;

        int[,] maze = GenerateMaze(width, height);

        for (int i = 0; i < width; i++)
        {
            for (int j = 0; j < height; j++)
            {
                Console.Write(maze[i, j] == 1 ? "█" : " ");
            }
            Console.WriteLine();
        }
    }
}
```

Este código gera um labirinto aleatório utilizando o algoritmo de Prim. Ele começa criando uma matriz de tamanho `width` por `height` onde cada posição representa uma célula do labirinto. Inicialmente, todas as células são preenchidas com valor 1, que representa uma parede.

Em seguida, é escolhida aleatoriamente uma célula inicial para iniciar a construção do labirinto. Essa célula é marcada como início (valor 0) e adicionada a uma lista de paredes.

Enquanto houver paredes na lista, o código seleciona aleatoriamente uma parede da lista e verifica se há células vizinhas que ainda não foram visitadas. Se houver, uma parede é derrubada (valor 0) e a célula vizinha correspondente também é marcada como visitada.

Esse processo continua até que todas as paredes sejam visitadas, resultando em um labirinto completo. Por fim, o código exibe o labirinto na tela, utilizando "█" para representar as paredes e espaços em branco para representar os caminhos.