Claro! Aqui está um código complexo em C# que realiza a busca e ordenação de um array de números inteiros utilizando a técnica do Merge Sort:

```csharp
using System;

class Program
{
    static void Main(string[] args)
    {
        int[] array = { 9, 7, 5, 3, 1, 8, 6, 4, 2 };

        Console.WriteLine("Array original:");
        PrintArray(array);

        MergeSort(array, 0, array.Length - 1);

        Console.WriteLine("Array ordenado:");
        PrintArray(array);

        Console.WriteLine("Digite o número que deseja buscar:");
        int num = int.Parse(Console.ReadLine());

        int index = BinarySearch(array, num);

        if (index >= 0)
        {
            Console.WriteLine("O número {0} foi encontrado na posição {1}.", num, index);
        }
        else
        {
            Console.WriteLine("O número {0} não foi encontrado.", num);
        }
    }

    static void MergeSort(int[] array, int left, int right)
    {
        if (left < right)
        {
            int middle = (left + right) / 2;

            MergeSort(array, left, middle);
            MergeSort(array, middle + 1, right);

            Merge(array, left, middle, right);
        }
    }

    static void Merge(int[] array, int left, int middle, int right)
    {
        int[] temp = new int[right - left + 1];

        int i = left;
        int j = middle + 1;
        int k = 0;

        while (i <= middle && j <= right)
        {
            if (array[i] <= array[j])
            {
                temp[k] = array[i];
                i++;
            }
            else
            {
                temp[k] = array[j];
                j++;
            }

            k++;
        }

        while (i <= middle)
        {
            temp[k] = array[i];
            i++;
            k++;
        }

        while (j <= right)
        {
            temp[k] = array[j];
            j++;
            k++;
        }

        for (int x = 0; x < temp.Length; x++)
        {
            array[left + x] = temp[x];
        }
    }

    static int BinarySearch(int[] array, int num)
    {
        int left = 0;
        int right = array.Length - 1;

        while (left <= right)
        {
            int middle = (left + right) / 2;

            if (array[middle] == num)
            {
                return middle;
            }
            else if (array[middle] < num)
            {
                left = middle + 1;
            }
            else
            {
                right = middle - 1;
            }
        }

        return -1;
    }

    static void PrintArray(int[] array)
    {
        for (int i = 0; i < array.Length; i++)
        {
            Console.Write(array[i] + " ");
        }
        Console.WriteLine();
    }
}
```

Este código realiza a ordenação de um array de números inteiros utilizando o algoritmo Merge Sort. Ele divide o array em metades menores, ordena cada metade recursivamente e depois junta as duas metades em ordem crescente. Ao final da ordenação, o array é exibido na tela.

Após a ordenação, o código solicita ao usuário que digite um número para buscar no array. Em seguida, é realizada uma busca binária para encontrar o número digitado. Se o número for encontrado, a posição é exibida. Caso contrário, uma mensagem informando que o número não foi encontrado é exibida.

Espero que este código atenda às suas necessidades de complexidade e diferenciação!