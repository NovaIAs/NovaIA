**Code complexe en C#**

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodeComplexe
{
    class Program
    {
        static void Main(string[] args)
        {
            // Déclaration et initialisation d'une liste de nombres premiers
            List<int> nombresPremiers = new List<int>() { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 };

            // Utilisation d'une expression lambda pour filtrer la liste des nombres premiers et obtenir les nombres impairs
            List<int> nombresPremiersImpairs = nombresPremiers.Where(x => x % 2 != 0).ToList();

            // Déclaration et initialisation d'un dictionnaire pour stocker les nombres premiers et leurs facteurs
            Dictionary<int, List<int>> facteursNombresPremiers = new Dictionary<int, List<int>>();

            // Calcul des facteurs de chaque nombre premier et stockage dans le dictionnaire
            foreach (int nombrePremier in nombresPremiers)
            {
                List<int> facteurs = new List<int>();
                for (int i = 2; i <= Math.Sqrt(nombrePremier); i++)
                {
                    if (nombrePremier % i == 0)
                    {
                        facteurs.Add(i);
                    }
                }
                facteursNombresPremiers[nombrePremier] = facteurs;
            }

            // Affichage des nombres premiers, de leurs facteurs et du nombre de facteurs
            foreach (KeyValuePair<int, List<int>> facteur in facteursNombresPremiers)
            {
                Console.WriteLine("Nombre premier : {0}", facteur.Key);
                Console.WriteLine("Facteurs : {0}", string.Join(", ", facteur.Value));
                Console.WriteLine("Nombre de facteurs : {0}", facteur.Value.Count);
            }

            // Utilisation de la méthode LINQ GroupBy pour regrouper les nombres premiers par le nombre de facteurs
            var nombresPremiersParNombreFacteurs = nombresPremiers.GroupBy(x => facteursNombresPremiers[x].Count);

            // Affichage des groupes de nombres premiers par nombre de facteurs
            foreach (var groupe in nombresPremiersParNombreFacteurs)
            {
                Console.WriteLine("Nombres premiers avec {0} facteurs :", groupe.Key);
                Console.WriteLine(string.Join(", ", groupe));
            }

            // Utilisation de la méthode LINQ Max pour trouver le nombre premier avec le plus de facteurs
            int nombrePremierAvecPlusFacteurs = nombresPremiers.Max(x => facteursNombresPremiers[x].Count);

            // Affichage du nombre premier avec le plus de facteurs
            Console.WriteLine("Nombre premier avec le plus de facteurs : {0}", nombrePremierAvecPlusFacteurs);
        }
    }
}
```

**Explication du code**

Ce code est un exemple complexe qui montre l'utilisation de diverses fonctionnalités de C#, notamment :

* **Listes génériques** (List<int>) pour stocker des collections de nombres.
* **Expressions lambda** (x => x % 2 != 0) pour filtrer et transformer des collections.
* **Dictionnaires** (Dictionary<int, List<int>>) pour stocker des paires clé-valeur.
* **LINQ (Language Integrated Query)** pour effectuer des requêtes sur des collections.
* **Regroupement** (GroupBy) pour regrouper des éléments par une clé commune.
* **Agrégation** (Max) pour trouver le maximum d'une collection.

Ce code spécifique effectue les opérations suivantes :

1. Il filtre une liste de nombres premiers pour obtenir les nombres premiers impairs.
2. Il calcule les facteurs de chaque nombre premier et stocke ces facteurs dans un dictionnaire.
3. Il affiche les nombres premiers, leurs facteurs et le nombre de facteurs pour chacun d'eux.
4. Il regroupe les nombres premiers par le nombre de facteurs.
5. Il trouve le nombre premier avec le plus de facteurs.

Ce code démontre les capacités puissantes de C# pour manipuler et analyser des données complexes et illustre l'utilisation de fonctionnalités avancées telles que LINQ et les expressions lambda.