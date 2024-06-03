```csharp
// Implémentation d'un algorithme génétique pour résoudre un problème d'optimisation combinatoire complexe

using System;
using System.Collections.Generic;

namespace AlgoGenetiqueOptimisation
{
    // Représentation d'une solution candidate
    public class Solution
    {
        public int[] Genes { get; set; }
        public double Fitness { get; set; }

        public Solution(int[] genes, double fitness)
        {
            Genes = genes;
            Fitness = fitness;
        }
    }

    // Classe principale de l'algorithme génétique
    public class AlgorithmeGenetique
    {
        // Paramètres de l'algorithme
        private int PopulationSize;
        private double MutationRate;
        private double CrossoverRate;

        // Population actuelle
        private List<Solution> Population;

        // Fonction d'évaluation pour calculer l'aptitude d'une solution
        private Func<int[], double> FonctionEvaluation;

        // Constructeur
        public AlgorithmeGenetique(int populationSize, double mutationRate, double crossoverRate, Func<int[], double> fonctionEvaluation)
        {
            PopulationSize = populationSize;
            MutationRate = mutationRate;
            CrossoverRate = crossoverRate;
            FonctionEvaluation = fonctionEvaluation;
        }

        // Initialisation de la population
        public void InitialiserPopulation(int longueurGene)
        {
            Population = new List<Solution>();
            for (int i = 0; i < PopulationSize; i++)
            {
                int[] genes = new int[longueurGene];
                for (int j = 0; j < longueurGene; j++)
                {
                    genes[j] = new Random().Next(0, 2);
                }
                double fitness = FonctionEvaluation(genes);
                Population.Add(new Solution(genes, fitness));
            }
        }

        // Sélection des parents
        public List<Solution> Selection()
        {
            List<Solution> parents = new List<Solution>();

            // Sélection par roulette
            double totalFitness = 0;
            foreach (var solution in Population)
            {
                totalFitness += solution.Fitness;
            }

            for (int i = 0; i < PopulationSize; i++)
            {
                double r = new Random().NextDouble() * totalFitness;
                double currentFitness = 0;
                foreach (var solution in Population)
                {
                    currentFitness += solution.Fitness;
                    if (currentFitness >= r)
                    {
                        parents.Add(solution);
                        break;
                    }
                }
            }

            return parents;
        }

        // Croisement
        public Solution Croisement(Solution parent1, Solution parent2)
        {
            // Croisement par un point unique
            int point = new Random().Next(0, parent1.Genes.Length - 1);
            int[] genes = new int[parent1.Genes.Length];

            for (int i = 0; i < point; i++)
            {
                genes[i] = parent1.Genes[i];
            }

            for (int i = point; i < parent1.Genes.Length; i++)
            {
                genes[i] = parent2.Genes[i];
            }

            return new Solution(genes, FonctionEvaluation(genes));
        }

        // Mutation
        public Solution Mutation(Solution solution)
        {
            // Inversion d'un bit aléatoire
            int index = new Random().Next(0, solution.Genes.Length);
            solution.Genes[index] = 1 - solution.Genes[index];
            return solution;
        }

        // Remplacement
        public void Remplacement(List<Solution> population, List<Solution> enfants)
        {
            // Tri par aptitude décroissante
            population.Sort((x, y) => y.Fitness.CompareTo(x.Fitness));
            enfants.Sort((x, y) => y.Fitness.CompareTo(x.Fitness));

            // Remplacement des solutions les moins aptes par les enfants les plus aptes
            for (int i = 0; i < enfants.Count; i++)
            {
                population[i] = enfants[i];
            }
        }

        // Exécution de l'algorithme génétique
        public Solution Executer(int nombreGenerations)
        {
            for (int generation = 0; generation < nombreGenerations; generation++)
            {
                // Sélection des parents
                List<Solution> parents = Selection();

                // Croisement et mutation
                List<Solution> enfants = new List<Solution>();
                for (int i = 0; i < parents.Count; i += 2)
                {
                    Solution enfant1 = Croisement(parents[i], parents[i + 1]);
                    if (new Random().NextDouble() < MutationRate)
                    {
                        enfant1 = Mutation(enfant1);
                    }
                    enfants.Add(enfant1);

                    if (i < parents.Count - 2)
                    {
                        Solution enfant2 = Croisement(parents[i + 1], parents[i + 2]);
                        if (new Random().NextDouble() < MutationRate)
                        {
                            enfant2 = Mutation(enfant2);
                        }
                        enfants.Add(enfant2);
                    }
                }

                // Remplacement
                Remplacement(Population, enfants);

                // Affichage de la meilleure solution de chaque génération
                var meilleureSolution = Population[0];
                Console.WriteLine($"Génération {generation + 1} : Meilleure solution : {meilleureSolution.Fitness}");
            }

            return Population[0];
        }
    }

    // Fonction exemple pour évaluer la solution d'un problème de sac à dos
    public static class FonctionEvaluation
    {
        public static double EvaluationSacADos(int[] genes)
        {
            // Liste des objets et leurs poids et valeurs respectifs
            var objets = new List<(int Poids, int Valeur)>
            {
                (10, 10),
                (15, 12),
                (20, 15),
                (25, 18),
                (30, 20)
            };

            // Capacité maximale du sac à dos
            int capacite = 50;

            // Initialisation des variables
            int poidsTotal = 0;
            int valeurTotale = 0;

            // Parcours des objets et ajout au sac à dos
            for (int i = 0; i < genes.Length; i++)
            {
                if (genes[i] == 1)
                {
                    var objet = objets[i];
                    poidsTotal += objet.Poids;
                    valeurTotale += objet.Valeur;
                }
            }

            // Si le poids total dépasse la capacité, la valeur est négative
            if (poidsTotal > capacite)
            {
                valeurTotale = -valeurTotale;
            }

            return valeurTotale;
        }
    }

    // Programme principal
    public class Program
    {
        public static void Main(string[] args)
        {
            // Paramètres de l'algorithme génétique
            int populationSize = 100;
            double mutationRate = 0.05;
            double crossoverRate = 0.7;

            // Initialisation de l'algorithme génétique
            var algoGenetique = new AlgorithmeGenetique(populationSize, mutationRate, crossoverRate, FonctionEvaluation.EvaluationSacADos);

            // Initialisation de la population
            algoGenetique.InitialiserPopulation(5);

            // Exécution de l'algorithme génétique
            var meilleureSolution = algoGenetique.Executer(100);

            // Affichage de la meilleure solution
            Console.WriteLine($"Meilleure solution trouvée : {string.Join(",", meilleureSolution.Genes)}");
            Console.WriteLine($"Fitness de la meilleure solution : {meilleureSolution.Fitness}");
        }
    }
}
```

**Explication du code :**

Ce code implémente un algorithme génétique pour résoudre un problème de sac à dos, où l'objectif est de trouver la meilleure combinaison d'objets à mettre dans un sac à dos de capacité limitée afin de maximiser la valeur totale des objets.

**Fonction d'évaluation (FonctionEvaluation.EvaluationSacADos)** :

Cette fonction prend un tableau de gènes (0 ou 1 pour chaque objet) et calcule l'aptitude de la solution. Elle ajoute les objets correspondants au sac à dos et calcule la valeur totale. Si le poids total dépasse la capacité, la valeur devient négative.

**Algorithme génétique (AlgorithmeGenetique)** :

* **Initialisation de la population** : crée une population initiale de solutions aléatoires.
* **Sélection** : sélectionne les parents pour le croisement à l'aide de la sélection par roulette.
* **Croisement** : crée