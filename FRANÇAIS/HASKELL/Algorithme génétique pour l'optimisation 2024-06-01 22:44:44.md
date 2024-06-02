**Algorithme génétique pour la recherche d'optimisation**

Cet algorithme génétique implémente une stratégie évolutionnaire pour résoudre des problèmes d'optimisation. Il utilise des concepts de sélection, croisement et mutation pour optimiser une fonction objectif.

```haskell
import Data.List (unfoldr, maximumBy)
import System.Random

-- Type de données pour représenter un individu (solution candidate)
data Individu a = Individu {
    genome       :: [a],        -- Génotype (liste de valeurs)
    fitness     :: Double      -- Aptitude (valeur d'évaluation)
} deriving (Show)

-- Créer une population initiale
nouvellePopulation :: Int -> (Double, Double) -> [Individu Double]
nouvellePopulation n (minVal, maxVal) = unfoldr (\_ -> Just (randomPopulation minVal maxVal n))

-- Créer une population aléatoire
randomPopulation :: Double -> Double -> Int -> [Individu Double]
randomPopulation minVal maxVal n = map (\_ -> Individu {genome = randomList n minVal maxVal, fitness = 0.0}) [1..n]

-- Liste aléatoire de valeurs
randomList :: Int -> Double -> Double -> [Double]
randomList n minVal maxVal = take n $ randomRs (minVal, maxVal) gen

-- Générateur de nombres aléatoires
gen :: StdGen

-- Évaluer l'aptitude d'un individu
evaluerFitness :: (Double -> Double) -> Individu Double -> Double
evaluerFitness fonction individu = fonction (sum $ genome individu)

-- Fonction de comparaison d'individus par aptitude
comparerFitness :: Individu Double -> Individu Double -> Ordering
comparerFitness a b = compare (fitness a) (fitness b)

-- Sélection par tournoi
selectionnerTournoi :: Int -> [Individu Double] -> [Individu Double]
selectionnerTournoi n population =
    maximumBy (\_ -> randomProbabilityGen) . replicateM n $ do
        randomList n minBound maxBound <- randomRs ((minBound, maxBound) :: (Double, Double)) gen
        return $ population !! floor . (* size) $ uncurry minBound maxBound randomList

-- Croisement par point médian
croiserMediane :: Individu Double -> Individu Double -> Individu Double
croiserMediane parent1 parent2 =
    let medianIndex = floor . (/ 2) $ length (genome parent1)
    in Individu {
        genome = take medianIndex (genome parent1) ++ drop medianIndex (genome parent2),
        fitness = 0.0
    }

-- Mutation avec probabilité uniforme
muterUniforme :: Double -> Individu Double -> Individu Double
muterUniforme p individu =
    let pointIndex = floor . (* length (genome individu)) $ randomGen
    in Individu {
        genome = take pointIndex (genome individu) ++ [randomValue minBound maxBound] ++ drop (pointIndex + 1) (genome individu),
        fitness = 0.0
    }

-- Boucle principale de l'algorithme génétique
algorithmeGenetique :: (Double -> Double) -> Int -> Double -> Double -> [Individu Double]
algorithmeGenetique fonction populationSize nbGenerations mutationProbability =
    let generationInitiale = nouvellePopulation populationSize (minBound, maxBound)
        fitnessEvaluation = map (evaluerFitness fonction) generationInitiale
        selection = selectionnerTournoi (populationSize `div` 4) generationInitiale
        descendance = zipWith croiserMediane selection (tail selection)
        populationSuivante = map muterUniforme mutationProbability descendance
        evals = fitnessEvaluation ++ map (evaluerFitness fonction) populationSuivante
    in take generationCount $ iterate (\result -> sortOn (fitness . fst) $ zip fitnessEvaluation (tail result)) (zip evals populationSuivante)
    where
        generationCount = nbGenerations
        fitnessEvaluation = map (evaluerFitness fonction) generationInitiale
        minBound = -100.0
        maxBound = 100.0
```

**Explication du code**

* **Type de données Individu:** Représente une solution candidate avec un génotype (liste de valeurs) et une aptitude (valeur d'évaluation).
* **NouvellePopulation:** Génère une population initiale aléatoire de solutions candidates.
* **RandomPopulation:** Crée une population aléatoire dans une plage de valeurs spécifiée.
* **RandomList:** Génère une liste de valeurs aléatoires dans une plage spécifiée.
* **EvaluerFitness:** Évalue l'aptitude d'un individu en fonction de sa valeur.
* **ComparerFitness:** Compare les individus par leur aptitude.
* **SélectionnerTournoi:** Sélectionne les individus les plus aptes de la population par tournoi.
* **CroiserMediane:** Croise deux individus par un point médian.
* **MuterUniforme:** Muter un individu avec une probabilité uniforme.
* **AlgorithmeGenetique:** Boucle principale de l'algorithme génétique qui sélectionne, croise, mute et évalue les individus pour trouver la meilleure solution.