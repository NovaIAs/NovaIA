**Programme complexe en Go pour le calcul de séries temporelles**

```go
package main

import (
    "context"
    "fmt"
    "log"
    "math/rand"
    "sync"
    "time"
)

// Paramètres du programme
const (
    // Nombre de séries temporelles à générer
    NbSeries = 100
    // Longueur de chaque série temporelle
    LongueurSerie = 1000
    // Nombre de threads pour le calcul parallèle
    NbThreads = 8
)

// Génère une série temporelle aléatoire
func genererSerie() []float64 {
    serie := make([]float64, LongueurSerie)
    for i := range serie {
        serie[i] = rand.Float64()
    }
    return serie
}

// Calcule la moyenne d'une série temporelle
func moyenneSerie(serie []float64) float64 {
    somme := 0.0
    for _, valeur := range serie {
        somme += valeur
    }
    return somme / float64(len(serie))
}

// Fonction principale du programme
func main() {
    // Création d'un canal pour récupérer les résultats
    resultats := make(chan []float64, NbSeries)

    // Création d'un groupe d'attente pour synchroniser les threads
    var wg sync.WaitGroup
    wg.Add(NbThreads)

    // Lancement des threads de calcul
    for i := 0; i < NbThreads; i++ {
        go func(id int) {
            defer wg.Done()
            // Génère une liste de séries temporelles
            series := make([][]float64, NbSeries/NbThreads)
            for j := 0; j < len(series); j++ {
                series[j] = genererSerie()
            }

            // Calcule les moyennes des séries temporelles
            moyennes := make([]float64, len(series))
            for j := range series {
                moyennes[j] = moyenneSerie(series[j])
            }

            // Envoie les résultats au canal
            resultats <- moyennes
        }(i)
    }

    // Récupération des résultats
    var moyennesTotales []float64
    ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
    defer cancel()
loop:
    for {
        select {
        case <-ctx.Done():
            log.Fatal("Timeout atteint")
        case moyennes := <-resultats:
            moyennesTotales = append(moyennesTotales, moyennes...)
            if len(moyennesTotales) == NbSeries {
                break loop
            }
        }
    }

    // Affichage des résultats
    fmt.Println("Moyennes des séries temporelles")
    for _, moyenne := range moyennesTotales {
        fmt.Println(moyenne)
    }
}
```

**Explication du code**

Ce programme génère un grand nombre de séries temporelles (100), chacune contenant 1000 valeurs aléatoires. Il utilise le calcul parallèle sur 8 threads pour accélérer le calcul des moyennes de chaque série.

* La fonction `genererSerie()` génère une série temporelle aléatoire.
* La fonction `moyenneSerie()` calcule la moyenne d'une série temporelle.
* La fonction principale crée un canal pour récupérer les résultats, lance les threads de calcul et récupère les moyennes calculées.
* Le canal `resultats` est utilisé pour synchroniser les threads et collecter les résultats.
* Un groupe d'attente est utilisé pour attendre que tous les threads aient terminé leur calcul.
* Le programme définit un délai d'attente de 5 minutes pour éviter les blocages indéfinis.
* Les résultats sont affichés dans la console.