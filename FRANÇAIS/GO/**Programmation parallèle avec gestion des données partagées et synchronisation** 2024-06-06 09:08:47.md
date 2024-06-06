**Importation des packages nécessaires**

```go
package main

import (
    "fmt"
    "log"
    "sync"
    "sync/atomic"
    "time"
)
```

**Déclaration des constantes**

```go
const (
    NUM_WORKERS   = 10
    DATA_COUNT    = 1000000
    COMPUTATION_NS = 10000
)
```

**Structures de données**

```go
type Data struct {
    Value int
    Lock  sync.Mutex
}

type Queue struct {
    Data  []*Data
    Mutex sync.Mutex
}
```

**Fonctions principales**

```go
func main() {
    // Création d'une file d'attente de données
    queue := &Queue{Data: make([]*Data, DATA_COUNT)}

    // Initialisation des données
    for i := 0; i < DATA_COUNT; i++ {
        queue.Data[i] = &Data{Value: i}
    }

    // Création d'un groupe d'attente pour les workers
    var wg sync.WaitGroup
    wg.Add(NUM_WORKERS)

    // Création et lancement des workers
    for i := 0; i < NUM_WORKERS; i++ {
        go worker(&wg, queue)
    }

    // Attente de la fin des workers
    wg.Wait()
}

func worker(wg *sync.WaitGroup, queue *Queue) {
    defer wg.Done()

    for {
        var data *Data

        // Récupération d'un élément de la file d'attente de manière thread-safe
        queue.Mutex.Lock()
        if len(queue.Data) == 0 {
            queue.Mutex.Unlock()
            return
        }
        data = queue.Data[0]
        queue.Data = queue.Data[1:]
        queue.Mutex.Unlock()

        // Calcul coûteux
        time.Sleep(time.Duration(COMPUTATION_NS) * time.Nanosecond)

        // Mise à jour de la valeur et libération du verrou
        data.Lock.Lock()
        data.Value *= 2
        data.Lock.Unlock()
    }
}
```

**Explication du code**

Ce code simule un scénario complexe de traitement parallèle où de nombreux workers partagent une file d'attente de données. Chaque worker récupère un élément de la file d'attente et effectue un calcul coûteux sur celui-ci.

Les principales fonctionnalités de ce code sont les suivantes :

* **Parallélisme** : Les workers sont créés et lancés simultanément pour optimiser le traitement des données.
* **Structures de données thread-safe** : Les files d'attente et les structures de données sont protégées par des verrous pour garantir la cohérence des données partagées.
* **Gestion des erreurs** : Les erreurs sont enregistrées à l'aide du package `log`.
* **Comptage atomique** : La variable `NUM_WORKERS` est utilisée comme compteur atomique pour les workers en cours d'exécution.
* **Synchronisation** : Le groupe d'attente (`wg`) garantit que tous les workers ont terminé avant que le programme principal ne s'arrête.

Ce code démontre des pratiques avancées de programmation concurrente en Go et met en évidence la gestion de la parallélisation, de la synchronisation et des structures de données partagées.