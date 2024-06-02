**Programme Go complexe pour la gestion de données et la visualisation**

Ce code Go complexe illustre les concepts avancés de gestion de données, de concurrence et de visualisation. Il montre comment manipuler des données structurées, effectuer des calculs parallèles et présenter les résultats de manière efficace.

```go
package main

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"log"
	"math/rand"
	"os"
	"runtime"
	"sync"
	"sync/atomic"
	"time"

	"github.com/gonum/matrix/mat64"
	"gonum.org/v1/plot"
	"gonum.org/v1/plot/plotter"
	"gonum.org/v1/plot/vg"
)

// Configuration du programme
const (
	NumDonnées         = 1000000
	NumTravailleurs    = 10
	InterrogationEnMasse = false
)

// Structure de données représentant un enregistrement de données
type Enregistrement struct {
	Clé    uint64  // Clé unique de l'enregistrement
	Valeur float64 // Valeur associée à l'enregistrement
}

// Canal pour transmettre les enregistrements traités
var enregistrements chan *Enregistrement

// Variables atomiques pour compter les enregistrements traités
var enregistrementsTraités, enregistrementsÉcartés uint64

// Fonction principale
func main() {
	// Initialisation du canal d'enregistrements
	enregistrements = make(chan *Enregistrement, NumTravailleurs)

	// Lancer une goroutine pour lire les données du fichier
	go lireFichier()

	// Démarrer des goroutines pour traiter les enregistrements
	var wg sync.WaitGroup
	pour i := 0; i < NumTravailleurs; i++ {
		wg.Add(1)
		go traiterEnregistrements(&wg)
	}

	// Attendre que toutes les goroutines se terminent
	wg.Wait()

	// Afficher les statistiques
	fmt.Printf("Enregistrements traités : %d\n", enregistrementsTraités)
	fmt.Printf("Enregistrements écartés : %d\n", enregistrementsÉcartés)

	// Créer un graphique à partir des données traitées
	créerGraphique()
}

// Fonction pour lire les données d'un fichier
func lireFichier() {
	// Ouvrir le fichier
	fichier, err := os.Open("donnees.txt")
	if err != nil {
		log.Fatalf("Impossible d'ouvrir le fichier : %v", err)
	}
	defer fichier.Close()

	// Créer un scanner pour lire le fichier ligne par ligne
	scanner := bufio.NewScanner(fichier)

	// Parcourir le fichier et envoyer les enregistrements sur le canal
	for scanner.Scan() {
		// Convertir la ligne en un enregistrement
		enregistrement, err := convertirLigne(scanner.Text())
		if err != nil {
			log.Fatalf("Impossible de convertir la ligne : %v", err)
		}

		// Envoyer l'enregistrement sur le canal
		enregistrements <- enregistrement
	}

	if err := scanner.Err(); err != nil {
		log.Fatalf("Erreur lors de la lecture du fichier : %v", err)
	}

	// Fermer le canal pour indiquer que la lecture est terminée
	close(enregistrements)
}

// Fonction pour convertir une ligne de texte en un enregistrement
func convertirLigne(ligne string) (*Enregistrement, error) {
	// Diviser la ligne en champs
	champs := strings.Split(ligne, "\t")
	if len(champs) != 2 {
		return nil, errors.New("format de ligne incorrect")
	}

	// Convertir la clé et la valeur
	clé, err := strconv.ParseUint(champs[0], 10, 64)
	if err != nil {
		return nil, errors.New("clé invalide")
	}
	valeur, err := strconv.ParseFloat(champs[1], 64)
	if err != nil {
		return nil, errors.New("valeur invalide")
	}

	// Créer et retourner l'enregistrement
	return &Enregistrement{Clé: clé, Valeur: valeur}, nil
}

// Fonction pour traiter les enregistrements
func traiterEnregistrements(wg *sync.WaitGroup) {
	// Décrémenter le compteur d'attente
	defer wg.Done()

	// Boucler sur les enregistrements reçus
	for enregistrement := range enregistrements {
		// Effectuer des calculs sur l'enregistrement
		valeur := enregistrement.Valeur * rand.Float64()

		// Vérifier si la valeur est dans la plage acceptable
		if valeur < 0 || valeur > 1 {
			// Incrémenter le compteur d'enregistrements écartés
			atomic.AddUint64(&enregistrementsÉcartés, 1)
			continue
		}

		// Incrémenter le compteur d'enregistrements traités
		atomic.AddUint64(&enregistrementsTraités, 1)
	}
}

// Fonction pour créer un graphique à partir des données traitées
func créerGraphique() {
	// Créer un nouveau graphique
	p, err := plot.New()
	if err != nil {
		log.Fatalf("Impossible de créer le graphique : %v", err)
	}

	// Créer un plotter de points
	pts := plotter.XYs{}
	for i := 0; i < NumDonnées; i++ {
		clé := rand.Uint64()
		valeur := rand.Float64()

		// Trouver l'enregistrement avec la clé
		enregistrement := trouverEnregistrement(clé)

		// Si l'enregistrement a été trouvé, ajouter son point au plotter
		if enregistrement != nil {
			pts = append(pts, plotter.XY{X: float64(clé), Y: enregistrement.Valeur})
		}
	}

	// Ajouter le plotter au graphique
	p.Add(plotter.NewScatter(pts))

	// Enregistrer le graphique au format PNG
	if err := p.Save(400, 400, "graphique.png"); err != nil {
		log.Fatalf("Impossible d'enregistrer le graphique : %v", err)
	}
}

// Fonction pour trouver un enregistrement par sa clé (pour la démonstration)
func trouverEnregistrement(clé uint64) *Enregistrement {
	// Simuler l'accès aux données (ici, nous allons supposer que nous avons accès à un cache ou à une base de données)
	// Dans un scénario réel, cette fonction pourrait effectuer une requête ou une recherche dans un index.

	// Créer un contexte pour la requête
	ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
	defer cancel()

	// Lancer une goroutine pour effectuer la requête en parallèle
	var enregistrement *Enregistrement
	var err error
	go func() {
		// Requête de base de données simulée
		enregistrement, err = simulerRequête(ctx, clé)
	}()

	// Attendre le résultat de la requête ou le délai d'expiration du contexte
	select {
	case <-ctx.Done():
		log.Printf("Requête pour la clé %d expirée\n", clé)
		return nil
	case <-time.After(50 * time.Millisecond):
		enregistrement, err = attentePourRésultat(enregistrement, err)
		return enregistrement
	}
}

// Fonction pour simuler une requête de base de données (pour la démonstration)
func simulerRequête(ctx context.Context, clé uint64) (*Enregistrement, error) {
	// Délai aléatoire pour simuler le temps de requête
	time.Sleep(time.Duration(rand.Intn(100)) * time.Millisecond)

	// Retourner un enregistrement factice
	return &Enregistrement{Clé: clé, Valeur: rand.Float64()}, nil
}

// Fonction pour attendre le résultat d'une requête en parallèle (pour la démonstration)
func attentePourRésultat(enregistrement *Enregistrement, err error) (*Enregistrement, error) {
	// Vérifier si le contexte a été annulé
	select {
	case <-ctx.Done():
		return nil, errors.New("requête annulée")
	default:
	}

	// Si aucune erreur, retourner l'enregistrement
	if err == nil {
		return enregistrement, nil
	}

	// Sinon, retourner l'erreur
	return nil, err
}
```

**Explication du code**

Ce code Go complexe illustre les concepts suivants :

* **Gestion de données massive :** Gère des ensembles de données volumineux (1 million d'enregistrements) de manière efficace en utilisant la concurrence et