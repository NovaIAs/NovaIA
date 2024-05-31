```go
// Ce programme détecte les cycles dans un graphe.

package main

import (
	"fmt"
	"log"
)

// Graphe représente un graphe orienté.
type Graphe struct {
	Sommets map[string]map[string]bool // Sommets et leurs voisins.
}

// NouveauGraphe crée un nouveau graphe.
func NouveauGraphe() *Graphe {
	return &Graphe{
		Sommets: make(map[string]map[string]bool),
	}
}

// AjouterSommet ajoute un sommet au graphe.
func (g *Graphe) AjouterSommet(sommet string) {
	if _, ok := g.Sommets[sommet]; !ok {
		g.Sommets[sommet] = make(map[string]bool)
	}
}

// AjouterArête ajoute une arête entre deux sommets.
func (g *Graphe) AjouterArête(src, dest string) {
	if _, ok := g.Sommets[src]; !ok {
		log.Fatalf("Sommet %s n'existe pas dans le graphe.", src)
	}
	if _, ok := g.Sommets[dest]; !ok {
		log.Fatalf("Sommet %s n'existe pas dans le graphe.", dest)
	}
	g.Sommets[src][dest] = true
}

// DétecterCycles utilise l'algorithme de profondeur pour détecter les cycles dans le graphe.
func (g *Graphe) DétecterCycles() bool {
	// Initialiser l'état des sommets.
	état := make(map[string]int) // 0 : non visité, 1 : en cours de visite, 2 : visité
	for sommet := range g.Sommets {
		état[sommet] = 0
	}

	// Recherche en profondeur récursive.
	for sommet := range g.Sommets {
		if détecterCycle(sommet, g, état) {
			return true
		}
	}
	return false
}

// détecterCycle effectue une recherche en profondeur récursive pour détecter un cycle à partir d'un sommet.
func détecterCycle(sommet string, g *Graphe, état map[string]int) bool {
	// Si le sommet est déjà en cours de visite, un cycle a été détecté.
	if état[sommet] == 1 {
		return true
	}

	// Marquer le sommet comme en cours de visite.
	état[sommet] = 1

	// Explorer les voisins du sommet.
	for voisin := range g.Sommets[sommet] {
		if détecterCycle(voisin, g, état) {
			return true
		}
	}

	// Marquer le sommet comme visité.
	état[sommet] = 2

	return false
}

// Exemple d'utilisation.
func main() {
	// Créer un graphe.
	g := NouveauGraphe()
	g.AjouterSommet("A")
	g.AjouterSommet("B")
	g.AjouterSommet("C")
	g.AjouterArête("A", "B")
	g.AjouterArête("B", "C")
	g.AjouterArête("C", "A")

	// Détecter les cycles.
	if g.DétecterCycles() {
		fmt.Println("Le graphe contient des cycles.")
	} else {
		fmt.Println("Le graphe ne contient pas de cycles.")
	}
}
```

**Explication du code :**

Ce code est une implémentation de l'algorithme de détection de cycles dans un graphe orienté. Voici une explication détaillée :

* **Graphe** : La structure de données `Graphe` représente un graphe orienté à l'aide d'une carte de cartes. Les clés externes représentent les sommets, et les clés internes représentent les voisins des sommets.

* **NouveauGraphe** : Cette fonction crée une nouvelle instance de graphe.

* **AjouterSommet** : Cette méthode ajoute un sommet au graphe s'il n'existe pas déjà.

* **AjouterArête** : Cette méthode ajoute une arête entre deux sommets du graphe s'ils existent déjà.

* **DétecterCycles** : Cette méthode utilise l'algorithme de profondeur pour détecter les cycles dans le graphe. Il utilise une carte `état` pour suivre l'état de visite des sommets.

* **détecterCycle** : Cette fonction récursive est utilisée par `DétecterCycles` pour effectuer la recherche en profondeur. Elle explore les voisins d'un sommet et marque les sommets comme visités ou en cours de visite. Si un sommet est trouvé en cours de visite, un cycle a été détecté.

* **Exemple d'utilisation** : L'exemple d'utilisation crée un graphe cyclique et appelle `DétecterCycles` pour détecter les cycles.