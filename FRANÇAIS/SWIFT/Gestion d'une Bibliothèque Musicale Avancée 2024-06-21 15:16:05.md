**Gestion d'une bibliothèque musicale complexe**

Le code suivant permet de gérer une bibliothèque musicale complexe avec plusieurs niveaux de hiérarchie, des métadonnées avancées et des opérations de recherche et de tri sophistiquées.

```swift
// Modèle d'album
struct Album {
    let titre: String
    let artiste: String
    let annee: Int
    let genre: [String]
    let chansons: [Chanson]
}

// Modèle de chanson
struct Chanson {
    let titre: String
    let duree: Double
    let compositeur: [String]
    let paroles: String
}

// Modèle de bibliothèque musicale
class BibliothequeMusicale {
    
    var albums: [Album]
    
    // Constructeur
    init(albums: [Album]) {
        self.albums = albums
    }
    
    // Rechercher un album par titre
    func rechercherAlbum(titre: String) -> Album? {
        return albums.first(where: { $0.titre == titre })
    }
    
    // Rechercher une chanson par titre dans tous les albums
    func rechercherChanson(titre: String) -> Chanson? {
        for album in albums {
            if let chanson = album.chansons.first(where: { $0.titre == titre }) {
                return chanson
            }
        }
        return nil
    }
    
    // Trier les albums par année
    func trierAlbumsParAnnee() {
        albums.sort(by: { $0.annee < $1.annee })
    }
    
    // Trier les chansons par durée
    func trierChansonsParDuree() {
        for album in albums {
            album.chansons.sort(by: { $0.duree < $1.duree })
        }
    }
    
    // Ajouter un nouvel album
    func ajouterAlbum(album: Album) {
        albums.append(album)
    }
    
    // Supprimer un album
    func supprimerAlbum(titre: String) {
        albums.removeAll(where: { $0.titre == titre })
    }
}

// Exemple d'utilisation
let album1 = Album(titre: "Un album magnifique", artiste: "Artiste 1", annee: 2020, genre: ["Pop", "Rock"], chansons: [
    Chanson(titre: "Chanson 1", duree: 2.5, compositeur: ["Compositeur 1"], paroles: "Paroles de la chanson 1"),
    Chanson(titre: "Chanson 2", duree: 3.0, compositeur: ["Compositeur 1", "Compositeur 2"], paroles: "Paroles de la chanson 2")
])

let album2 = Album(titre: "Un autre album génial", artiste: "Artiste 2", annee: 2021, genre: ["Jazz", "Blues"], chansons: [
    Chanson(titre: "Chanson 3", duree: 4.0, compositeur: ["Compositeur 3"], paroles: "Paroles de la chanson 3"),
    Chanson(titre: "Chanson 4", duree: 5.0, compositeur: ["Compositeur 3", "Compositeur 4"], paroles: "Paroles de la chanson 4")
])

let bibliotheque = BibliothequeMusicale(albums: [album1, album2])

// Rechercher l'album "Un album magnifique"
let albumTrouve = bibliotheque.rechercherAlbum(titre: "Un album magnifique")

// Rechercher la chanson "Chanson 3" dans tous les albums
let chansonTrouvee = bibliotheque.rechercherChanson(titre: "Chanson 3")

// Trier les albums par année
bibliotheque.trierAlbumsParAnnee()

// Trier les chansons par durée
bibliotheque.trierChansonsParDuree()

// Ajouter un nouvel album
bibliotheque.ajouterAlbum(album: Album(titre: "Un troisième album", artiste: "Artiste 3", annee: 2022, genre: ["Classique", "Orchestral"], chansons: []))

// Supprimer l'album "Un autre album génial"
bibliotheque.supprimerAlbum(titre: "Un autre album génial")
```

**Explication du code:**

* Les modèles `Album` et `Chanson` représentent les entités de base de la bibliothèque musicale.
* La classe `BibliothequeMusicale` est responsable de la gestion de la collection d'albums.
* Les méthodes `rechercherAlbum` et `rechercherChanson` permettent de rechercher des entités spécifiques.
* Les méthodes `trierAlbumsParAnnee` et `trierChansonsParDuree` trient la bibliothèque selon les critères spécifiés.
* Les méthodes `ajouterAlbum` et `supprimerAlbum` modifient la bibliothèque dynamiquement.

Ce code fournit un cadre flexible pour gérer des bibliothèques musicales complexes avec une hiérarchie, des métadonnées et des opérations de recherche et de tri avancées.