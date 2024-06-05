**Objet :** Gestion d'une base de données de films

**Classe :** Film

```smalltalk
Classe Film
    Attributs : titre réalisateur année
    Méthodes :
        init: titre unRealisateur uneAnnée
            "Crée un nouveau film avec les informations fournies."
            titre value: titre.
            réalisateur value: unRealisateur.
            année value: uneAnnée.
        titre -> String
            "Renvoie le titre du film."
            titre value.
        réalisateur -> Personne
            "Renvoie le réalisateur du film."
            réalisateur value.
        année -> Integer
            "Renvoie l'année de sortie du film."
            année value.
End classe Film.
```

**Classe :** Personne

```smalltalk
Classe Personne
    Attributs : nom prénom
    Méthodes :
        init: unNom unPrénom
            "Crée une nouvelle personne avec les informations fournies."
            nom value: unNom.
            prénom value: unPrénom.
        nom -> String
            "Renvoie le nom de la personne."
            nom value.
        prénom -> String
            "Renvoie le prénom de la personne."
            prénom value.
End classe Personne.
```

**Classe :** BaseDeDonneesFilms

```smalltalk
Classe BaseDeDonneesFilms
    Attributs : films
    Méthodes :
        init -> BaseDeDonneesFilms
            "Crée une nouvelle base de données de films."
            "Instancie le tableau contenant les films."
            films value: Tableau nouveau.
        ajouterFilm: unFilm
            "Ajoute un film à la base de données."
            films add: unFilm.
        rechercherFilmParTitre: unTitre -> Film
            "Recherche un film par son titre et le renvoie."
            "Parcours le tableau de films."
            1 to: films size do: [:i |
                film := films at: i.
                (film titre = unTitre) ifTrue: [^film]].
            "Renvoie nil si aucun film ne correspond."
            nil.
        listerFilms -> String
            "Renvoie une chaîne de caractères contenant la liste des films de la base de données."
            "Initalise la chaîne de caractères."
            result := String nouveau.
            "Parcours le tableau de films."
            1 to: films size do: [:i |
                film := films at: i.
                result appendLine: (film titre)].
            "Renvoie la chaîne de caractères."
            result.
End classe BaseDeDonneesFilms.
```

**Utilisation :**

```smalltalk
base := BaseDeDonneesFilms nouveau.

"Ajout de quelques films à la base de données."
base ajouterFilm: (Film init: 'La Matrice' unRealisateur: (Personne init: 'Lana Wachowski' unPrénom: 'Lana') uneAnnée: 1999).
base ajouterFilm: (Film init: 'Le Seigneur des anneaux' unRealisateur: (Personne init: 'Peter Jackson' unPrénom: 'Peter') uneAnnée: 2001).
base ajouterFilm: (Film init: 'Pulp Fiction' unRealisateur: (Personne init: 'Quentin Tarantino' unPrénom: 'Quentin') uneAnnée: 1994).

"Recherche d'un film par son titre."
film := base rechercherFilmParTitre: 'Pulp Fiction'.

"Affichage des informations sur le film trouvé."
Transcript show: (film titre); cr.
Transcript show: (film réalisateur nom); cr.
Transcript show: (film année); cr.

"Affichage de la liste des films de la base de données."
Transcript cr; show: (base listerFilms).
```

**Explications :**

* La classe **Film** représente un film avec son titre, son réalisateur et son année de sortie.
* La classe **Personne** représente une personne avec son nom et son prénom.
* La classe **BaseDeDonneesFilms** gère une collection de films, permettant d'ajouter, de rechercher et de lister les films.
* Le code d'utilisation crée une base de données de films, ajoute quelques films, recherche un film par son titre et affiche les informations sur le film trouvé, ainsi que la liste de tous les films de la base de données.