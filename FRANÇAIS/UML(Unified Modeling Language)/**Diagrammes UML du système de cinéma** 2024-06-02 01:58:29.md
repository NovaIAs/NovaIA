**Diagramme de classes**

```
+-----------------+
| **Personne**       |
+-----------------+
| - nom : String   |
| - prenom : String |
| - age : int      |
+-----------------+

+------------------+
| **Acteur**         |
+------------------+
| - filmsTournes : Set<Film> |
+------------------+

+--------------------+
| **Film**          |
+--------------------+
| - titre : String   |
| - anneeSortie : int |
| - genre : String   |
+--------------------+

+---------------------------+
| **Realisateur**         |
+---------------------------+
| - filmsRealises : Set<Film> |
+---------------------------+

+-------------------+
| **Salle**         |
+-------------------+
| - numero : int   |
| - capacite : int  |
+-------------------+

+----------------------------+
| **Projection**            |
+----------------------------+
| - film : Film              |
| - date : LocalDate          |
| - heure : LocalTime         |
| - salle : Salle            |
+----------------------------+
```

**Diagramme de séquences**

```
participant Personne
participant Acteur
participant Film
participant Realisateur
participant Salle
participant Projection

lifeline Personne
lifeline Acteur
lifeline Film
lifeline Realisateur
lifeline Salle
lifeline Projection

Personne->Film : rechercherFilm(titre)
Film->Personne : retournerFilm(film)
Personne->Acteur : rechercherActeur(nom)
Acteur->Personne : retournerActeur(acteur)
Personne->Realisateur : rechercherRealisateur(nom)
Realisateur->Personne : retournerRealisateur(realisateur)
Personne->Salle : rechercherSalle(numero)
Salle->Personne : retournerSalle(salle)
Personne->Projection : rechercherProjection(film, date, heure, salle)
Projection->Personne : retournerProjection(projection)
```

**Diagramme d'activité**

```
initial
  -> Rechercher film
    -> Le film existe
      -> Rechercher acteur
        -> L'acteur existe
          -> Rechercher réalisateur
            -> Le réalisateur existe
              -> Rechercher salle
                -> La salle existe
                  -> Rechercher projection
                    -> La projection existe
                      -> Afficher la projection
                    -> Pas de projection
                      -> Terminer
                  -> Pas de salle
                    -> Terminer
              -> Pas de réalisateur
                -> Terminer
            -> Pas d'acteur
              -> Terminer
        -> Pas d'acteur
          -> Terminer
      -> Pas de film
        -> Terminer
```

**Diagramme de cas d'utilisation**

```
Acteur : Personne

Cas d'utilisation : Rechercher une projection

Préconditions :
- La personne connaît le titre du film, la date, l'heure et le numéro de la salle de la projection

Flux principal :
- La personne recherche le film par son titre
- La personne recherche l'acteur principal du film
- La personne recherche le réalisateur du film
- La personne recherche la salle de projection
- La personne recherche la projection par le film, la date, l'heure et la salle
- La personne affiche la projection

Flux alternatif :
- Le film n'existe pas
- L'acteur n'existe pas
- Le réalisateur n'existe pas
- La salle n'existe pas
- La projection n'existe pas
```

**Explication du code**

Ce code UML modélise un système de cinéma. Le système permet à un utilisateur de rechercher des films, des acteurs, des réalisateurs, des salles et des projections.

Le diagramme de classes définit les classes du système et leurs relations. Les classes principales sont Personne, Acteur, Film, Realisateur, Salle et Projection.

Le diagramme de séquences montre comment les objets du système interagissent pour réaliser le cas d'utilisation "Rechercher une projection".

Le diagramme d'activité montre le flux de contrôle du cas d'utilisation "Rechercher une projection".

Le diagramme de cas d'utilisation définit le cas d'utilisation "Rechercher une projection" et ses préconditions et flux principal et alternatif.