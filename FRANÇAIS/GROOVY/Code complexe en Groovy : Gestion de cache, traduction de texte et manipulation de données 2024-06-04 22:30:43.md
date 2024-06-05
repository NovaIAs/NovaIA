**Code complexe en Groovy**

```groovy
// Définition des constantes
def MAPS = ['FR': ['France', 'Paris'], 'US': ['États-Unis', 'Washington D.C.']]
def LANGUES = ['FR': 'Français', 'US': 'Anglais']

// Création d'une carte avec les pays et leurs capitales
def paysCapitales = MAPS.collectEntries { pays, ville -> [pays: ville.get(0), capitale: ville.get(1)] }

// Création d'une liste avec les noms de pays et leurs langues correspondantes
def nomsLangues = LANGUES.collectEntries { pays, langue -> [pays: pays, langue: langue] }

// Fonction pour traduire un texte de la langue d'origine vers la langue cible
def traduireTexte(String texte, String langueOrigine, String langueCible) {
    // Vérification des langues valides
    if (!LANGUES.containsKey(langueOrigine) || !LANGUES.containsKey(langueCible)) {
        throw new IllegalArgumentException("Langues invalides : $langueOrigine, $langueCible")
    }

    // Définition de la méthode de traduction
    def traduction = { texte ->
        // Génération d'une clé unique pour la traduction
        def cle = "$texte,$langueOrigine,$langueCible"

        // Vérification si la traduction est déjà en cache
        def traductionCachee = Cache.get(cle)
        if (traductionCachee != null) {
            return traductionCachee
        }

        // Traduction du texte
        def texteTraduit = // Logic de traduction complexe ici

        // Stockage de la traduction en cache
        Cache.put(cle, texteTraduit)

        // Renvoi de la traduction
        return texteTraduit
    }

    // Exécution de la traduction
    return traduction(texte)
}

// Définition des actions à effectuer
def actions = [
    // Action 1 : Traduire un texte du français vers l'anglais
    {
        def texte = "Bonjour le monde !"
        def langueOrigine = "FR"
        def langueCible = "US"
        def traductionFrEn = traduireTexte(texte, langueOrigine, langueCible)
        println "Traduction FR -> US : $traductionFrEn"
    },
    // Action 2 : Afficher la capitale de la France
    {
        def pays = "FR"
        def capitale = paysCapitales.get(pays)
        println "Capitale de $pays : $capitale"
    },
    // Action 3 : Afficher la langue parlée aux États-Unis
    {
        def pays = "US"
        def langue = nomsLangues.get(pays)
        println "Langue parlée aux $pays : $langue"
    }
]

// Exécution des actions
actions.each { action -> action() }
```

**Explication du code**

Ce code Groovy illustre des concepts complexes tels que la gestion de cache, la traduction de texte et la manipulation de données. Voici une explication détaillée :

* **Gestion de cache** : Le code utilise une variable `Cache` pour stocker les traductions en cache afin d'éviter de traduire le même texte plusieurs fois.

* **Traduction de texte** : La fonction `traduireTexte` prend un texte, une langue d'origine et une langue cible, et renvoie le texte traduit. Elle vérifie d'abord que les langues sont valides, puis utilise une méthode de traduction complexe (non implémentée ici) pour effectuer la traduction.

* **Manipulation de données** : Le code utilise des cartes (dictionnaires) (`MAPS` et `nomsLangues`) pour stocker des informations sur les pays et les langues. Il utilise également la méthode `collectEntries` pour transformer les cartes en listes de paires clé-valeur.

* **Actions** : Le code définit une liste d'actions à effectuer, notamment la traduction de texte, l'affichage des capitales et des langues. La méthode `each` exécute chaque action par itération.