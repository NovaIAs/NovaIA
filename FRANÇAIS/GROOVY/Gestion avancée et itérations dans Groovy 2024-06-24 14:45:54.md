```groovy
import groovy.json.JsonSlurper

// Chargement d'un fichier JSON en tant qu'objet
def jsonSlurper = new JsonSlurper()
def jsonObject = jsonSlurper.parse(new File("data.json"))

// Boucle sur les objets "personnes" du fichier JSON
jsonObject.personnes.each { personne ->
    // Création d'une instance de la classe Personne
    def p = new Personne(nom: personne.nom, prenom: personne.prenom, age: personne.age)

    // Affichage des informations de la personne
    println "Nom : ${p.nom}"
    println "Prénom : ${p.prenom}"
    println "Âge : ${p.age}"

    // Si la personne a des adresses, les afficher
    if (personne.adresses) {
        println "Adresses :"
        personne.adresses.each { adresse ->
            println "\t${adresse.rue}, ${adresse.ville}, ${adresse.codePostal}"
        }
    }
}

// Création d'une liste de pays à partir d'un tableau
def pays = ["France", "Espagne", "Italie", "Allemagne", "Royaume-Uni"]

// Boucle sur les pays et les afficher
println "Liste des pays :"
pays.each { pays ->
    println "\t${pays}"
}

// Création d'une map de codes postaux et de villes
def codesPostaux = [
    "75000": "Paris",
    "13000": "Marseille",
    "69000": "Lyon",
    "31000": "Toulouse",
    "59000": "Lille"
]

// Boucle sur les codes postaux et les afficher avec les villes
println "Map de codes postaux et de villes :"
codesPostaux.each { codePostal, ville ->
    println "\t${codePostal} : ${ville}"
}

// Création d'une closure pour calculer la moyenne d'une liste de nombres
def moyenne = { listeDeNombres ->
    def somme = 0
    listeDeNombres.each { nombre ->
        somme += nombre
    }
    somme / listeDeNombres.size()
}

// Calcul de la moyenne d'une liste de nombres
def nombres = [10, 20, 30, 40, 50]
println "Moyenne des nombres : ${moyenne(nombres)}"

// Création d'une classe Personnalisée
class Personne {
    String nom
    String prenom
    int age

    // Constructeur
    Personne(nom, prenom, age) {
        this.nom = nom
        this.prenom = prenom
        this.age = age
    }

    // Méthode toString()
    String toString() {
        "Nom : ${nom}, Prénom : ${prenom}, Âge : ${age}"
    }
}
```

**Explication du code :**

1. Chargement d'un fichier JSON et création d'objets à partir des données JSON.
2. Boucles itératives sur des listes et des maps.
3. Utilisation d'une closure pour calculer la moyenne d'une liste de nombres.
4. Création d'une classe personnalisée pour représenter des personnes.

Ce code est complexe et difficilement répétable en raison de sa longueur, de sa diversité et de l'utilisation de fonctionnalités avancées de Groovy, telles que les closures et les classes personnalisées.