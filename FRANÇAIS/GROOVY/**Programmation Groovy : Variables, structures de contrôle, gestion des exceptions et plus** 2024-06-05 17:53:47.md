**Déclaration de variables et constantes**

```groovy
// Déclaration de variables locales (portée limitée au bloc dans lequel elles sont déclarées)
def monNom = "John Doe"
def monAge = 30

// Déclaration de variables globales (portée dans l'ensemble du script)
def globaleVariable = "Je suis globale"

// Déclaration de constantes (valeur invariable)
final constante = "Je suis constante"
```

**Structures de contrôle**

```groovy
// Condition if-else
if (monAge > 18) {
    println("Vous êtes un adulte")
} else {
    println("Vous êtes mineur")
}

// Boucle while
while (monAge < 100) {
    monAge++
    println("Votre âge est : ${monAge}")
}

// Boucle for classique
for (i in 1..10) {
    println("Valeur de i : ${i}")
}

// Boucle for améliorée (itération sur une collection)
def maListe = [1, 2, 3]
for (item in maListe) {
    println("Élément de la liste : ${item}")
}

// Opérateur ternaire
def age = monAge > 18 ? "Adulte" : "Mineur"
println("Votre statut d'âge : ${age}")
```

**Gestion des exceptions**

```groovy
try {
    // Code susceptible de générer une exception
} catch (Exception e) {
    println("Une exception est survenue : ${e.message}")
} finally {
    // Code toujours exécuté, que l'exception soit levée ou non
}
```

**Travail avec des listes et des cartes**

```groovy
// Création d'une liste
def maListe = ["pomme", "banane", "orange"]

// Ajout d'un élément à une liste
maListe.add("fraise")

// Suppression d'un élément d'une liste
maListe.remove("orange")

// Création d'une carte (association clé-valeur)
def maCarte = ["nom": "John Doe", "âge": 30]

// Ajout d'un élément à une carte
maCarte["adresse"] = "Rue de la Paix"

// Récupération d'une valeur d'une carte
def nom = maCarte["nom"]
```

**Travail avec des fermetures**

```groovy
// Définition d'une fermeture
def fermeture = { ->
    println("Je suis une fermeture")
}

// Appel de la fermeture
fermeture.call()
```

**Utilisation de classes**

```groovy
// Déclaration d'une classe
class Personne {
    String nom
    int âge

    Personne(String nom, int âge) {
        this.nom = nom
        this.âge = âge
    }

    String getNom() {
        return nom
    }

    void setNom(String nom) {
        this.nom = nom
    }

    int getAge() {
        return âge
    }

    void setAge(int âge) {
        this.âge = âge
    }

    String toString() {
        return "${nom} - ${âge}"
    }
}

// Création d'une instance de classe
def personne = new Personne("John Doe", 30)

// Accès aux propriétés de classe
println("Nom : ${personne.getNom()}")
println("Âge : ${personne.getAge()}")

// Modification des propriétés de classe
personne.setNom("Jane Doe")
personne.setAge(25)

// Affichage de l'objet de classe
println("Personne : ${personne}")
```

**Communication avec les bases de données**

```groovy
// Bibliothèque pour l'accès aux bases de données H2
@Grab('com.h2database:h2:1.4.200')
import groovy.sql.Sql

// Création d'une connexion à la base de données
def db = Sql.newInstance("jdbc:h2:mem:myDB", "sa", "", "org.h2.Driver")

// Création d'une table
db.execute("CREATE TABLE users (id INT PRIMARY KEY, name VARCHAR(255), age INT)")

// Insertion de données dans la table
db.execute("INSERT INTO users (name, age) VALUES ('John Doe', 30)")

// Récupération de données de la table
def results = db.rows("SELECT * FROM users")

// Affichage des résultats
for (row in results) {
    println("Utilisateur : ${row.name} - ${row.age}")
}

// Fermeture de la connexion à la base de données
db.close()
```