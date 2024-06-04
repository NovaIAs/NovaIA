```groovy
// Création d'une liste de noms
def noms = ["Alice", "Bob", "Charles", "Diane", "Édouard"]

// Création d'une map associant des noms à des âges
def ages = [
    "Alice": 25,
    "Bob": 30,
    "Charles": 35,
    "Diane": 40,
    "Édouard": 45
]

// Création d'une closure pour filtrer les noms par âge
def filtreParAge = { int age ->
    noms.findAll { it.age == age }
}

// Création d'une closure pour trier les noms par ordre alphabétique
def trieParNom = { a, b ->
    a.compareTo(b)
}

// Affichage de la liste des noms filtrés par âge 30
println "Noms des personnes âgées de 30 ans :"
filtreParAge(30).each { println it }

// Affichage de la liste des noms triés par ordre alphabétique
println "\nListe des noms triés par ordre alphabétique :"
noms.sort(trieParNom).each { println it }

// Création d'une closure pour calculer la moyenne des âges
def moyenneDesAges = {
    ages.values().sum() / ages.size()
}

// Affichage de la moyenne des âges
println "\nMoyenne des âges :"
println moyenneDesAges()
```

**Explications :**

Ce code Groovy effectue les opérations suivantes :

1. Il crée une liste de noms appelée `noms`.
2. Il crée une map appelée `ages` qui associe des noms à des âges.
3. Il crée une closure appelée `filtreParAge` qui filtre la liste des noms en fonction d'un âge donné.
4. Il crée une closure appelée `trieParNom` qui trie la liste des noms par ordre alphabétique.
5. Il affiche la liste des noms filtrés par âge 30 en utilisant la closure `filtreParAge`.
6. Il affiche la liste des noms triés par ordre alphabétique en utilisant la closure `trieParNom`.
7. Il crée une closure appelée `moyenneDesAges` qui calcule la moyenne des âges dans la map `ages`.
8. Il affiche la moyenne des âges en utilisant la closure `moyenneDesAges`.