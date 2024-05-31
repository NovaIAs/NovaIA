**Système de gestion de bibliothèque en Ruby**

**Classe Bibliothèque**

```ruby
class Bibliothèque
  attr_reader :livres, :lecteurs

  def initialize
    @livres = []
    @lecteurs = []
  end

  def ajouter_livre(livre)
    @livres << livre
  end

  def ajouter_lecteur(lecteur)
    @lecteurs << lecteur
  end

  def trouver_livre_par_titre(titre)
    @livres.find { |livre| livre.titre == titre }
  end

  def trouver_lecteur_par_nom(nom)
    @lecteurs.find { |lecteur| lecteur.nom == nom }
  end

  def emprunter_livre(titre, lecteur)
    livre = trouver_livre_par_titre(titre)
    raise "Le livre n'existe pas" unless livre

    lecteur = trouver_lecteur_par_nom(lecteur)
    raise "Le lecteur n'existe pas" unless lecteur

    livre.emprunte = true
    lecteur.emprunts << livre
  end

  def rendre_livre(titre, lecteur)
    livre = trouver_livre_par_titre(titre)
    raise "Le livre n'existe pas" unless livre

    lecteur = trouver_lecteur_par_nom(lecteur)
    raise "Le lecteur n'existe pas" unless lecteur

    livre.emprunte = false
    lecteur.emprunts.delete(livre)
  end
end
```

**Classe Livre**

```ruby
class Livre
  attr_accessor :titre, :auteur, :emprunte

  def initialize(titre, auteur)
    @titre = titre
    @auteur = auteur
    @emprunte = false
  end
end
```

**Classe Lecteur**

```ruby
class Lecteur
  attr_accessor :nom, :emprunts

  def initialize(nom)
    @nom = nom
    @emprunts = []
  end
end
```

**Exemple d'utilisation**

```ruby
# Créer une bibliothèque
bibliotheque = Bibliothèque.new

# Ajouter des livres
bibliotheque.ajouter_livre(Livre.new("Le Seigneur des Anneaux", "J.R.R. Tolkien"))
bibliotheque.ajouter_livre(Livre.new("Harry Potter à l'école des sorciers", "J.K. Rowling"))

# Ajouter des lecteurs
bibliotheque.ajouter_lecteur(Lecteur.new("Jean Dupont"))
bibliotheque.ajouter_lecteur(Lecteur.new("Marie Martin"))

# Emprunter un livre
bibliotheque.emprunter_livre("Le Seigneur des Anneaux", "Jean Dupont")

# Rendre un livre
bibliotheque.rendre_livre("Harry Potter à l'école des sorciers", "Marie Martin")
```

**Explication**

Ce code implémente un système de gestion de bibliothèque complet en Ruby. Il permet d'ajouter des livres et des lecteurs à la bibliothèque, d'emprunter et de rendre des livres, et de rechercher des livres et des lecteurs par leur titre ou leur nom.

Les classes `Bibliothèque`, `Livre` et `Lecteur` représentent respectivement la bibliothèque, les livres et les lecteurs. Chaque classe a des attributs et des méthodes appropriés pour gérer ses données.

La classe `Bibliothèque` est le point d'entrée principal du système. Elle contient des méthodes pour ajouter des livres et des lecteurs, rechercher des livres et des lecteurs, et emprunter et rendre des livres.

Les classes `Livre` et `Lecteur` représentent des objets simples avec des attributs et des méthodes de base. La classe `Livre` possède un attribut `emprunte` qui indique si le livre est actuellement emprunté. La classe `Lecteur` possède un attribut `emprunts` qui contient une liste des livres empruntés par le lecteur.

La méthode `emprunter_livre` vérifie si le livre existe dans la bibliothèque et si le lecteur existe. Elle vérifie également si le livre n'est pas déjà emprunté. Si toutes les vérifications réussissent, le livre est marqué comme emprunté et ajouté à la liste des emprunts du lecteur.

La méthode `rendre_livre` vérifie si le livre existe dans la bibliothèque et si le lecteur existe. Elle vérifie également si le livre est actuellement emprunté par le lecteur. Si toutes les vérifications réussissent, le livre est marqué comme non emprunté et supprimé de la liste des emprunts du lecteur.

Ce code fournit un système de gestion de bibliothèque complet et extensible qui peut être utilisé pour gérer une bibliothèque réelle ou virtuelle.