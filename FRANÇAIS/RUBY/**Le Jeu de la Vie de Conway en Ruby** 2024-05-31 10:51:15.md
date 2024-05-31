**Le Jeu de la Vie de Conway**

Le Jeu de la Vie de Conway est un jeu automatisé créé par John Horton Conway en 1970. Il est basé sur un ensemble de règles simples qui déterminent l'évolution d'une grille de cellules en fonction de l'état de ses voisines.

**Code:**

```ruby
class JeuDeLaVie

  attr_accessor :grille

  def initialize(taille_grille)
    @grille = Array.new(taille_grille) { Array.new(taille_grille, 0) }
  end

  def initialiser(motif)
    motif.each do |ligne, colonne|
      @grille[ligne][colonne] = 1
    end
  end

  def evoluer
    grille_suivante = Array.new(@grille.size) { Array.new(@grille.size, 0) }

    @grille.each_with_index do |ligne, l|
      ligne.each_with_index do |cellule, c|
        voisins_vivants = compter_voisins_vivants(l, c)

        case cellule
        when 0
          grille_suivante[l][c] = 1 if voisins_vivants == 3
        when 1
          grille_suivante[l][c] = 0 if voisins_vivants < 2 || voisins_vivants > 3
        end
      end
    end

    @grille = grille_suivante
  end

  def compter_voisins_vivants(ligne, colonne)
    compteur = 0

    (-1..1).each do |l|
      (-1..1).each do |c|
        i = ligne + l
        j = colonne + c

        if i.between?(0, @grille.size - 1) && j.between?(0, @grille.size - 1) && @grille[i][j] == 1
          compteur += 1
        end
      end
    end

    compteur -= 1 if @grille[ligne][colonne] == 1

    compteur
  end

  def afficher
    puts "Grille actuelle :"
    @grille.each do |ligne|
      puts ligne.join(" ")
    end
  end
end

# Exemple d'utilisation

jeu = JeuDeLaVie.new(20)
jeu.initialiser([[10, 10], [10, 11], [11, 10], [11, 11]])
jeu.evoluer
jeu.afficher
```

**Explication:**

* La classe `JeuDeLaVie` représente le jeu et contient la grille et les méthodes pour la manipuler.
* La méthode `initialize` crée une nouvelle grille de la taille spécifiée, initialisée avec des cellules mortes (0).
* La méthode `initialiser` place les cellules vivantes (1) aux coordonnées spécifiées dans le motif.
* La méthode `evoluer` calcule la nouvelle génération de la grille en appliquant les règles du jeu.
* La méthode `compter_voisins_vivants` compte le nombre de cellules vivantes voisines d'une cellule donnée.
* La méthode `afficher` affiche la grille actuelle.

L'exemple d'utilisation crée un jeu avec une grille de 20x20, initialise un motif de planeur, fait évoluer la grille une fois et l'affiche.