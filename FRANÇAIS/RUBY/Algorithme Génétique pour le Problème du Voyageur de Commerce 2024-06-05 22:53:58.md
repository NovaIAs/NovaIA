```ruby
# Implémentation d'un algorithme génétique pour la résolution du problème du voyageur de commerce

# Représentation d'un individu (un chemin)
class Individu
  attr_accessor :ville, :distance

  def initialize(villes)
    @villes = villes
    @distance = 0
  end

  # Génère un nouveau chemin aléatoire
  def generer
    @villes.shuffle!
  end

  # Calcule la distance du chemin
  def calculer_distance
    @distance = 0
    (0...@villes.size-1).each do |i|
      @distance += villes[i].distance_to(@villes[i+1])
    end
    @distance
  end

  # Affiche le chemin
  def to_s
    @villes.join(' -> ')
  end
end

# Population d'individus
class Population
  attr_accessor :individus

  def initialize(taille, villes)
    @individus = Array.new(taille) { Individu.new(villes) }
  end

  # Évalue la population (calcul la distance de chaque individu)
  def evaluer
    @individus.each { |individu| individu.calculer_distance }
  end

  # Tri par distance ascendante
  def trier
    @individus.sort! { |a, b| a.distance <=> b.distance }
  end

  # Sélectionne les meilleurs individus pour la reproduction
  def selectionner(proportion)
    nb_individus = (@individus.size * proportion).to_i
    @individus.slice(0, nb_individus)
  end

  # Croisement des individus (méthode PMX)
  def croiser(parent1, parent2)
    # Choix d'un point de croisement aléatoire
    point_croisement = rand(1...@individus.size)

    # Création d'un nouvel individu vide
    enfant = Individu.new(@individus[0].villes)

    # Copie la partie gauche
    enfant.villes[0...point_croisement] = parent1.villes[0...point_croisement]

    # Remplissage de la partie droite avec des gènes uniques du parent2
    villes_manquantes = parent2.villes.select { |ville| !enfant.villes.include?(ville) }
    villes_manquantes.each do |ville|
      # Recherche de la position d'insertion
      index = parent1.villes.index(villes_manquantes[0])
      while enfant.villes[index]
        index = parent1.villes.index(enfant.villes[index])
      end
      enfant.villes[index] = ville
    end

    enfant
  end

  # Mutation d'un individu (inversion de deux gènes)
  def muter(individu)
    # Choix de deux points de mutation aléatoires
    point1, point2 = rand(1...@individus.size), rand(1...@individus.size)
    while point1 == point2
      point2 = rand(1...@individus.size)
    end

    # Inversion des gènes
    individu.villes[point1...point2] = individu.villes[point1...point2].reverse
  end

  # Affiche la meilleure solution
  def meilleure_solution
    puts "Meilleur chemin : #{individus[0]}"
    puts "Meilleure distance : #{individus[0].distance}"
  end
end

# Fonction principale
def main
  # Liste des villes
  villes = [
    Ville.new(0, 0),
    Ville.new(1, 2),
    Ville.new(2, 3),
    Ville.new(3, 1),
  ]

  # Taille de la population
  taille_population = 100

  # Nombre de générations
  nb_generations = 100

  # Proportion de sélection
  proportion_selection = 0.2

  # Taux de mutation
  taux_mutation = 0.05

  # Création de la population initiale
  population = Population.new(taille_population, villes)

  # Boucle d'évolution
  nb_generations.times do |generation|
    # Évaluation de la population
    population.evaluer

    # Sélection des meilleurs individus
    individus_selectionnes = population.selectionner(proportion_selection)

    # Création de la nouvelle génération
    nouvelle_population = Population.new(taille_population, villes)
    individus_selectionnes.each do |individu1|
      individus_selectionnes.each do |individu2|
        enfant = population.croiser(individu1, individu2)
        nouvelle_population.individus << enfant
      end
    end

    # Mutation de la population
    puts "Génération #{generation+1} :"
    nouvelle_population.individus.each do |individu|
      if rand < taux_mutation
        population.muter(individu)
      end
    end

    population = nouvelle_population
  end

  # Affichage de la meilleure solution
  population.meilleure_solution
end

# Lancement de la fonction principale
main
```

**Explications du code :**

Ce code implémente un algorithme génétique pour résoudre le problème du voyageur de commerce. Cet algorithme recherche le chemin le plus court qui visite toutes les villes données une fois et revient au point de départ.

**Représentation de l'individu (chemin)**

La classe `Individu` représente un individu, qui est un chemin dans ce contexte. Elle contient un tableau de villes et la distance du chemin. Les méthodes de cette classe permettent de générer un nouveau chemin aléatoire, de calculer la distance du chemin et d'afficher le chemin.

**Population d'individus**

La classe `Population` représente une population d'individus. Elle contient un tableau d'individus et les méthodes pour évaluer la population (calculer la distance de chaque individu), trier la population par distance, sélectionner les meilleurs individus pour la reproduction et croiser et muter les individus.

**Algorithme génétique**

La fonction `main` est la fonction principale de l'algorithme génétique. Elle crée une population initiale, la fait évoluer pendant un certain nombre de générations et affiche la meilleure solution trouvée. L'évolution se fait par sélection des meilleurs individus, croisement, mutation et remplacement de la population par la nouvelle génération.

**Méthode PMX**

La méthode PMX (Partially Mapped Crossover) est une méthode de croisement utilisée dans les algorithmes génétiques. Elle consiste à choisir deux points de croisement aléatoires dans les chemins parents et à copier la partie gauche du premier parent dans l'enfant. Ensuite, les gènes manquants du second parent sont insérés dans l'enfant en suivant l'ordre du premier parent.

**Mutation**

La mutation est un processus qui introduit de la diversité dans la population. Dans ce code, la mutation consiste à inverser deux gènes aléatoires dans le chemin.

**Exemple d'utilisation**

Pour utiliser le code, créez une liste de villes et appelez la fonction `main`. Le code trouvera le chemin le plus court qui visite toutes les villes données et reviendra au point de départ.