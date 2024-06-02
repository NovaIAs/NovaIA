```ruby
# Appel d'une méthode de classe <=> définissant une comparaison personnalisée pour la classe Voitures
class Voitures
  class << self
    def <=>(autre)
      # Utilisation d'une technique d'interpolation de chaîne pour construire une comparaison personnalisée
      "#{self.marque}#{self.modele}" <=> "#{autre.marque}#{autre.modele}"
    end
  end

  # Définition des attributs d'instance
  attr_reader :marque, :modele, :annee

  # Constructeur de la classe
  def initialize(marque, modele, annee)
    @marque = marque
    @modele = modele
    @annee = annee
  end

  # Méthode d'instance inspect fournissant une représentation textuelle de l'objet
  def inspect
    "<Voiture: #{@marque} #{@modele} (#{@annee})>"
  end

  # Méthode d'instance to_s fournissant une représentation textuelle de l'objet
  def to_s
    "Voiture : #{@marque} #{@modele} (#{@annee})"
  end
end

# Création de quelques objets Voiture
voiture1 = Voitures.new("Toyota", "Camry", 2018)
voiture2 = Voitures.new("Honda", "Accord", 2019)
voiture3 = Voitures.new("Ford", "Mustang", 2020)

# Utilisation de la comparaison personnalisée <=> pour trier les voitures
voitures_triees = [voiture1, voiture2, voiture3].sort

# Affichage des voitures triées dans un tableau
puts "Voitures triées :"
voitures_triees.each { |voiture| puts voiture }

# Utilisation de la méthode inspect pour obtenir une représentation textuelle des objets Voiture
puts "Représentation inspect :"
puts voiture1.inspect
puts voiture2.inspect
puts voiture3.inspect

# Utilisation de la méthode to_s pour obtenir une représentation textuelle des objets Voiture
puts "Représentation to_s :"
puts voiture1.to_s
puts voiture2.to_s
puts voiture3.to_s
```

**Explications du code :**

* La méthode de classe `Voitures.<=>` définit une comparaison personnalisée pour la classe `Voitures`, permettant de comparer des objets `Voitures` en fonction de leur marque et de leur modèle.
* Les attributs d'instance `marque`, `modele` et `annee` sont utilisés pour stocker les informations sur la voiture.
* La méthode `inspect` fournit une représentation textuelle de l'objet `Voiture`, y compris sa marque, son modèle et son année.
* La méthode `to_s` fournit également une représentation textuelle de l'objet `Voiture`, mais dans un format plus convivial.
* La méthode `sort` est utilisée pour trier le tableau `voitures` en utilisant la comparaison personnalisée définie dans `Voitures.<=>`.
* Les méthodes `inspect` et `to_s` sont appelées pour afficher des représentations textuelles des objets `Voitures`.