```ruby
# Définir une classe pour modéliser un document de texte.
class Document
  def initialize(texte)
    @texte = texte
  end

  def nombre_mots
    @texte.split(" ").size
  end

  def nombre_caractères
    @texte.length
  end

  def fréquence_mot(mot)
    @texte.scan(mot).count
  end
end

# Définir une classe pour analyser un ensemble de documents.
class AnaliseurDocument
  def initialize(documents)
    @documents = documents
  end

  def nombre_total_mots
    @documents.map(&:nombre_mots).sum
  end

  def nombre_total_caractères
    @documents.map(&:nombre_caractères).sum
  end

  def fréquence_mot(mot)
    @documents.map { |doc| doc.fréquence_mot(mot) }.sum
  end

  def document_avec_le_plus_grand_nombre_de_mots
    @documents.max_by(&:nombre_mots)
  end

  def document_avec_la_fréquence_la_plus_élevée_pour_un_mot(mot)
    @documents.max_by { |doc| doc.fréquence_mot(mot) }
  end
end

# Exemple d'utilisation
documents = [
  Document.new("Ceci est un document avec vingt mots."),
  Document.new("Ceci est un autre document avec quinze mots."),
  Document.new("Ceci est un troisième document avec dix mots.")
]

analyseur = AnaliseurDocument.new(documents)

puts "Nombre total de mots : #{analyseur.nombre_total_mots}"
puts "Nombre total de caractères : #{analyseur.nombre_total_caractères}"
puts "Fréquence du mot \"avec\" : #{analyseur.fréquence_mot("avec")}"
puts "Document avec le plus grand nombre de mots : #{analyseur.document_avec_le_plus_grand_nombre_de_mots}"
puts "Document avec la fréquence la plus élevée pour le mot \"dix\" : #{analyseur.document_avec_la_fréquence_la_plus_élevée_pour_un_mot("dix")}"
```

**Explication:**

* La classe `Document` représente un document de texte et fournit des méthodes pour calculer le nombre de mots, le nombre de caractères et la fréquence d'un mot donné.
* La classe `AnaliseurDocument` analyse un ensemble de documents et fournit des méthodes pour calculer le nombre total de mots, le nombre total de caractères, la fréquence d'un mot donné, le document avec le plus grand nombre de mots et le document avec la fréquence la plus élevée pour un mot donné.
* L'exemple d'utilisation crée un ensemble de documents, crée un analyseur de documents, puis utilise l'analyseur pour calculer et afficher diverses statistiques sur les documents.