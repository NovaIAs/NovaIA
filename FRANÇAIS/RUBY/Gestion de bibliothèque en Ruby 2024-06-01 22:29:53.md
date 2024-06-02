**Programme de gestion de bibliothèque en Ruby**

```ruby
class Livre
  attr_reader :titre, :auteur, :genre, :nombre_exemplaires
  attr_writer :nombre_exemplaires

  def initialize(titre:, auteur:, genre:, nombre_exemplaires:)
    @titre             = titre
    @auteur            = auteur
    @genre             = genre
    @nombre_exemplaires = nombre_exemplaires
  end

  def to_s
    "Titre : #{@titre}, Auteur : #{@auteur}, Genre : #{@genre}, Nombre d'exemplaires : #{@nombre_exemplaires}"
  end
end

class Bibliotheque
  attr_reader :livres

  def initialize
    @livres = []
  end

  def ajouter_livre(livre)
    @livres << livre
  end

  def supprimer_livre(livre)
    @livres.delete(livre)
  end

  def rechercher_livre_par_titre(titre)
    @livres.find { |livre| livre.titre == titre }
  end

  def rechercher_livre_par_auteur(auteur)
    @livres.find_all { |livre| livre.auteur == auteur }
  end

  def rechercher_livre_par_genre(genre)
    @livres.select { |livre| livre.genre == genre }
  end

  def liste_des_livres
    @livres.map(&:to_s).join("\n")
  end

  def emprunter_livre(livre)
    if livre.nombre_exemplaires > 0
      livre.nombre_exemplaires -= 1
      puts "Emprunt réussi !"
    else
      puts "Emprunt impossible, aucun exemplaire disponible."
    end
  end

  def rendre_livre(livre)
    livre.nombre_exemplaires += 1
    puts "Rendu réussi !"
  end
end

def menu_principal
  puts "Bienvenue dans la gestion de bibliothèque."
  puts "Choisissez une option :"
  puts "1. Ajouter un livre"
  puts "2. Supprimer un livre"
  puts "3. Rechercher un livre par titre"
  puts "4. Rechercher un livre par auteur"
  puts "5. Rechercher un livre par genre"
  puts "6. Lister tous les livres"
  puts "7. Emprunter un livre"
  puts "8. Rendre un livre"
  puts "9. Quitter"

  choix = gets.chomp.to_i

  case choix
  when 1
    ajouter_livre
  when 2
    supprimer_livre
  when 3
    rechercher_livre_par_titre
  when 4
    rechercher_livre_par_auteur
  when 5
    rechercher_livre_par_genre
  when 6
    liste_des_livres
  when 7
    emprunter_livre
  when 8
    rendre_livre
  when 9
    exit
  else
    puts "Choix invalide, veuillez réessayer."
    menu_principal
  end
end

def ajouter_livre
  puts "Entrez le titre du livre :"
  titre = gets.chomp

  puts "Entrez le nom de l'auteur :"
  auteur = gets.chomp

  puts "Entrez le genre du livre :"
  genre = gets.chomp

  puts "Entrez le nombre d'exemplaires :"
  nombre_exemplaires = gets.chomp.to_i

  livre = Livre.new(titre: titre, auteur: auteur, genre: genre, nombre_exemplaires: nombre_exemplaires)
  @bibliotheque.ajouter_livre(livre)

  puts "Livre ajouté avec succès !"
  menu_principal
end

def supprimer_livre
  puts "Entrez le titre du livre à supprimer :"
  titre = gets.chomp

  livre = @bibliotheque.rechercher_livre_par_titre(titre)

  if livre
    @bibliotheque.supprimer_livre(livre)
    puts "Livre supprimé avec succès !"
  else
    puts "Livre introuvable."
  end

  menu_principal
end

def rechercher_livre_par_titre
  puts "Entrez le titre du livre à rechercher :"
  titre = gets.chomp

  livre = @bibliotheque.rechercher_livre_par_titre(titre)

  if livre
    puts livre
  else
    puts "Livre introuvable."
  end

  menu_principal
end

def rechercher_livre_par_auteur
  puts "Entrez le nom de l'auteur à rechercher :"
  auteur = gets.chomp

  livres = @bibliotheque.rechercher_livre_par_auteur(auteur)

  if livres.empty?
    puts "Aucun livre trouvé pour cet auteur."
  else
    puts "Livres trouvés :"
    livres.each { |livre| puts livre }
  end

  menu_principal
end

def rechercher_livre_par_genre
  puts "Entrez le genre à rechercher :"
  genre = gets.chomp

  livres = @bibliotheque.rechercher_livre_par_genre(genre)

  if livres.empty?
    puts "Aucun livre trouvé pour ce genre."
  else
    puts "Livres trouvés :"
    livres.each { |livre| puts livre }
  end

  menu_principal
end

def liste_des_livres
  puts "Liste des livres :"
  puts @bibliotheque.liste_des_livres

  menu_principal
end

def emprunter_livre
  puts "Entrez le titre du livre à emprunter :"
  titre = gets.chomp

  livre = @bibliotheque.rechercher_livre_par_titre(titre)

  if livre
    @bibliotheque.emprunter_livre(livre)
  else
    puts "Livre introuvable."
  end

  menu_principal
end

def rendre_livre
  puts "Entrez le titre du livre à rendre :"
  titre = gets.chomp

  livre = @bibliotheque.rechercher_livre_par_titre(titre)

  if livre
    @bibliotheque.rendre_livre(livre)
  else
    puts "Livre introuvable."
  end

  menu_principal
end

# Instanciation de la bibliothèque
@bibliotheque = Bibliotheque.new

# Affichage du menu principal
menu_principal
```

**Explication du code :**

Ce programme implémente un système de gestion de bibliothèque en Ruby. Il permet d'ajouter, supprimer, rechercher, emprunter et rendre des livres.

**Classes et méthodes**

* **Livre** : représente un livre avec les attributs `titre`, `auteur`, `genre` et `nombre_exemplaires`.
* **Bibliotheque** : représente la bibliothèque et contient une liste de livres. Elle possède des méthodes pour ajouter, supprimer, rechercher, emprunter et rendre des livres.

**Fonctions**

* **menu_principal** : affiche le menu principal et récupère le choix de l'utilisateur.
* **ajouter_livre** : ajoute un nouveau livre à la bibliothèque.
* **supprimer_livre** : supprime un livre de la bibliothèque.
* **rechercher_livre_par_titre** : recherche un livre par son titre.
* **rechercher_livre_par_auteur** : recherche un ou plusieurs livres par leur auteur.
* **rechercher_livre_par_genre** : recherche un ou plusieurs livres par leur genre.
* **liste_des_livres** : affiche la liste de tous les livres.
* **emprunter_livre** : emprunte un livre s'il est disponible.
* **rendre_livre** : rend un livre qui a été emprunté.

**Utilisation**

Pour utiliser le programme, lancez le fichier Ruby et suivez les instructions affichées dans le menu principal.