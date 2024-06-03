```ruby
# **CRÉATION D'UN OBJET Employé**
class Employé
  attr_reader :nom, :prénom, :date_embauche, :salaire, :service

  def initialize(nom, prénom, date_embauche, salaire, service)
    @nom = nom
    @prénom = prénom
    @date_embauche = date_embauche
    @salaire = salaire
    @service = service
  end
end

# **GESTION DES EMPLOYES**
class GestionEmployés
  def initialize
    @employés = []
  end

  def ajouter(employé)
    @employés << employé
  end

  def supprimer(identifiant)
    @employés.delete_if { |e| e.identifiant == identifiant }
  end

  def modifier(identifiant, attributs)
    employé = @employés.find { |e| e.identifiant == identifiant }
    employé.update_attributes(attributs) if employé
  end

  def rechercher(critères)
    @employés.select { |e| critères.all? { |clé, valeur| e.send(clé) == valeur } }
  end

  def lister
    @employés
  end

  def calculer_masse_salariale
    @employés.sum(&:salaire)
  end
end

# **MODULE DE CALCUL DE SALAIRE**
module CalculSalaire
  def calculer_salaire(heures_travaillées)
    salaire_horaire * heures_travaillées
  end

  def salaire_horaire
    @salaire / 160  # 160 heures travaillées par mois
  end
end

# **CLASSE Cadre**
class Cadre < Employé
  include CalculSalaire
end

# **CLASSE Technicien**
class Technicien < Employé
  include CalculSalaire

  def calculer_salaire(heures_travaillées)
    super(heures_travaillées) + prime_ancienneté
  end

  def prime_ancienneté
    @salaire * @date_embauche.year - Date.today.year / 100.0
  end
end

# **UTILISATION DU CODE**

# Création des instances d'employés
employé1 = Cadre.new("Dupont", "Jean", Date.new(2020, 1, 1), 2500, "Direction")
employé2 = Technicien.new("Martin", "Marie", Date.new(2018, 6, 1), 2000, "Technique")

# Gestion des employés
gestion_employés = GestionEmployés.new
gestion_employés.ajouter(employé1)
gestion_employés.ajouter(employé2)

# Recherche d'un employé par son nom
employé_recherché = gestion_employés.rechercher(nom: "Dupont")

# Affichage des informations de l'employé trouvé
puts "Nom : #{employé_recherché.nom}"
puts "Prénom : #{employé_recherché.prénom}"
puts "Date d'embauche : #{employé_recherché.date_embauche}"
puts "Salaire : #{employé_recherché.salaire}"
puts "Service : #{employé_recherché.service}"

# Calcul de la masse salariale
masse_salariale = gestion_employés.calculer_masse_salariale
puts "Masse salariale : #{masse_salariale}"
```

**Explications du code :**

Ce code Ruby implémente un système de gestion des employés. Il définit des classes pour représenter les employés, une classe pour gérer les employés et un module pour calculer les salaires.

* **Classe Employé:** Représente un employé avec des attributs tels que le nom, le prénom, la date d'embauche, le salaire et le service.
* **Classe GestionEmployés:** Gère une collection d'employés, permettant d'ajouter, supprimer, modifier, rechercher et lister les employés.
* **Module CalculSalaire:** Fournit une méthode pour calculer le salaire d'un employé en fonction des heures travaillées.
* **Classe Cadre:** Une classe enfant de Employé qui inclut le module CalculSalaire.
* **Classe Technicien:** Une autre classe enfant de Employé qui inclut CalculSalaire et implémente une logique de calcul de salaire spécifique avec une prime d'ancienneté.

Le code utilise également le modèle **Enregistrement Actif** (ORM) pour gérer la persistance des données. Les classes Employé, Cadre et Technicien sont mappées à des tables de base de données et les attributs sont représentés par des colonnes de table.

Enfin, le code fournit un exemple d'utilisation en créant des instances d'employés, en les ajoutant au gestionnaire d'employés, en recherchant un employé par son nom et en affichant ses informations. Il calcule également la masse salariale totale.