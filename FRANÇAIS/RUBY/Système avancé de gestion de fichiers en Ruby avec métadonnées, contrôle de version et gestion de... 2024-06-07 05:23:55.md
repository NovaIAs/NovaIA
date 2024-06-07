**Code complexe en Ruby avec explication détaillée**

Ce code complexe en Ruby implémente un système de gestion de fichiers avancé avec prise en charge des métadonnées, du contrôle de version et de la gestion des utilisateurs :

```ruby
# Module de gestion des métadonnées
module FileMetadata
  def metadata
    @metadata ||= {}
  end

  def set_metadata(key, value)
    metadata[key] = value
  end

  def get_metadata(key)
    metadata[key]
  end
end

# Module de contrôle de version
module VersionControl
  def versions
    @versions ||= []
  end

  def save_version
    versions << {
      content: content,
      metadata: metadata
    }
  end

  def restore_version(version_index)
    self.content = versions[version_index][:content]
    self.metadata = versions[version_index][:metadata]
  end
end

# Module de gestion des utilisateurs
module UserManagement
  def users
    @users ||= []
  end

  def add_user(user)
    users << user
  end

  def remove_user(user)
    users.delete(user)
  end
end

# Classe File représentant un fichier
class File
  include FileMetadata
  include VersionControl
  include UserManagement

  attr_accessor :name, :content

  def initialize(name, content)
    self.name = name
    self.content = content
  end

  # Permet de sauvegarder le fichier avec les métadonnées actuelles et de créer une nouvelle version
  def save
    save_version
  end

  # Permet de restaurer le fichier à partir d'une version précédente
  def restore(version_index)
    restore_version(version_index)
  end

  # Permet d'ajouter un utilisateur autorisé à accéder au fichier
  def grant_access(user)
    add_user(user)
  end

  # Permet de révoquer l'accès d'un utilisateur au fichier
  def revoke_access(user)
    remove_user(user)
  end

  # Permet de vérifier si un utilisateur a accès au fichier
  def has_access?(user)
    users.include?(user)
  end
end

# Classe FileManager pour gérer les fichiers
class FileManager
  attr_accessor :files

  def initialize
    self.files = []
  end

  # Permet d'ajouter un nouveau fichier au gestionnaire
  def add_file(file)
    files << file
  end

  # Permet de rechercher un fichier par son nom
  def find_file(name)
    files.find { |file| file.name == name }
  end

  # Permet de supprimer un fichier du gestionnaire
  def remove_file(name)
    files.delete_if { |file| file.name == name }
  end
end

# Exemple d'utilisation
file_manager = FileManager.new
file = File.new("mon_fichier.txt", "Contenu du fichier")
file_manager.add_file(file)
file.set_metadata("auteur", "Jean Dupont")

# Enregistrement de la première version
file.save

# Modification du contenu du fichier
file.content = "Nouveau contenu du fichier"

# Enregistrement de la deuxième version
file.save

# Restauration de la première version
file.restore(0)

# Vérification de l'accès à un utilisateur
utilisateur = "alice"
file.has_access?(utilisateur) # true si l'utilisateur a accès, false sinon
```

**Explication du code :**

* Les modules `FileMetadata`, `VersionControl` et `UserManagement` fournissent des fonctionnalités communes à tous les fichiers.
* La classe `File` représente un fichier individuel avec son nom, son contenu, ses métadonnées, ses versions et sa liste d'utilisateurs autorisés à y accéder.
* La classe `FileManager` gère les fichiers en les ajoutant, les supprimant et en les recherchant.
* L'exemple d'utilisation montre comment créer un fichier, le modifier, enregistrer ses versions, le restaurer à partir d'une version précédente et vérifier les autorisations des utilisateurs.

Ce code illustre la complexité et la puissance de Ruby en permettant de gérer des fichiers avec des métadonnées, des versions et des contrôles d'accès sophistiqués.