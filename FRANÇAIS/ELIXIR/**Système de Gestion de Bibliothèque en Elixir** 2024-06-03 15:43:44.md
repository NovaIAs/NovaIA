**Système de Gestion de Bibliothèque**

```elixir
# Définition des types de données

defmodule Bibliotheque do
  defstruct [:id, :titre, :auteur, :statut]
end

defmodule Utilisateur do
  defstruct [:id, :nom, :emprunts]
end

# Module de création de bibliothèques

defmodule Bibliotheques do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: :bibliotheques)
  end

  def add_livre(livre) do
    GenServer.call(:bibliotheques, {:add_livre, livre})
  end

  def get_livre(id) do
    GenServer.call(:bibliotheques, {:get_livre, id})
  end

  def update_statut(id, statut) do
    GenServer.call(:bibliotheques, {:update_statut, id, statut})
  end

  def handle_call({:add_livre, livre}, _, state) do
    {_, nouveau_id} = Enum.max_by(state, & &1.id)
    livre_avec_id = %Bibliotheque{livre | id: nouveau_id + 1}
    {state, livre_avec_id}
  end

  def handle_call({:get_livre, id}, _, state) do
    {livre, _} = Enum.find(state, &(&1.id == id))
    {livre, state}
  end

  def handle_call({:update_statut, id, statut}, _, state) do
    state = Enum.map(state, fn livre ->
      if livre.id == id do
        %Bibliotheque{livre | statut: statut}
      else
        livre
      end
    end)
    {state, :ok}
  end
end

# Module de création d'utilisateurs

defmodule Utilisateurs do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: :utilisateurs)
  end

  def add_utilisateur(utilisateur) do
    GenServer.call(:utilisateurs, {:add_utilisateur, utilisateur})
  end

  def get_utilisateur(id) do
    GenServer.call(:utilisateurs, {:get_utilisateur, id})
  end

  def add_emprunt(id, id_livre) do
    GenServer.call(:utilisateurs, {:add_emprunt, id, id_livre})
  end

  def handle_call({:add_utilisateur, utilisateur}, _, state) do
    {_, nouveau_id} = Enum.max_by(state, & &1.id)
    utilisateur_avec_id = %Utilisateur{utilisateur | id: nouveau_id + 1}
    {state, utilisateur_avec_id}
  end

  def handle_call({:get_utilisateur, id}, _, state) do
    {utilisateur, _} = Enum.find(state, &(&1.id == id))
    {utilisateur, state}
  end

  def handle_call({:add_emprunt, id, id_livre}, _, state) do
    state = Enum.map(state, fn utilisateur ->
      if utilisateur.id == id do
        %Utilisateur{utilisateur | emprunts: [id_livre | utilisateur.emprunts]}
      else
        utilisateur
      end
    end)
    {state, :ok}
  end
end

# Main

Process.start(Bibliotheques, :start_link, [])
Process.start(Utilisateurs, :start_link, [])

livre1 = %Bibliotheque{titre: "Le Petit Prince", auteur: "Antoine de Saint-Exupéry", statut: :disponible}
Bibliotheques.add_livre(livre1)

utilisateur1 = %Utilisateur{nom: "Jean", emprunts: []}
Utilisateurs.add_utilisateur(utilisateur1)

Bibliotheques.update_statut(livre1.id, :emprunté)
Utilisateurs.add_emprunt(utilisateur1.id, livre1.id)

livre1 = Bibliotheques.get_livre(livre1.id)
utilisateur1 = Utilisateurs.get_utilisateur(utilisateur1.id)

IO.puts "Livre emprunté : #{livre1.titre}"
IO.puts "Emprunteur : #{utilisateur1.nom}"
```

**Explication du code :**

* Ce code implémente un système de gestion de bibliothèque en Elixir.
* Il utilise le modèle GenServer pour gérer l'état des bibliothèques et des utilisateurs.
* Il définit des types de données `Bibliotheque` et `Utilisateur`.
* Il crée des modules `Bibliotheques` et `Utilisateurs` pour gérer les bibliothèques et les utilisateurs respectivement.
* Le module `Main` démarre les processus GenServer pour les bibliothèques et les utilisateurs.
* Il crée un livre, un utilisateur et simule un emprunt.
* Il affiche le titre du livre emprunté et le nom de l'emprunteur.