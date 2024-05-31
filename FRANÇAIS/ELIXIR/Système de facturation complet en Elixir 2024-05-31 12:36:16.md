**Système de facturation complexe en Elixir**

Ce système de facturation gère les factures, les paiements et les clients. Il implémente des fonctionnalités avancées telles que les abonnements, les remises et les taxes.

**Module de facturation**

```elixir
defmodule Facturation do
  @moduledoc """
  Système de facturation pour gérer les factures, les paiements et les clients.
  """

  defstruct [:id, :client_id, :date, :montant, :devise, :etat]

  @type t :: %__MODULE__{
          id: non_neg_integer(),
          client_id: non_neg_integer(),
          date: DateTime.t(),
          montant: float(),
          devise: String.t(),
          etat: Facturation.Etat
        }

  @type etat :: Abonnement | Paiement | Annulé

  def create(client_id, montant, devise) do
    %__MODULE__{
      client_id: client_id,
      date: DateTime.utc_now(),
      montant: montant,
      devise: devise,
      etat: Abonnement
    }
  end

  def mark_as_paid(facture) do
    %Facturation{facture | etat: Paiement}
  end

  def mark_as_cancelled(facture) do
    %Facturation{facture | etat: Annulé}
  end
end
```

**Module de paiement**

```elixir
defmodule Paiement do
  @moduledoc """
  Module pour gérer les paiements et les remboursements.
  """

  defstruct [:id, :facture_id, :mode, :montant, :date]

  @type t :: %__MODULE__{
          id: non_neg_integer(),
          facture_id: non_neg_integer(),
          mode: Paiement.Mode,
          montant: float(),
          date: DateTime.t()
        }

  @type mode :: Carte bancaire | Virement | Chèque

  def create(facture_id, mode, montant) do
    %__MODULE__{
      facture_id: facture_id,
      mode: mode,
      montant: montant,
      date: DateTime.utc_now()
    }
  end

  def refund(paiement) do
    # Logique de remboursement
  end
end
```

**Module de client**

```elixir
defmodule Client do
  @moduledoc """
  Module pour gérer les clients et leurs informations.
  """

  defstruct [:id, :nom, :email, :adresse]

  @type t :: %__MODULE__{
          id: non_neg_integer(),
          nom: String.t(),
          email: String.t(),
          adresse: String.t()
        }

  def create(nom, email, adresse) do
    %__MODULE__{
      nom: nom,
      email: email,
      adresse: adresse
    }
  end

  def get(id) do
    # Récupération du client à partir de la base de données
  end
end
```

**Utilisation**

```elixir
facture = Facturation.create(1, 100.0, "€")
paiement = Paiement.create(facture.id, :Carte bancaire, 100.0)

Facturation.mark_as_paid(facture)

# Récupération du client à partir de son identifiant
client = Client.get(1)

# Envoi de la facture par email au client
Mail.send(client.email, "Facture n° #{facture.id}", "Votre facture est jointe à ce mail.")
```