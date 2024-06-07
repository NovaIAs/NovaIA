**Gestionnaire de commandes complexe en Elixir**

```elixir
defmodule OrderManager do
  defdelegate create_order(order_params), to: Order

  defdelegate update_order(id, order_params), to: Order

  defdelegate cancel_order(id), to: Order

  defdelegate get_order(id), to: Order

  defdelegate list_orders(), to: Order

  defdelegate get_orders_for_customer(customer_id), to: Order

  defdelegate create_order_item(order_item_params), to: OrderItem

  defdelegate update_order_item(id, order_item_params), to: OrderItem

  defdelegate delete_order_item(id), to: OrderItem

  defdelegate create_shipment(shipment_params), to: Shipment

  defdelegate update_shipment(id, shipment_params), to: Shipment

  defdelegate cancel_shipment(id), to: Shipment

  defdelegate get_shipment(id), to: Shipment

  defdelegate list_shipments(), to: Shipment

  defdelegate get_shipments_for_order(order_id), to: Shipment

  def handle_event(event) do
    case event.type do
      "order_created" ->
        create_order(event.data)
        {:ok, :handled}

      "order_updated" ->
        update_order(event.data.id, event.data)
        {:ok, :handled}

      "order_cancelled" ->
        cancel_order(event.data.id)
        {:ok, :handled}

      "order_item_created" ->
        create_order_item(event.data)
        {:ok, :handled}

      "order_item_updated" ->
        update_order_item(event.data.id, event.data)
        {:ok, :handled}

      "order_item_deleted" ->
        delete_order_item(event.data.id)
        {:ok, :handled}

      "shipment_created" ->
        create_shipment(event.data)
        {:ok, :handled}

      "shipment_updated" ->
        update_shipment(event.data.id, event.data)
        {:ok, :handled}

      "shipment_cancelled" ->
        cancel_shipment(event.data.id)
        {:ok, :handled}

      _ ->
        {:error, :unknown_event}
    end
  end
end
```

**Explication du code**

Ce code Elixir implémente un gestionnaire de commandes complexe qui gère plusieurs types d'entités et d'événements liés aux commandes.

**Entités**

* **Commande** : Représente une commande passée par un client.
* **Article de commande** : Représente un article spécifique inclus dans une commande.
* **Expédition** : Représente l'expédition d'une commande.

**Événements**

* **order_created** : Événement déclenché lorsqu'une nouvelle commande est passée.
* **order_updated** : Événement déclenché lorsqu'une commande existante est mise à jour.
* **order_cancelled** : Événement déclenché lorsqu'une commande est annulée.
* **order_item_created** : Événement déclenché lorsqu'un nouvel article de commande est ajouté à une commande.
* **order_item_updated** : Événement déclenché lorsqu'un article de commande existant est mis à jour.
* **order_item_deleted** : Événement déclenché lorsqu'un article de commande est supprimé d'une commande.
* **shipment_created** : Événement déclenché lorsqu'une nouvelle expédition est créée pour une commande.
* **shipment_updated** : Événement déclenché lorsqu'une expédition existante est mise à jour.
* **shipment_cancelled** : Événement déclenché lorsqu'une expédition est annulée.

**Fonctions**

* **create_order** : Crée une nouvelle commande.
* **update_order** : Met à jour une commande existante.
* **cancel_order** : Annule une commande existante.
* **get_order** : Obtient une commande spécifique.
* **list_orders** : Obtient la liste de toutes les commandes.
* **get_orders_for_customer** : Obtient les commandes d'un client spécifique.
* **create_order_item** : Crée un nouvel article de commande.
* **update_order_item** : Met à jour un article de commande existant.
* **delete_order_item** : Supprime un article de commande.
* **create_shipment** : Crée une nouvelle expédition pour une commande.
* **update_shipment** : Met à jour une expédition existante.
* **cancel_shipment** : Annule une expédition existante.
* **get_shipment** : Obtient une expédition spécifique.
* **list_shipments** : Obtient la liste de toutes les expéditions.
* **get_shipments_for_order** : Obtient les expéditions d'une commande spécifique.
* **handle_event** : Traite les différents types d'événements liés aux commandes, en créant, mettant à jour ou annulant les entités correspondantes.

Ce gestionnaire de commandes est conçu pour être extensif, flexible et capable de gérer des événements complexes et interconnectés liés aux commandes.