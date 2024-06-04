**Gestion d'un système de messagerie complexe en Elixir**

```elixir
defmodule MessageSystem do
  def start_link do
    Agent.start_link(fn -> [] end, name: :messages)
  end

  def add_message(message) do
    Agent.update(:messages, fn messages -> [message] ++ messages end)
  end

  def get_messages do
    Agent.get(:messages, &(&1))
  end

  def subscribe(consumer) do
    :ok = Agent.subscribe(:messages, consumer, &handle_message/2)
  end

  defp handle_message(message, consumer) do
    consumer.handle_message(message)
  end
end

defmodule Consumer do
  def start_link(options) do
    Agent.start_link(fn -> 0 end, name: :consumer_count)
  end

  def handle_message(message) do
    Agent.update(:consumer_count, fn count -> count + 1 end)
    IO.puts "Message reçu : #{message}"
  end
end

defmodule Producer do
  def start_link do
    Agent.start_link(fn -> [] end, name: :messages_to_send)
  end

  def add_message(message) do
    Agent.update(:messages_to_send, fn messages -> messages ++ [message] end)
  end

  def send_messages do
    messages = Agent.get(:messages_to_send, &(&1))
    Agent.update(:messages_to_send, fn -> [] end)

    for message <- messages, do: MessageSystem.add_message(message)
  end
end
```

**Explication du code**

Ce code Elixir implémente un système de messagerie complexe avec des producteurs et des consommateurs.

**Gestionnaire de messages**

Le module `MessageSystem` gère une liste de messages en mémoire. Il fournit des fonctions pour ajouter de nouveaux messages (`add_message`), obtenir la liste des messages (`get_messages`) et s'abonner aux mises à jour de la liste (`subscribe`).

**Consommateur**

Le module `Consumer` représente un consommateur de messages. Il reçoit les messages via une fonction de rappel et les gère.

**Producteur**

Le module `Producer` représente un producteur de messages. Il gère une liste de messages à envoyer. Il fournit des fonctions pour ajouter de nouveaux messages (`add_message`) et envoyer les messages en attente (`send_messages`).

**Utilisation**

Pour utiliser ce système de messagerie :

1. Démarrez le gestionnaire de messages : `start_link`
2. Démarrez un consommateur : `start_link`
3. Abonnez le consommateur au gestionnaire de messages : `MessageSystem.subscribe`
4. Démarrez un producteur : `start_link`
5. Ajoutez des messages au producteur : `Producer.add_message`
6. Envoyez les messages : `Producer.send_messages`
7. Le consommateur recevra les messages envoyés via la fonction de rappel `handle_message`