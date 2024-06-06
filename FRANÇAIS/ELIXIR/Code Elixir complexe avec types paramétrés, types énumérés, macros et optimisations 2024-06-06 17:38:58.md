Ok, voici un code complexe en Elixir qui couvre différents aspects du langage :

```elixir
defmodule ExempleComplexe do
  defmodule TypeParamétré do
    defstruct [:valeur]

    def new(valeur) do
      %__MODULE__{valeur: valeur}
    end
  end

  defmodule TypeEnuméré do
    defenum [:un, :deux, :trois]
  end

  defmacro déconstructeur(type) do
    quote do
      def unquote(var) when is_struct(unquote(var), unquote(type.new(1))) do
        {:ok, unquote(var).valeur}
      end

      def unquote(var) when is_enum(unquote(var), unquote(type)) do
        {:ok, unquote(var)}
      end

      def unquote(var) do
        {:error, "Type non géré"}
      end
    end
  end

  defmacro composition_fonctions do
    quote do
      defp unquote(var) do
        unquote(var)
      end
    end
  end

  defp fonction_pure(valeur) do
    valeur + 1
  end

  def appel_fonctions_pures(valeur) do
    fonction_pure(fonction_pure(fonction_pure(fonction_pure(valeur))))
  end

  @spec pipeline_fusion(list) :: list
  def pipeline_fusion(liste) do
    liste
    |> Enum.map(fn (x) -> 2 * x end)
    |> Enum.reduce(fn (x, acc) -> acc + x end, 0)
  end

  @spec broadcast_fusion(list) :: map
  def broadcast_fusion(liste) do
    Enum.reduce(liste, %{}, fn (x, acc) ->
      Map.put(acc, x, 2 * x)
    end)
  end

  @spec stream_fusion(list) :: list
  def stream_fusion(liste) do
    liste
    |> Stream.map(fn (x) -> 2 * x end)
    |> Stream.filter(fn (x) -> rem(x, 2) == 0 end)
    |> Enum.to_list()
  end

  @spec parallélisme_processeurs(list) :: list
  def parallélisme_processeurs(liste) do
    liste
    |> Enum.map(fn (x) ->
      Agent.start_link(fn -> 2 * x end)
    end)
    |> Enum.map(fn (agent) -> Agent.get(agent) end)
  end

  @spec calcul_parallèle(list) :: list
  def calcul_parallèle(liste) do
    liste
    |> Enum.map(fn (x) ->
      spawn(fn -> 2 * x end)
    end)
    |> Enum.map(fn (pid) -> receive do
      {:ok, résultat} -> résultat
    end end)
  end

  def main() do
    type_param = TypeParamétré.new(1)
    IO.inspect(TypeParamétré.valeur(type_param))

    type_enum = TypeEnuméré.deux()
    IO.inspect(TypeEnuméré.to_string(type_enum))

    IO.inspect(déconstructeur(TypeEnuméré).deux())

    fonctions = Macro.expand(composition_fonctions(), Elixir)
    IO.inspect(fonctions.unquote())

    IO.inspect(appel_fonctions_pures(1))

    liste = [1, 2, 3, 4, 5]
    IO.inspect(pipeline_fusion(liste))

    IO.inspect(broadcast_fusion(liste))

    IO.inspect(stream_fusion(liste))

    IO.inspect(parallélisme_processeurs(liste))

    IO.inspect(calcul_parallèle(liste))
  end
end

ExempleComplexe.main()
```

**Explication du code :**

* **Modules et macros :** Le code utilise des modules pour organiser le code et des macros pour générer du code dynamique.
* **Types paramétrés et énumérés :** Le code définit un type paramétré (**TypeParamétré**) et un type énuméré (**TypeEnuméré**).
* **Déconstructeur de macro :** La macro **déconstructeur** génère du code pour déconstruire des types en fonction de leur valeur.
* **Composition de fonctions :** La macro **composition_fonctions** génère du code pour composer des fonctions.
* **Fonctions pures :** La fonction **fonction_pure** est une fonction pure qui renvoie son argument incrémenté.
* **Appel de fonctions pures :** La fonction **appel_fonctions_pures** appelle plusieurs fois la fonction **fonction_pure**, démontrant la composition de fonctions.
* **Fusion de pipeline :** La fonction **pipeline_fusion** utilise la fusion de pipeline Elixir pour optimiser une série d'opérations sur une liste.
* **Fusion de broadcast :** La fonction **broadcast_fusion** utilise la fusion de broadcast Elixir pour optimiser une opération de mise à jour de carte sur une liste.
* **Fusion de flux :** La fonction **stream_fusion** utilise la fusion de flux Elixir pour optimiser une série d'opérations sur un flux de données.
* **Parallélisme de processus :** La fonction **parallélisme_processeurs** crée plusieurs processus pour effectuer des calculs en parallèle.
* **Calcul parallèle :** La fonction **calcul_parallèle** utilise Erlang **spawn** pour créer des processus pour effectuer des calculs en parallèle.

Ce code couvre divers concepts Elixir, notamment :

* Modules
* Macros
* Typage
* Composition de fonctions
* Programmation fonctionnelle
* Parallélisme