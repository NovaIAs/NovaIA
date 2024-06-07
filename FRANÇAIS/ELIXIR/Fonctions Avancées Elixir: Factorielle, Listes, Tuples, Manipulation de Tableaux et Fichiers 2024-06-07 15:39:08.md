```elixir
defmodule Factorielle do
  def fact(0), do: 1
  def fact(n), do: n * fact(n-1)
end

defmodule ReprésentationListes do
  def liste_vide(), do: []
  def ajout_tête(valeur, liste), do: [valeur | liste]
  def liste_première(liste), do: hd liste
  def liste_reste(liste), do: tl liste
end

defmodule ManipulationListes do
  def parcourir_liste(liste, fonction), do: Enum.each(liste, fonction)
  def map_liste(liste, fonction), do: Enum.map(liste, fonction)
  def filtrer_liste(liste, condition), do: Enum.filter(liste, condition)
  def ordonner_liste(liste, critère), do: Enum.sort_by(liste, critère)
end

defmodule ManipulerTuples do
  def élément_tuple(tuple, index), do: elem(tuple, index)
  def mettre_à_jour_tuple(tuple, index, valeur), do: put_elem(tuple, index, valeur)
  def permuter_avec_tuple(tuple), do: Tuple.to_list(tuple) |> Enum.reverse() |> Tuple.from_list()
end

defmodule SaisieClavier do
  def demander_texte(prompt), do: IO.gets prompt
  def demander_nombre(), do: IO.gets "" |> String.to_integer
end

defmodule ConversionTypes do
  def convertir_chaîne_vers_nombre(chaîne), do: String.to_integer(chaîne)
  def convertir_nombre_vers_chaîne(nombre), do: Integer.to_string(nombre)
  def convertir_liste_vers_tuple(liste), do: Tuple.from_list(liste)
  def convertir_tuple_vers_liste(tuple), do: Tuple.to_list(tuple)
end

defmodule ManipulationFichiers do
  def ouvrir_fichier(nom_fichier, mode), do: File.open(nom_fichier, mode)
  def lire_ligne_fichier(flux), do: IO.gets(flux)
  def écrire_ligne_fichier(flux, ligne), do: IO.puts(flux, ligne)
  def fermer_fichier(flux), do: File.close(flux)
end

defmodule ManipulationTableaux do
  def tableau_vide(taille), do: :erlang.make_tuple(taille)
  def ajouter_tableau(tableau, valeur), do: :erlang.append_element(tableau, valeur)
  def obtenir_élément_tableau(tableau, index), do: :erlang.element(tableau, index)
  def mettre_à_jour_élément_tableau(tableau, index, valeur), do: :erlang.setelement(tableau, index, valeur)
end
```

**Ce code complexe Elixr couvre divers concepts avancés et fonctionnalités de la langue.**

**Factorielle**

* Calcule la factorielle d'un nombre entier.

**Représentation de listes**

* Fournit des fonctions pour créer et manipuler des listes vides et liées.

**Manipulation de listes**

* Contient des fonctions pour parcourir, mapper, filtrer et trier des listes.

**Manipulation de tuples**

* Fournit des fonctions pour accéder aux éléments et modifier les tuples.

**Saisie au clavier**

* Demande du texte et des nombres à l'utilisateur via la console.

**Conversion de types**

* Contient des fonctions pour convertir entre les types de données courants.

**Manipulation de fichiers**

* Fournit des fonctions pour ouvrir, lire, écrire et fermer des fichiers texte.

**Manipulation de tableaux**

* Crée et manipule des tableaux de taille fixe, utilisés pour stocker des séquences d'éléments.