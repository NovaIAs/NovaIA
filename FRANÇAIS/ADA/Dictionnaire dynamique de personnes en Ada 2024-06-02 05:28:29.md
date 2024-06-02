```ada
with Ada.Text_IO, Ada.Containers.Ordered_Maps, Ada.Containers.Indefinite_Ordered_Maps;
with function "Ada.Text_IO.Get_Line" (Item : out String) return Boolean;
with function "Ada.Text_IO.Put_Line" (Item : in String) return Boolean;

procedure Main is

   type Personne is record
      Nom, Prenom : String (1..20);
   end record;

   type Dictionnaire is Ada.Containers.Indefinite_Ordered_Maps.Ordered_Map (String, Personne);
   type Dictionnaire_Iterateur is Ada.Containers.Ordered_Maps.Cursor (Dictionnaire);

   procedure Ajouter_Personne (Dictio : in out Dictionnaire; Personne : in Personne) is
      Dictio (Personne.Nom) := Personne;
   end Ajouter_Personne;

   procedure Afficher_Personne (Dictio : in Dictionnaire; Nom : in String) is
      Put_Line (Nom & " - " & Dictio (Nom).Prenom);
   end Afficher_Personne;

   procedure Main is
      var Dict : Dictionnaire;
      var Iterateur : Dictionnaire_Iterateur := Dict.First;

      -- Ajoute quelques personnes au dictionnaire
      Ajouter_Personne (Dict, (Nom => "Dupont", Prenom => "Jean"));
      Ajouter_Personne (Dict, (Nom => "Durand", Prenom => "Marie"));
      Ajouter_Personne (Dict, (Nom => "Martin", Prenom => "Paul"));

      -- Affiche le contenu du dictionnaire
      while Iterateur /= Dict.Last loop
         Afficher_Personne (Dict, Iterateur.Key);
         Iterateur := Dict.Next (Iterateur);
      end loop;

   begin
      Main;
   end Main;
end Ada.Main;
```

**Explication du code :**

Ce code est un programme ADA complet qui utilise des structures de données avancées et des itérateurs pour gérer un dictionnaire de personnes.

**Type Personne** :

* Représente un enregistrement contenant deux champs de type chaîne : `Nom` et `Prenom`.

**Type Dictionnaire** :

* Représente un dictionnaire ordonné basé sur le composant **Ada.Containers.Indefinite_Ordered_Maps**.
* Stocke des paires clé-valeur, où la clé est une chaîne et la valeur est une valeur `Personne`.
* De par sa nature ordonnée, les paires clé-valeur sont stockées et récupérées par ordre alphabétique de la clé.

**Type Dictionnaire_Iterateur** :

* Représente un itérateur pour parcourir les paires clé-valeur du dictionnaire.
* Fournit des méthodes pour se déplacer vers le premier, le dernier, le suivant et le précédent élément du dictionnaire.

**Procédure Ajouter_Personne** :

* Ajoute une nouvelle personne au dictionnaire.
* Si une personne avec la même clé existe déjà, elle remplace la valeur existante.

**Procédure Afficher_Personne** :

* Affiche le nom et le prénom d'une personne donnée, trouvée dans le dictionnaire.

**Procédure Main** :

* Crée une variable `Dict` de type `Dictionnaire` qui sera utilisée pour stocker les personnes.
* Crée une variable `Iterateur` de type `Dictionnaire_Iterateur` pour parcourir le dictionnaire.
* Ajoute trois personnes au dictionnaire.
* Parcourt le dictionnaire à l'aide de l'itérateur et affiche le nom et le prénom de chaque personne.

Ce code illustre l'utilisation de structures de données complexes en ADA, ainsi que les méthodes d'ajout, d'itération et de récupération de données à partir d'un dictionnaire ordonné.