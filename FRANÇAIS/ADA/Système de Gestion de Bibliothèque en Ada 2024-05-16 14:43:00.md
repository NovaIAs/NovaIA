**Système de gestion de bibliothèque en Ada**

Ce code gère une bibliothèque avec plusieurs fonctionnalités avancées :

```ada
with Ada.Text_IO;
with Ada.Collections;
with Ada.Calendar;    -- Pour les dates emprunt/retour
with Ada.Exceptions;   -- Pour gestion des erreurs

procedure Gestion_Biblio is

   type Livre is record
      Titre : String(1..50);
      Auteur : String(1..20);
      Disponibilite : Boolean;
   end record;

   type Livre_Table is array (Positive range <>) of Livre;
   type Adherent_Table is array (Positive range <>) of String;

   type Emprunt is record
      Adherent : Positive;
      Livre : Positive;
      Date_Emprunt : Calendar.Time;
      Date_Retour : Calendar.Time;
   end record;

   type Emprunt_Table is array (Positive range <>) of Emprunt;

   Biblio : Livre_Table;
   Adherents : Adherent_Table;
   Emprunts : Emprunt_Table;

   -- Index des livres/adherents/emprunts
   Biblio_Index : Ada.Containers.Ordered_Table(Positive, Livre);
   Adherent_Index : Ada.Containers.Ordered_Table(Positive, String);
   Emprunt_Index : Ada.Containers.Ordered_Table(Positive, Emprunt);

   -- Initialisation du système
   procedure Initialiser is
      use type Biblio_Table, Adherent_Table, Emprunt_Table;
      begin
         null;     -- Initialisation par défaut
      end Initialiser;

   -- Ajouter un livre
   procedure Ajouter_Livre (T : in out Livre_Table; L : in Livre) is
      begin
         -- Vérifier si le livre existe déjà
         if Biblio_Index(L.Titre) /= null then
            raise Ada.Exceptions.Constraint_Error;
         end if;

         -- Ajouter le livre à la table et mettre à jour l'index
         T.Last +:= 1;
         T(T.Last) := L;
         Biblio_Index.Insert(L.Titre, T.Last);
      end Ajouter_Livre;

   -- Ajouter un adhérent
   procedure Ajouter_Adherent (T : in out Adherent_Table; A : in String) is
      begin
         -- Vérifier si l'adhérent existe déjà
         if Adherent_Index(A) /= null then
            raise Ada.Exceptions.Constraint_Error;
         end if;

         -- Ajouter l'adhérent à la table et mettre à jour l'index
         T.Last +:= 1;
         T(T.Last) := A;
         Adherent_Index.Insert(A, T.Last);
      end Ajouter_Adherent;

   -- Emprunter un livre
   procedure Emprunter (Adherent : in Positive; Livre : in Positive;
                        Date_Emprunt : in Calendar.Time; Date_Retour : in Calendar.Time) is
      begin
         -- Vérifier si le livre est disponible
         if Biblio(Livre).Disponibilite = False then
            raise Ada.Exceptions.Constraint_Error;
         end if;

         -- Vérifier si l'adhérent existe
         if Adherent_Index(Adherent) = null then
            raise Ada.Exceptions.Constraint_Error;
         end if;

         -- Ajouter l'emprunt à la table et mettre à jour l'index
         Emprunts.Last +:= 1;
         Emprunt(Emprunts.Last).Adherent := Adherent;
         Emprunt(Emprunts.Last).Livre := Livre;
         Emprunt(Emprunts.Last).Date_Emprunt := Date_Emprunt;
         Emprunt(Emprunts.Last).Date_Retour := Date_Retour;
         Emprunt_Index.Insert(Emprunts.Last, Emprunts.Last);

         -- Mettre à jour la disponibilité du livre
         Biblio(Livre).Disponibilite := False;
      end Emprunter;

   -- Retourner un livre
   procedure Retourner (Emprunt_ID : in Positive) is
      begin
         -- Vérifier si l'emprunt existe
         if Emprunt_Index(Emprunt_ID) = null then
            raise Ada.Exceptions.Constraint_Error;
         end if;

         -- Mettre à jour la disponibilité du livre
         Biblio(Emprunt(Emprunt_ID).Livre).Disponibilite := True;

         -- Supprimer l'emprunt de la table et mettre à jour l'index
         Emprunt_Index.Delete(Emprunt_ID);
         Emprunts(Emprunt_ID) := Emprunts(Emprunts.Last);
         Emprunts.Last -:= 1;
      end Retourner;

   -- Rechercher un livre par titre
   function Rechercher_Livre (Titre : in String) return Positive is
      begin
         return Biblio_Index(Titre);
      end Rechercher_Livre;

   -- Rechercher un adhérent par nom
   function Rechercher_Adherent (Nom : in String) return Positive is
      begin
         return Adherent_Index(Nom);
      end Rechercher_Adherent;

   -- Afficher la liste des livres
   procedure Afficher_Livres is
      use type Livre_Table;
      begin
         for L in Livre_Table'Range loop
            Ada.Text_IO.Put_Line(L.Titre);
         end loop;
      end Afficher_Livres;

   -- Afficher la liste des adhérents
   procedure Afficher_Adherents is
      use type Adherent_Table;
      begin
         for A in Adherent_Table'Range loop
            Ada.Text_IO.Put_Line(A);
         end loop;
      end Afficher_Adherents;

   -- Afficher la liste des emprunts
   procedure Afficher_Emprunts is
      use type Emprunt_Table;
      begin
         for E in Emprunt_Table'Range loop
            Ada.Text_IO.Put_Line(Emprunt(E).Adherent & " " & Emprunt(E).Livre & " " & Emprunt(E).Date_Emprunt & " " & Emprunt(E).Date_Retour);
         end loop;
      end Afficher_Emprunts;

begin
   -- Initialisation du système
   Initialiser;

   -- Ajout de quelques livres
   Ajouter_Livre(Biblio, (Titre => "Le Seigneur des Anneaux", Auteur => "J.R.R. Tolkien", Disponibilite => True));
   Ajouter_Livre(Biblio, (Titre => "1984", Auteur => "George Orwell", Disponibilite => True));

   -- Ajout de quelques adhérents
   Ajouter_Adherent(Adherents, "Alice");
   Ajouter_Adherent(Adherents, "Bob");

   -- Emprunt d'un livre
   Emprunter(Rechercher_Adherent("Alice"), Rechercher_Livre("Le Seigneur des Anneaux"), Calendar.Clock, Calendar.Clock + 7);

   -- Retour du livre
   Retourner(1);

   -- Affichage des informations
   Ada.Text_IO.Put_Line("Livres :");
   Afficher_Livres;

   Ada.Text_IO.Put_Line("Adhérents :");
   Afficher_Adherents;

   Ada.Text_IO.Put_Line("Emprunts :");
   Afficher_Emprunts;
end Gestion_Biblio;
```

**Explications du code :**

* Les types `Livre`, `Adherent` et `Emprunt` représentent les livres, les adhérents et les emprunts.
* Les tables `Livre_Table`, `Adherent_Table` et `Emprunt_Table` stockent les données.
* Les index `Biblio_Index`, `Adherent_Index` et `Emprunt_Index` permettent de rechercher rapidement les données.
* Les procédures `Ajouter_Livre`, `Ajouter_Adherent`, `Emprunter` et `Retourner` gèrent les opérations de la bibliothèque.
* Les fonctions `Rechercher_Livre` et `Rechercher_Adherent` permettent de rechercher des données par titre ou nom.
* Les procédures `Afficher_Livres`, `Afficher_Adherents` et `Afficher_Emprunts` affichent les données stockées.