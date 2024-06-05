**Module de Gestion de Compte Bancaire**

```ada
package Banque is

   Compte type is
      record
         solde : Float;
         numero : String (1..10);
      end record;

   Compte_base type is
      abstract new Compte with private;

   type Compte_courant is new Compte_base with private;

   Compte_courant_ops : operations Compte_courant;

   procédure Débiter (C : in out Compte_courant; Somme : in Float);
   procédure Créditer (C : in out Compte_courant; Somme : in Float);

private
   Compte_courant_ops : constant operations Compte_courant
      is (
         procédure Débiter (C : in out Compte_courant; Somme : in Float) is
            begin
               C.solde := C.solde - Somme;
            end;
         procédure Créditer (C : in out Compte_courant; Somme : in Float) is
            begin
               C.solde := C.solde + Somme;
            end;
      );

end Banque;
```

**Module de Gestion de Bibliothèque**

```ada
package Bibliothèque is

   Livre type is
      record
         titre         : String (1..50);
         auteur         : String (1..50);
         nombre_pages   : Integer;
         stock_disponible : Integer;
      end record;

   Emprunt type is
      record
         id_emprunteur   : Integer;
         id_livre         : Integer;
         date_emprunt     : Date;
         date_restitution : Date;
      end record;

   type Bibliothèque is
      record
         livres       : array (1..100) of Livre;
         emprunts      : array (1..100) of Emprunt;
         nb_livres    : Integer;
         nb_emprunteurs : Integer;
      end record;

   Bibliothèque_ops : operations Bibliothèque;

   procédure Ajouter_livre (B : in out Bibliothèque; L : in Livre);
   procédure Supprimer_livre (B : in out Bibliothèque; Titre : in String (1..50));
   procédure Emprunter_livre (B : in out Bibliothèque; Id_emprunteur : in Integer; Titre : in String (1..50); Date_emprunt : in Date);
   procédure Restituer_livre (B : in out Bibliothèque; Id_emprunteur : in Integer; Titre : in String (1..50); Date_restitution : in Date);
   fonction Rechercher_livre (B : in Bibliothèque; Titre : in String (1..50)) return Livre;
   fonction Rechercher_emprunteur (B : in Bibliothèque; Id_emprunteur : in Integer) return Emprunt;

private
   Bibliothèque_ops : constant operations Bibliothèque
      is (
         procédure Ajouter_livre (B : in out Bibliothèque; L : in Livre) is
            begin
               B.livres (B.nb_livres + 1) := L;
               B.nb_livres := B.nb_livres + 1;
            end;
         procédure Supprimer_livre (B : in out Bibliothèque; Titre : in String (1..50)) is
            begin
               Effacer(B.livres);
               for I in 1..B.nb_livres loop
                  if B.livres (I).titre /= Titre then
                     B.livres (I) := B.livres (I + 1);
                  end if;
               end loop;
               B.nb_livres := B.nb_livres - 1;
            end;
         procédure Emprunter_livre (B : in out Bibliothèque; Id_emprunteur : in Integer; Titre : in String (1..50); Date_emprunt : in Date) is
            begin
               L := Rechercher_livre (B, Titre);
               if L.stock_disponible > 0 then
                  B.emprunts (B.nb_emprunteurs + 1) := Emprunt (Id_emprunteur, L.id, Date_emprunt, Date);
                  B.nb_emprunteurs := B.nb_emprunteurs + 1;
                  L.stock_disponible := L.stock_disponible - 1;
               end if;
            end;
         procédure Restituer_livre (B : in out Bibliothèque; Id_emprunteur : in Integer; Titre : in String (1..50); Date_restitution : in Date) is
            begin
               L := Rechercher_livre (B, Titre);
               E := Rechercher_emprunteur (B, Id_emprunteur);
               if E is not null then
                  if L.id = E.id_livre and E.date_restitution = Date then
                     L.stock_disponible := L.stock_disponible + 1;
                     Effacer(B.emprunts);
                     for I in 1..B.nb_emprunteurs loop
                        if B.emprunts (I).id_emprunteur /= Id_emprunteur or B.emprunts (I).id_livre /= L.id then
                           B.emprunts (I) := B.emprunts (I + 1);
                        end if;
                     end loop;
                     B.nb_emprunteurs := B.nb_emprunteurs - 1;
                  end if;
               end if;
            end;
         fonction Rechercher_livre (B : in Bibliothèque; Titre : in String (1..50)) return Livre is
            begin
               for I in 1..B.nb_livres loop
                  if B.livres (I).titre = Titre then
                     return B.livres (I);
                  end if;
               end loop;
               return null;
            end;
         fonction Rechercher_emprunteur (B : in Bibliothèque; Id_emprunteur : in Integer) return Emprunt is
            begin
               for I in 1..B.nb_emprunteurs loop
                  if B.emprunts (I).id_emprunteur = Id_emprunteur then
                     return B.emprunts (I);
                  end if;
               end loop;
               return null;
            end;
      );

end Bibliothèque;
```

**Application de Gestion Bancaire et de Bibliothèque**

```ada
with Banque;
with Bibliothèque;

procedure Gestion_banque_bibliothèque is
   Compte : Banque.Compte_courant;
   Bibliothèque : Bibliothèque.Bibliothèque;
begin
   -- Gestion bancaire
   Banque.Créditer (Compte, 1000.0);
   Banque.Débiter (Compte, 500.0);

   -- Gestion de bibliothèque
   Bibliothèque.Ajouter_livre (Bibliothèque, Livre ("Le Seigneur des Anneaux", "Tolkien", 500, 10));
   Bibliothèque.Emprunter_livre (Bibliothèque, 1, "Le Seigneur des Anneaux", Date' (2023, 03, 01));
   Bibliothèque.Restituer_livre (Bibliothèque, 1, "Le Seigneur des Anneaux", Date' (2023, 03, 15));
end Gestion_banque_bibliothèque;
```

**Explication du code**

Le code est divisé en trois modules : un module de gestion de compte bancaire (`Banque`), un module de gestion de bibliothèque (`Bibliothèque`) et une application principale (`Gestion_banque_bibliothèque`) qui utilise les deux modules.

**Module de Gestion de Compte Bancaire (Banque)**

Ce module définit un type `Compte` représentant les comptes bancaires et une interface `Compte_base` abstraite qui définit les opérations de base pour les comptes bancaires. Il implémente un type `Compte_courant` spécifique qui hérite de `Compte_base` et définit les opérations `Débiter` et `Créditer` pour gérer les transactions sur le compte.

**Module de Gestion de Bibliothèque (Bibliothèque)**

Ce module définit un type `Livre` représentant les livres, un type `Emprunt` représentant les emprunts et un type `Bibliothèque` représentant l'ensemble de la bibliothèque. Il définit une interface `Bibliothèque` abstraite qui définit les opérations de base pour gérer une bibliothèque. Il implémente des opérations telles que `Ajouter_livre`, `Supprimer_livre`, `Emprunter_livre`, `Restituer_livre`, `Rechercher_livre` et `Rechercher_emprunteur` pour gérer les livres, les emprunts et les emprunteurs.

**Application de Gestion Bancaire et de Bibliothèque (Gestion_banque_bibliothèque)**

L'application principale utilise les modules `Banque` et `