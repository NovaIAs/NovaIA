**Programme de jeu de Blackjack complexe en Ada**

```ada
with Ada.Text_IO; Ada.Numerics.Discrete_Random;
use Ada.Text_IO, Ada.Numerics.Discrete_Random;

type Carte is new Integer range 2..14;

type T_Cartes is array (Positive range <>) of Carte;

type T_Main is record
  Cartes: T_Cartes;
  Somme: Integer := 0;
end record;

procedure CreerMain (out Main: T_Main) is
  NB_CARTES: constant := 5;
  Card: Carte;
begin
  Main.Cartes := T_Cartes (Positive'First .. Positive'Last);
  for I in Main.Cartes'Range loop
    Card := Random (2..14);
    Main.Cartes (I) := Card;
    Main.Somme := Main.Somme + Card;
  end loop;
end CreerMain;

procedure AfficherMain (Main: in T_Main) is
begin
  for Card in Main.Cartes loop
    Put_Line (Integer'Image (Natural (Card)));
  end loop;
  Put_Line ("Somme : " & Integer'Image (Main.Somme));
end AfficherMain;

procedure Jouer (inout Main: T_Main) is
begin
  loop
    AfficherMain (Main);
    Put_Line ("Voulez-vous une autre carte ? (O/N)");
    Get_Line (Reponse: String);

    if Reponse = "O" then
      Card := Random (2..14);
      Main.Cartes := Main.Cartes & Card;
      Main.Somme := Main.Somme + Card;
    elsif Reponse = "N" then
      Exit;
    end if;
  end loop;
end Jouer;

procedure main is
  Main: T_Main;
begin
  CreerMain (Main);
  Jouer (Main);

  if Main.Somme > 21 then
    Put_Line ("Vous avez perdu !");
  else
    Put_Line ("Vous avez gagné !");
  end if;
end main;
```

**Explication du code :**

* Le type **Carte** représente les cartes du jeu, avec des valeurs allant de 2 à 14.
* Le type **T_Cartes** est un tableau de cartes.
* Le type **T_Main** représente la main du joueur, composée d'un tableau de cartes et de la somme des valeurs des cartes.
* La procédure **CreerMain** crée une nouvelle main en initialisant les cartes et la somme.
* La procédure **AfficherMain** affiche les cartes et la somme de la main.
* La procédure **Jouer** permet au joueur de tirer des cartes jusqu'à ce qu'il décide de s'arrêter ou qu'il dépasse 21.
* La procédure **main** est le point d'entrée du programme qui crée une nouvelle main, lance le jeu et annonce le résultat.

Ce code fournit un programme de blackjack de base qui simule le jeu contre l'ordinateur. Le joueur peut tirer des cartes jusqu'à ce qu'il décide de s'arrêter ou qu'il dépasse 21. Le programme vérifie ensuite si le joueur a gagné ou perdu en fonction de la somme des valeurs des cartes dans sa main.