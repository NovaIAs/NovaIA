```ada
-- Déclaration du package
package Interface_Graphique is

    -- Procédure pour créer une fenêtre
    procedure Create_Window (Nom_Fenêtre : String; Largeur, Hauteur : Integer);

    -- Procédure pour afficher une chaîne dans une fenêtre
    procedure Display_String (Fenêtre : Window; Chaîne : String);

    -- Procédure pour récupérer les événements utilisateur
    function Get_Event (Fenêtre : Window) return Event;

    -- Constantes pour les événements
    Event_Button_Click : constant Event := 1;
    Event_Mouse_Move : constant Event := 2;
    Event_Key_Press : constant Event := 3;

end Interface_Graphique;

with Ada.Text_IO;

procedure main is

    -- Déclaration des variables
    Nom_Fenêtre : constant String := "Ma fenêtre";
    Largeur : constant Integer := 800;
    Hauteur : constant Integer := 600;
    Fenetre : Window;
    E : Event;

begin

    -- Création de la fenêtre
    Interface_Graphique.Create_Window (Nom_Fenêtre, Largeur, Hauteur);

    -- Affichage d'une chaîne dans la fenêtre
    Interface_Graphique.Display_String (Fenetre, "Bonjour, monde !");

    -- Boucle principale
    loop
        -- Récupération de l'événement utilisateur
        E := Interface_Graphique.Get_Event (Fenetre);

        -- Traitement de l'événement
        case E is
            when Event_Button_Click =>
                -- Traitement du clic de bouton
                Ada.Text_IO.Put_Line ("Clic de bouton !");
            when Event_Mouse_Move =>
                -- Traitement du déplacement de la souris
                Ada.Text_IO.Put_Line ("Déplacement de la souris !");
            when Event_Key_Press =>
                -- Traitement de la pression d'une touche
                Ada.Text_IO.Put_Line ("Pression d'une touche !");
            when others =>
                -- Traitement de tout autre événement
                Ada.Text_IO.Put_Line ("Autre événement !");
        end case;
    end loop;

end main;
```

**Explication du code :**

Ce code ADA crée une interface graphique simple avec une fenêtre qui affiche le texte "Bonjour, monde !" et attend les événements utilisateur. Lorsqu'un événement se produit (comme un clic de bouton ou un déplacement de la souris), le programme l'affiche dans la console.

**Voici une explication détaillée du code :**

* **Déclaration du package Interface_Graphique :** Ce package définit les procédures et fonctions utilisées pour manipuler l'interface graphique.
* **Procédure Create_Window :** Crée une fenêtre avec le nom, la largeur et la hauteur spécifiés.
* **Procédure Display_String :** Affiche une chaîne dans la fenêtre spécifiée.
* **Fonction Get_Event :** Récupère l'événement utilisateur le plus récent dans la fenêtre spécifiée.
* **Constantes pour les événements :** Définissent les différents types d'événements qui peuvent se produire.
* **Procédure main :** Le point d'entrée du programme.
* **Déclaration des variables :** Déclare les variables utilisées dans le programme.
* **Création de la fenêtre :** Appelle la procédure Create_Window pour créer la fenêtre.
* **Affichage d'une chaîne dans la fenêtre :** Appelle la procédure Display_String pour afficher la chaîne "Bonjour, monde !" dans la fenêtre.
* **Boucle principale :** Une boucle infinie qui attend les événements utilisateur.
* **Récupération de l'événement utilisateur :** Appelle la fonction Get_Event pour récupérer l'événement utilisateur le plus récent.
* **Traitement de l'événement :** Utilise une instruction case pour traiter différents types d'événements et afficher le message approprié dans la console.