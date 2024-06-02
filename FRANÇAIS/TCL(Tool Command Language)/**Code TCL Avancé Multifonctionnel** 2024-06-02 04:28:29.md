**Code TCL complexe**

Ce code TCL complexe effectue les opérations suivantes :

* Crée un widget de fenêtre avec plusieurs éléments de contrôle.
* Initialise les valeurs des éléments de contrôle et définit les gestionnaires d'événements.
* Définit des fonctions pour traiter les interactions utilisateur et effectuer des calculs.
* Utilise la bibliothèque Tclx pour créer une barre d'outils personnalisée.
* Utilise la bibliothèque Tkcon pour créer des fenêtres modales pour les messages d'erreur et de confirmation.

```tcl
# Importation des bibliothèques nécessaires
package require Tk
package require Tclx
package require Tkcon

# Création de la fenêtre principale
set w .app
wm title $w "Application complexe"
wm geometry $w 600x400

# Création des éléments de contrôle
label .app.label -text "Nom :"
entry .app.entry
button .app.ok -text "OK" -command { ok }
button .app.cancel -text "Annuler" -command { closeApp }

# Gestionnaires d'événements
bind .app.entry <Return> { ok }

# Fonctions de traitement des interactions utilisateur
proc ok {} {
    set name [get .app.entry]
    if {[string trim $name] == ""} {
        errorMsg "Veuillez entrer un nom."
    } else {
        calcValues $name
    }
}

proc closeApp {} {
    set result [confirmMsg "Voulez-vous vraiment quitter ?"]
    if {$result eq "yes"} {
        wm withdraw .app
    }
}

# Fonction de calcul des valeurs
proc calcValues {name} {
    # Code de calcul
    set average 85
    set grade [expr {$average >= 90 ? "A" : $average >= 80 ? "B" : $average >= 70 ? "C" : $average >= 60 ? "D" : "E"}]
    # Affichage des résultats
    label .app.results -text "Nom : $name\nMoyenne : $average\nNote : $grade"
    .app.results pack
}

# Création d'une barre d'outils personnalisée
set tb .app.toolbar
TclxTreeBar $tb -orient horizontal
TclxTreeBarButton $tb -text "Nouveau"  -image imageCreatePhoto -compound imageleft -command { newFile }
TclxTreeBarSeparator $tb
TclxTreeBarButton $tb -text "Ouvrir"  -image imageCreatePhoto -compound imageleft -command { openFile }
TclxTreeBarButton $tb -text "Enregistrer" -image imageCreatePhoto -compound imageleft -command { saveFile }
TclxTreeBarButton $tb -text "Quitter" -image imageCreatePhoto -compound imageleft -command { closeApp }


# Fonctions de confirmation et d'erreur
proc confirmMsg {msg} {
    Tkcon.dialog --title "Confirmation" --text $msg --buttons yes no
}

proc errorMsg {msg} {
    Tkcon.error --title "Erreur" --text $msg
}

```

**Explication :**

* La fenêtre principale `.app` est créée avec un titre et une taille.
* Les éléments de contrôle (étiquette, zone de saisie, boutons) sont créés et placés dans la fenêtre.
* Les gestionnaires d'événements sont définis pour les interactions utilisateur, notamment l'appui sur la touche Entrée et les clics sur les boutons.
* Les fonctions `ok` et `closeApp` traitent les interactions utilisateur et effectuent les tâches appropriées.
* La fonction `calcValues` calcule et affiche les valeurs basées sur le nom de l'utilisateur.
* La barre d'outils personnalisée est créée à l'aide de la bibliothèque Tclx.
* Les fonctions `confirmMsg` et `errorMsg` affichent des fenêtres modales pour la confirmation et les messages d'erreur à l'aide de la bibliothèque Tkcon.