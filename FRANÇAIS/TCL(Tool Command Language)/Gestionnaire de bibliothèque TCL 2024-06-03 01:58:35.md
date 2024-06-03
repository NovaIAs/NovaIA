**Programme de Gestion de Bibliothèque en TCL**

```tcl
set biblio [lsearch -glob "*biblio*" [info vars]]  ;# Récupérer la variable "biblio" si elle existe

if {$biblio != -1} {
    puts "La variable 'biblio' existe déjà."
} else {
    set biblio {}               ;# Créer une liste vide pour la base de données bibliographique
    puts "Création de la base de données bibliographique..."
}

while {1} {
    puts "\nMenu :\n1. Ajouter un livre\n2. Afficher tous les livres\n3. Rechercher un livre\n4. Supprimer un livre\n5. Quitter"
    set choix [gets stdin]    ;# Lire le choix de l'utilisateur

    switch -exact $choix {
        1 {
            puts "Entrez le titre du livre :"
            set titre [gets stdin]

            puts "Entrez l'auteur du livre :"
            set auteur [gets stdin]

            puts "Entrez l'ISBN du livre :"
            set isbn [gets stdin]

            lset biblio $isbn [list $titre $auteur]  ;# Ajouter le livre à la base de données bibliographique
            puts "Livre ajouté avec succès."
        }
        2 {
            if {[llength $biblio] == 0} {
                puts "La base de données bibliographique est vide."
            } else {
                foreach isbn $biblio {
                    list titre auteur [lrange $biblio $isbn]
                    puts "\nTitre : $titre"
                    puts "Auteur : $auteur"
                    puts "ISBN : $isbn"
                }
            }
        }
        3 {
            puts "Entrez le titre ou l'ISBN du livre à rechercher :"
            set recherche [gets stdin]

            foreach isbn $biblio {
                list titre auteur [lrange $biblio $isbn]
                if {($recherche eq $titre) || ($recherche eq $isbn)} {
                    puts "\nTitre : $titre"
                    puts "Auteur : $auteur"
                    puts "ISBN : $isbn"
                    break
                }
            }

            if {[lsearch -exact $biblio $recherche] == -1} {
                puts "Aucun livre correspondant trouvé."
            }
        }
        4 {
            puts "Entrez l'ISBN du livre à supprimer :"
            set isbn [gets stdin]

            if {[lsearch -exact $biblio $isbn] != -1} {
                unset biblio($isbn)
                puts "Livre supprimé avec succès."
            } else {
                puts "Aucun livre avec cet ISBN n'existe."
            }
        }
        5 {
            puts "Au revoir !"
            exit
        }
        default {
            puts "Choix invalide. Veuillez entrer un nombre entre 1 et 5."
        }
    }
}
```

**Explication du code :**

Ce code est un programme de gestion de bibliothèque en Tcl qui permet d'ajouter, d'afficher, de rechercher et de supprimer des livres dans une base de données bibliographique. Le programme utilise une liste associative pour stocker les informations sur les livres, où l'ISBN est la clé et la valeur est une liste contenant le titre et l'auteur du livre.

Le programme utilise un menu interactif pour permettre aux utilisateurs d'interagir avec la base de données bibliographique. Les utilisateurs peuvent choisir d'ajouter un nouveau livre, d'afficher tous les livres, de rechercher un livre par titre ou ISBN, de supprimer un livre par ISBN ou de quitter le programme.

Le code utilise les fonctions `lsearch`, `lset` et `llength` pour manipuler la base de données bibliographique et les fonctions `puts` et `gets` pour l'interaction avec l'utilisateur. Le code utilise également la commande `switch` pour gérer les différents choix du menu.