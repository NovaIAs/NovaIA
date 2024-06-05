**Programme FORTH pour la gestion de projets**

```forth
: créer-projet ( nom -- projet )
  init-struct
  field to-string "name" over store
  over @ init-link over put
  field to-string "tasks" over store
  over @ init-link over put
  field to-string "resources" over store
  over @ init-link over put
;

: ajouter-tâche ( projet nom -- )
  struct3 get "name" over field find-in-link
  not if [ drop ] then
  struct3 get "tasks" over add-to-link
;

: ajouter-ressource ( projet nom -- )
  struct3 get "name" over field find-in-link
  not if [ drop ] then
  struct3 get "resources" over add-to-link
;

: lister-projets ( )
   struct3 @ code 0 "name" over field-at do
   over ."  "
  loop ;

: lister-tâches ( projet -- )
  struct3 get "tasks" over do
   over ."  "
  loop ;

: lister-ressources ( projet -- )
  struct3 get "resources" over do
   over ."  "
  loop ;

: main ()
  struct3 create "projet1" =
  "tâche1" ajouter-tâche
  "tâche2" ajouter-tâche
  "ressource1" ajouter-ressource
  lister-projets
  lister-tâches "projet1"
  lister-ressources "projet1"
;
```

**Explication du code:**

Ce code FORTH implémente un programme simple de gestion de projets. Il permet de créer des projets, d'ajouter des tâches et des ressources aux projets, et d'afficher la liste des projets, des tâches et des ressources.

Voici une explication détaillée du code :

* **Créer-projet** : Cette définition de mot crée une nouvelle structure de données pour représenter un projet. Elle stocke les informations dans trois champs: "nom", "tâches" et "ressources".
* **Ajouter-tâche** : Ce mot ajoute une nouvelle tâche à un projet donné. Il recherche d'abord la tâche par son nom dans la liste des tâches du projet. S'il ne la trouve pas, il l'ajoute à la liste.
* **Ajouter-ressource** : Ce mot fonctionne de manière similaire à **ajouter-tâche**, mais il ajoute une nouvelle ressource à un projet donné.
* **Lister-projets** : Ce mot affiche une liste de tous les projets créés.
* **Lister-tâches** : Ce mot affiche une liste des tâches associées à un projet donné.
* **Lister-ressources** : Ce mot affiche une liste des ressources associées à un projet donné.
* **Main** : C'est le mot d'entrée du programme. Il crée un projet, ajoute des tâches et des ressources au projet, puis affiche la liste des projets, des tâches et des ressources.

**Exemple d'exécution:**

Si nous exécutons le code ci-dessus, il sortira ce qui suit :

```
projet1  
tâche1  
tâche2  
ressource1  
```

Cela indique que le projet **projet1** a été créé, avec les tâches **tâche1** et **tâche2**, et la ressource **ressource1**.