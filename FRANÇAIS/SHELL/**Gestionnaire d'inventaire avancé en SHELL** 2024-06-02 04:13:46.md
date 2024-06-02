**Gestionnaire d'inventaire complexe en SHELL**

Ce code complet fournit un système de gestion d'inventaire puissant et polyvalent, permettant de suivre les articles, les emplacements et les transactions.

```shell
#!/bin/bash

# Définition des variables globales
INVENTORY_FILE="inventaire.dat"
LOCATIONS_FILE="emplacements.dat"
TRANSACTIONS_FILE="transactions.dat"
SEPARATEUR=";"

# Fonctions d'assistance

# Lire un fichier dans une variable
function read_file() {
  local file=$1
  local variable_name=$2
  eval "$variable_name=\`cat $file\`"
}

# Écrire une variable dans un fichier
function write_file() {
  local variable_name=$1
  local file=$2
  local content=`eval "echo \$$variable_name"`
  echo "$content" > $file
}

# Ajouter un nouvel article à l'inventaire
function add_article() {
  local id=$1
  local name=$2
  local quantity=$3
  local location=$4

  # Vérifier si l'article existe déjà
  if grep -q "^$id;$name;$quantity;$location$" $INVENTORY_FILE; then
    echo "L'article $name existe déjà."
    return 1
  fi

  # Ajouter l'article à l'inventaire
  echo "$id;$name;$quantity;$location" >> $INVENTORY_FILE
}

# Modifier un article dans l'inventaire
function modify_article() {
  local id=$1
  local name=$2
  local quantity=$3
  local location=$4

  # Vérifier si l'article existe
  if ! grep -q "^$id;$name;$quantity;$location$" $INVENTORY_FILE; then
    echo "L'article $name n'existe pas."
    return 1
  fi

  # Modifier l'article
  sed -i "s/^$id;$name;$quantity;$location$/$id;$name;$quantity;$location/" $INVENTORY_FILE
}

# Supprimer un article de l'inventaire
function delete_article() {
  local id=$1

  # Vérifier si l'article existe
  if ! grep -q "^$id;" $INVENTORY_FILE; then
    echo "L'article $id n'existe pas."
    return 1
  fi

  # Supprimer l'article
  sed -i "/^$id;/d" $INVENTORY_FILE
}

# Ajouter un nouvel emplacement à la liste
function add_location() {
  local id=$1
  local name=$2

  # Vérifier si l'emplacement existe déjà
  if grep -q "^$id;$name$" $LOCATIONS_FILE; then
    echo "L'emplacement $name existe déjà."
    return 1
  fi

  # Ajouter l'emplacement à la liste
  echo "$id;$name" >> $LOCATIONS_FILE
}

# Modifier un emplacement dans la liste
function modify_location() {
  local id=$1
  local name=$2

  # Vérifier si l'emplacement existe
  if ! grep -q "^$id;$name$" $LOCATIONS_FILE; then
    echo "L'emplacement $name n'existe pas."
    return 1
  fi

  # Modifier l'emplacement
  sed -i "s/^$id;$name$/$id;$name/" $LOCATIONS_FILE
}

# Supprimer un emplacement de la liste
function delete_location() {
  local id=$1

  # Vérifier si l'emplacement existe
  if ! grep -q "^$id;" $LOCATIONS_FILE; then
    echo "L'emplacement $id n'existe pas."
    return 1
  fi

  # Supprimer l'emplacement
  sed -i "/^$id;/d" $LOCATIONS_FILE
}

# Ajouter une nouvelle transaction
function add_transaction() {
  local id=$1
  local article_id=$2
  local location_id=$3
  local quantity=$4
  local date=$5

  # Vérifier si l'article et l'emplacement existent
  if ! grep -q "^$article_id;" $INVENTORY_FILE || ! grep -q "^$location_id;" $LOCATIONS_FILE; then
    echo "L'article ou l'emplacement indiqué n'existe pas."
    return 1
  fi

  # Ajouter la transaction
  echo "$id;$article_id;$location_id;$quantity;$date" >> $TRANSACTIONS_FILE
}

# Modifier une transaction
function modify_transaction() {
  local id=$1
  local article_id=$2
  local location_id=$3
  local quantity=$4
  local date=$5

  # Vérifier si l'article, l'emplacement et la transaction existent
  if ! grep -q "^$article_id;" $INVENTORY_FILE || ! grep -q "^$location_id;" $LOCATIONS_FILE || ! grep -q "^$id;" $TRANSACTIONS_FILE; then
    echo "L'article, l'emplacement ou la transaction indiqué n'existe pas."
    return 1
  fi

  # Modifier la transaction
  sed -i "s/^$id;$article_id;$location_id;$quantity;$date$/$id;$article_id;$location_id;$quantity;$date/" $TRANSACTIONS_FILE
}

# Supprimer une transaction
function delete_transaction() {
  local id=$1

  # Vérifier si la transaction existe
  if ! grep -q "^$id;" $TRANSACTIONS_FILE; then
    echo "La transaction $id n'existe pas."
    return 1
  fi

  # Supprimer la transaction
  sed -i "/^$id;/d" $TRANSACTIONS_FILE
}

# Affichage du menu principal
function display_menu() {
  clear

  echo "**** Gestionnaire d'inventaire ****"
  echo "1. Gérer les articles"
  echo "2. Gérer les emplacements"
  echo "3. Gérer les transactions"
  echo "4. Rapports"
  echo "5. Quitter"
  echo "Choisissez une option : "
}

# Boucle principale du programme
while true; do
  display_menu

  read option

  case $option in
    1)
      # Menu de gestion des articles
      while true; do
        clear

        echo "Gestion des articles"
        echo "1. Ajouter un article"
        echo "2. Modifier un article"
        echo "3. Supprimer un article"
        echo "4. Retour"
        echo "Choisissez une option : "

        read article_option

        case $article_option in
          1)
            # Ajouter un article
            read_file $INVENTORY_FILE inventory_data
            echo "Entrez l'ID de l'article : "
            read id
            echo "Entrez le nom de l'article : "
            read name
            echo "Entrez la quantité : "
            read quantity
            echo "Entrez l'emplacement (ID) : "
            read location

            add_article $id $name $quantity $location
            ;;
          2)
            # Modifier un article
            read_file $INVENTORY_FILE inventory_data

            echo "Entrez l'ID de l'article à modifier : "
            read id

            if grep -q "^$id;" $inventory_data; then
              echo "Entrez le nouveau nom : "
              read name
              echo "Entrez la nouvelle quantité : "
              read quantity
              echo "Entrez le nouvel emplacement (ID) : "
              read location

              modify_article $id $name $quantity $location
            else
              echo "L'article $id n'existe pas."
            fi
            ;;
          3)
            # Supprimer un article
            read_file $INVENTORY_FILE inventory_data

            echo "Entrez l'ID de l'article à supprimer : "
            read id

            if grep -q "^$id;" $inventory_data; then
              delete_article $id
            else
              echo "L'article $id n'existe pas."
            fi
            ;;
          4)
            # Retour au menu principal
            break
            ;;
          *)
            echo "Option invalide."
            ;;
        esac
      done
      ;;
    2)
      # Menu de gestion des emplacements
      while true; do
        clear

        echo "Gestion des emplacements"
        echo "1. Ajouter un emplacement"
        echo "2. Modifier un emplacement"
        echo "3. Supprimer un emplacement"
        echo "4. Retour"
        echo "Choisissez une option : "

        read location_option

        case $location_option in
          1)
            # Ajouter un emplacement
            read_file $LOCATIONS_FILE locations_data
            echo "Entrez l'ID de l'emplacement : "
            