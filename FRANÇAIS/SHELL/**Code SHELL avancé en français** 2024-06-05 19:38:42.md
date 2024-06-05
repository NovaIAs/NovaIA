**Code SHELL complexe en français**

```shell
#!/bin/bash

# VARIABLES
## Déclaration des variables
NOM="Jean"
PRENOM="Dupont"
AGE=30
VILLE="Paris"
## Affectation de valeurs
NOM="Paul"

# CONDITIONS
## Tests conditionnels
if [ "$NOM" = "Paul" ]; then
  echo "Bonjour Paul."
elif [ "$NOM" = "Jean" ]; then
  echo "Bienvenue Jean."
else
  echo "Inconnu."
fi

## Conditions imbriquées
if [ "$AGE" -gt 25 ] && [ "$VILLE" = "Paris" ]; then
  echo "Vous êtes un Parisien de plus de 25 ans."
else
  echo "Vous n'êtes pas un Parisien de plus de 25 ans."
fi

## Opérateurs de comparaison
# = Égal à
# != Différent de
# -lt Inférieur à
# -le Inférieur ou égal à
# -gt Supérieur à
# -ge Supérieur ou égal à

# BOUCLES
## Boucle for
for (( i=0; i<10; i++ )); do
  echo "$i"
done

## Boucle while
while [ "$AGE" -lt 40 ]; do
  AGE=$((AGE + 1))
  echo "Vous avez $AGE ans."
done

## Boucle until
until [ "$VILLE" = "Nantes" ]; do
  VILLE="Lyon"
  echo "Vous êtes à $VILLE."
done

## Boucle case
case "$NOM" in
  "Paul")
    echo "Vous êtes Paul."
    ;;
  "Jean")
    echo "Vous êtes Jean."
    ;;
  *)
    echo "Vous n'êtes ni Paul ni Jean."
    ;;
esac

# FONCTIONS
## Déclaration d'une fonction
ma_fonction() {
  echo "Ceci est une fonction."
}

## Appel d'une fonction
ma_fonction

# TABLEAUX
## Création d'un tableau
tableau=(1 2 3 4 5)

## Accès à un élément du tableau
echo "${tableau[0]}"

## Parcours d'un tableau
for element in "${tableau[@]}"; do
  echo "$element"
done

# FICHIERS
## Création d'un fichier
touch fichier.txt

## Écriture dans un fichier
echo "Bonjour le monde !" > fichier.txt

## Lecture d'un fichier
cat fichier.txt

## Suppression d'un fichier
rm fichier.txt

# COMMANDES AVANCÉES
## Redirection d'entrée/sortie
ls -l | grep "nom-du-fichier"

## Pipe
ls -l | awk '{print $9}'

## Substitution de commandes
echo $(date)

## Variables d'environnement
echo "$HOME"
```

**Explication du code**

Ce code illustre diverses fonctionnalités avancées du shell, notamment :

* **Variables :** Déclaration, affectation et utilisation des variables pour stocker des informations.
* **Conditions :** Tests conditionnels pour prendre des décisions basées sur des conditions.
* **Boucles :** Boucles for, while, until et case pour répéter des actions.
* **Fonctions :** Déclaration et appel de fonctions pour réutiliser du code.
* **Tableaux :** Création, accès et parcours de tableaux pour stocker des collections d'éléments.
* **Fichiers :** Création, écriture, lecture et suppression de fichiers.
* **Commandes avancées :** Redirection d'entrée/sortie, pipe, substitution de commandes et variables d'environnement pour une manipulation avancée des données et des commandes.