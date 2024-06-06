```shell
#!/bin/bash

# Déclaration des variables
nom="John Doe"
age=35
ville="Paris"
pays="France"

# Boucle for pour parcourir un tableau
pays=("France" "Espagne" "Italie")
for pays in "${pays[@]}"; do
  echo "Pays : $pays"
done

# Boucle while pour lire l'entrée de l'utilisateur
while read ligne; do
  echo "Ligne lue : $ligne"
done

# Fonction pour calculer la factorielle d'un nombre
calcul_factorielle() {
  local nombre=$1
  local resultat=1
  while [ $nombre -gt 0 ]; do
    resultat=$((resultat * nombre))
    nombre=$((nombre - 1))
  done
  echo $resultat
}

# Appel de la fonction pour calculer la factorielle de 5
factorielle=$(calcul_factorielle 5)
echo "Factorielle de 5 : $factorielle"

# Script pour vérifier si un nombre est premier
is_premier() {
  local nombre=$1
  if [ $nombre -le 1 ]; then
    return 1  # Faux
  fi

  for ((i=2; i <= $(($nombre / 2)); i++)); do
    if [ $((nombre % i)) -eq 0 ]; then
      return 1  # Faux
    fi
  done

  return 0  # Vrai
}

# Test du script pour vérifier si 13 est premier
if is_premier 13; then
  echo "13 est un nombre premier."
else
  echo "13 n'est pas un nombre premier."
fi
```

**Explication du code**

Ce code contient un ensemble de fonctionnalités avancées en shell :

* **Variables et tableaux** : Le code déclare des variables pour stocker des informations telles que le nom, l'âge, la ville et le pays. Il crée également un tableau de pays pour parcourir ses éléments.
* **Boucles** : Le code utilise la boucle `for` pour itérer sur un tableau et la boucle `while` pour lire l'entrée de l'utilisateur.
* **Fonctions** : Le code définit une fonction `calcul_factorielle` pour calculer la factorielle d'un nombre.
* **Script** : Le code contient un script `is_premier` qui vérifie si un nombre donné est premier.
* **Conditions** : Le code utilise des conditions `if` et `while` pour contrôler le flux du programme.
* **Retour de valeur** : La fonction `is_premier` utilise `return` pour renvoyer un code de sortie indiquant si le nombre est premier ou non.

Ce code démontre la capacité de shell à effectuer des tâches complexes, notamment la manipulation des données, les boucles, les fonctions et les contrôles de flux. Il peut être utilisé comme base pour des scripts shell plus avancés.