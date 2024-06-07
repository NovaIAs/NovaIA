**Code complexe en Shell**

```shell
#!/bin/bash

# Initialisation des variables
fichier_donnees="donnees.csv"
separateur=","
champs_obligatoires=(nom prenom age ville)

# Fonctions utilitaires

function verifier_champ_obligatoire() {
  local champ=$1
  if [[ ! " ${champs_obligatoires[*]} " =~ " $champ " ]]; then
    echo "Erreur : le champ '$champ' est obligatoire." >&2
    return 1
  fi
  return 0
}

function lire_ligne() {
  local ligne=$1
  local champs
  IFS=$separateur
  read -ra champs <<< "$ligne"
  for champ in "${champs[@]}"; do
    champ=$(echo $champ | sed -e 's/[[:space:]]*//' -e 's/"//g')
    if ! verifier_champ_obligatoire "$champ"; then
      return 1
    fi
  done
  echo "${champs[@]}"
  return 0
}

# Traitement du fichier

echo "Traitement du fichier '$fichier_donnees'..."

# Vérification de l'existence du fichier
if [ ! -f "$fichier_donnees" ]; then
  echo "Erreur : le fichier '$fichier_donnees' est introuvable." >&2
  exit 1
fi

# Lecture du fichier ligne par ligne
while read ligne; do
  if ! lire_ligne "$ligne"; then
    echo "Erreur : la ligne '$ligne' contient des champs invalides." >&2
  else
    # Traitement de la ligne valide
    # ...
  fi
done < "$fichier_donnees"

echo "Traitement terminé."
```

**Explications du code**

Ce code Shell effectue les opérations suivantes :

* **Initialisation des variables** : Définit les variables utilisées dans le script.
* **Fonctions utilitaires** :
    * `verifier_champ_obligatoire` : Vérifie si un champ est obligatoire.
    * `lire_ligne` : Lit une ligne d'un fichier et vérifie si elle contient les champs obligatoires.
* **Traitement du fichier** :
    * Vérifie l'existence du fichier de données.
    * Lit le fichier ligne par ligne et appelle la fonction `lire_ligne` pour valider chaque ligne.
    * Traite les lignes valides.