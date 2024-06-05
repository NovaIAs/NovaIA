**Code**

```shell
#!/bin/bash

# Définition des variables
fichier_donnees="donnees.txt"
fichier_resultats="resultats.txt"
regex="^[0-9]+$"
somme=0

# Vérification de l'existence du fichier de données
if [ ! -f "$fichier_donnees" ]; then
  echo "Le fichier de données n'existe pas."
  exit 1
fi

# Lecture du fichier de données
while read ligne; do
  # Vérification de la validité des données
  if [[ $ligne =~ $regex ]]; then
    # Addition des valeurs valides à la somme
    somme=$(($somme + $ligne))
  else
    echo "La ligne '$ligne' contient des données invalides."
  fi
done < "$fichier_donnees"

# Calcul de la moyenne
moyenne=$(($somme / $(wc -l < "$fichier_donnees")))

# Écriture des résultats dans le fichier de résultats
echo "Somme : $somme" > "$fichier_resultats"
echo "Nombre de lignes : $(wc -l < "$fichier_donnees")" >> "$fichier_resultats"
echo "Moyenne : $moyenne" >> "$fichier_resultats"

# Affichage des résultats
echo "Résultats enregistrés dans le fichier '$fichier_resultats'."
```

**Explications**

**1. Définition des variables:**
   - `fichier_donnees`: Le nom du fichier contenant les données.
   - `fichier_resultats`: Le nom du fichier dans lequel les résultats seront enregistrés.
   - `regex`: Une expression régulière pour valider les données numériques.
   - `somme`: La somme des valeurs valides.

**2. Vérification de l'existence du fichier de données:**
   - La commande `if` vérifie si le fichier de données existe.
   - Si le fichier n'existe pas, un message d'erreur est affiché et le script s'arrête.

**3. Lecture du fichier de données:**
   - La commande `while` lit chaque ligne du fichier de données.
   - La commande `[[` vérifie si la ligne correspond à l'expression régulière définie.
   - Si la ligne est valide, elle est ajoutée à la somme.
   - Si la ligne est invalide, un message d'erreur est affiché.

**4. Calcul de la moyenne:**
   - La commande `wc -l` compte le nombre de lignes dans le fichier de données.
   - La moyenne est calculée en divisant la somme par le nombre de lignes.

**5. Écriture des résultats:**
   - La commande `echo` écrit les résultats dans le fichier de résultats.

**6. Affichage des résultats:**
   - Un message indiquant que les résultats ont été enregistrés dans le fichier de résultats est affiché.