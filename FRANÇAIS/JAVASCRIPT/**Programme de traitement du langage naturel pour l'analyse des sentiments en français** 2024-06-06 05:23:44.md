**Programme de traitement du langage naturel pour l'analyse de sentiments en français**

**Objectif:** Déterminer le sentiment général d'un texte en français en analysant la fréquence des mots positifs et négatifs.

**Code:**

```javascript
// Importer les bibliothèques nécessaires
import { readFileSync } from 'fs';
import stopwords from 'stopwords-fr';

// Lire le texte d'entrée
const texte = readFileSync('texte.txt', 'utf8');

// Nettoyer le texte
const texteNettoyé = texte
  .toLowerCase() // Convertir le texte en minuscules
  .replace(/[^a-zA-ZàâçéèêëîïôûüÿñÀÂÇÉÈÊËÎÏÔÛÜŸÑ ]/g, '') // Supprimer les caractères non alphabétiques
  .split(' ') // Diviser le texte en mots
  .filter(mot => !stopwords.includes(mot)); // Supprimer les mots vides

// Créer des dictionnaires de mots positifs et négatifs
const motsPositifs = ['heureux', 'content', 'génial', 'super', 'merveilleux'];
const motsNegatifs = ['triste', 'mécontent', 'horrible', 'terrible', 'piteux'];

// Initialiser les compteurs de sentiments
let sentimentPositif = 0;
let sentimentNegatif = 0;

// Analyser la fréquence des mots
texteNettoyé.forEach(mot => {
  if (motsPositifs.includes(mot)) {
    sentimentPositif++;
  } else if (motsNegatifs.includes(mot)) {
    sentimentNegatif++;
  }
});

// Calculer le sentiment général
const scoreSentiment = sentimentPositif - sentimentNegatif;

// Afficher le résultat
console.log(`Sentiment général : ${scoreSentiment < 0 ? 'Négatif' : 'Positif'}`);
```

**Explication du code:**

1. **Import des bibliothèques:** `fs` permet de lire le fichier texte, et `stopwords-fr` fournit une liste de mots vides en français.

2. **Lecture du texte d'entrée:** Le texte est lu depuis le fichier `texte.txt` et converti en minuscules.

3. **Nettoyage du texte:** Les caractères non alphabétiques sont supprimés, le texte est divisé en mots et les mots vides sont supprimés.

4. **Création des dictionnaires de mots:** Deux dictionnaires sont créés, contenant respectivement les mots positifs et négatifs.

5. **Initialisation des compteurs de sentiments:** Deux variables sont initialisées à 0 pour compter les sentiments positifs et négatifs.

6. **Analyse de fréquence des mots:** Chaque mot nettoyé est analysé pour déterminer s'il est positif ou négatif. Les compteurs correspondants sont incrémentés.

7. **Calcul du sentiment général:** Le score du sentiment est obtenu en soustrayant le nombre de mots négatifs au nombre de mots positifs.

8. **Affichage du résultat:** Le sentiment général (positif ou négatif) est affiché sur la console.