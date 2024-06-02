**Algorithme de recherche de Huffman (version en français)**

**Objectif :** Compresser efficacement des données en créant un arbre de Huffman, où chaque symbole est représenté par un code binaire de longueur minimale.

**Entrée :**
* Liste de caractères `caracteres` avec leurs fréquences `frequences`

**Sortie :**
* Arbre de Huffman sous forme de dictionnaire `arbre_huffman`

**Code :**

```python
from collections import Counter, deque
from heapq import heappush, heappop

def construire_arbre_huffman(caracteres, frequences):
    """Construit un arbre de Huffman à partir des caractères et de leurs fréquences."""

    # Créé une file de priorité avec des tuples (symbole, fréquence)
    file_priorite = [(freq, [caractere]) for caractere, freq in zip(caracteres, frequences)]
    heapq.heapify(file_priorite)

    while len(file_priorite) > 1:
        # Extrait les deux symboles les plus fréquents
        freq1, code1 = heappop(file_priorite)
        freq2, code2 = heappop(file_priorite)

        # Créé un nouveau noeud avec les deux symboles pour enfants
        noeud_parent = {'0': code1, '1': code2}
        freq_parent = freq1 + freq2

        # Insère le noeud parent dans la file de priorité
        heappush(file_priorite, (freq_parent, noeud_parent))

    # L'arbre de Huffman est le noeud racine de la file de priorité
    arbre_huffman = heappop(file_priorite)[1]
    return arbre_huffman

def coder_huffman(caracteres, arbre_huffman):
    """Code les caractères en utilisant l'arbre de Huffman."""

    # Crée un dictionnaire de codes
    codes_huffman = {}
    for caractere in caracteres:
        code = ""
        noeud = arbre_huffman
        while isinstance(noeud, dict):
            if caractere in noeud['0']:
                code += '0'
                noeud = noeud['0']
            elif caractere in noeud['1']:
                code += '1'
                noeud = noeud['1']
            else:
                raise ValueError(f"Impossible de trouver le code pour {caractere}")
        codes_huffman[caractere] = code

    return codes_huffman

def decoder_huffman(bits, arbre_huffman):
    """Décode une séquence de bits en utilisant l'arbre de Huffman."""

    # Initialise le noeud actuel
    noeud = arbre_huffman

    # Parcourt la séquence de bits
    message_decode = ""
    for bit in bits:
        if bit == '0':
            noeud = noeud['0']
        elif bit == '1':
            noeud = noeud['1']
        else:
            raise ValueError("Bit invalide")

        # Si le noeud est une feuille, décode le caractère et réinitialise le noeud
        if not isinstance(noeud, dict):
            message_decode += noeud
            noeud = arbre_huffman

    return message_decode

# Exemple d'utilisation
caracteres = ['a', 'b', 'c', 'd']
frequences = [4, 2, 2, 1]

# Construit l'arbre de Huffman
arbre_huffman = construire_arbre_huffman(caracteres, frequences)

# Code les caractères
codes_huffman = coder_huffman(caracteres, arbre_huffman)

# Décode une séquence de bits
bits = '010011'
message_decode = decoder_huffman(bits, arbre_huffman)

print(f"Codes de Huffman : {codes_huffman}")
print(f"Message décodé : '{message_decode}'")
```

**Explication :**

* La fonction `construire_arbre_huffman` construit l'arbre de Huffman en utilisant une file de priorité (un tas).
* La fonction `coder_huffman` utilise l'arbre de Huffman pour coder les caractères sous forme de codes binaires.
* La fonction `decoder_huffman` décode une séquence de bits en utilisant l'arbre de Huffman pour obtenir le message d'origine.
* La partie principale de l'algorithme est la construction de l'arbre de Huffman, qui est un processus itératif.
* À chaque itération, les deux symboles les plus fréquents sont combinés dans un noeud parent.
* Le processus se répète jusqu'à ce qu'il ne reste plus qu'un seul noeud parent, qui est la racine de l'arbre de Huffman.
* Les codes de Huffman sont construits en parcourant l'arbre de la racine aux feuilles, en ajoutant '0' pour le chemin de gauche et '1' pour le chemin de droite.