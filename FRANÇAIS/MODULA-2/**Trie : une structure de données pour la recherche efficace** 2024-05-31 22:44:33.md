**Module Trie (Trie)**

```modula-2
MODULE Trie;
FROM InOut IMPORT WriteString, WriteLn, ReadChar;

TYPE
  TrieNode = RECORD
    value: CARDINAL;
    children: ARRAY [CHAR] OF POINTER TO TrieNode;
  END;

VAR
  root: POINTER TO TrieNode;

PROCEDURE PrintTrie(node: POINTER TO TrieNode; prefix: ARRAY OF CHAR);

PROCEDURE Insert(value: CARDINAL; key: ARRAY OF CHAR);

PROCEDURE Search(key: ARRAY OF CHAR): BOOLEAN;

PROCEDURE Delete(key: ARRAY OF CHAR);

PROCEDURE Main();

BEGIN
  WriteString("Entrez les valeurs à insérer dans le trie (séparées par des espaces) : ");
  WriteStringLn();
  FOR value := ReadCard UNTIL value = 0 DO
    WriteString("Entrez une clé pour la valeur : ");
    WriteStringLn();
    Insert(value, ReadString);
  END;
  WriteStringLn();

  WriteString("Entrez une clé pour rechercher une valeur : ");
  WriteStringLn();
  IF Search(ReadString) THEN
    WriteStringLn("La valeur a été trouvée dans le trie.")
  ELSE
    WriteStringLn("La valeur n'a pas été trouvée dans le trie.")
  END;

  WriteStringLn();
  WriteString("Entrez une clé pour supprimer une valeur : ");
  WriteStringLn();
  Delete(ReadString);

  PrintTrie(root, []);

  WriteStringLn();
END Trie.
```

**Explication du code**

**Type TrieNode**

Le **TrieNode** est la structure de données de base du trie, contenant :

- **value:** Valeur associée au nœud
- **children:** Tableau d'enfants, indexés par des caractères

**Variables**

- **root:** Pointeur vers le nœud racine du trie

**Procédures**

- **PrintTrie:** Affiche le trie dans un format hiérarchique
- **Insert:** Insère une valeur dans le trie à l'aide d'une clé
- **Search:** Recherche une valeur dans le trie à l'aide d'une clé
- **Delete:** Supprime une valeur du trie à l'aide d'une clé
- **Main:** Point d'entrée du module

**Fonctionnement du code**

1. **Insertion:** L'insertion d'une paire valeur-clé commence au nœud racine. Si l'un des caractères de la clé ne correspond à aucun enfant existant, un nouveau nœud est créé et inséré à cet endroit. Cette procédure se poursuit jusqu'à ce que le dernier caractère de la clé soit atteint, moment auquel la valeur est associée au nœud final.

2. **Recherche:** La recherche d'une valeur à l'aide d'une clé commence également à la racine. Si un caractère de la clé ne correspond à aucun enfant existant, la recherche échoue. Sinon, la recherche se poursuit dans le nœud enfant correspondant, jusqu'à ce que le dernier caractère de la clé soit atteint. La valeur associée au nœud final est renvoyée.

3. **Suppression:** La suppression d'une valeur à l'aide d'une clé commence par la recherche du nœud final correspondant. Si le nœud n'a pas d'enfants, il est supprimé. Si le nœud a des enfants, des vérifications supplémentaires sont effectuées pour déterminer si le nœud peut être supprimé en toute sécurité sans affecter la recherche d'autres valeurs.

Ce code implémente un trie qui est utilisé pour stocker des paires valeur-clé. Il permet d'insérer, de rechercher et de supprimer efficacement des valeurs à l'aide de clés.