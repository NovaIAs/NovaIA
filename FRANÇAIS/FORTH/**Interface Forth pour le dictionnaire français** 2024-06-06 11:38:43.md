```forth
CRÉER dictionnaire de la langue française

: CHARGER-FICHIER (fichier -- )
    R\ lire fichier/source
    TOUT COPIER
    ECRIRE-FICHIER "compile.forth"
    ECRIRE "Fichier chargé : " "compile.forth" CR ;

: COMPILER (fichier -- )
    R\ charger et compiler
    "compile.forth" FICHIER-SUPPRIMER ;

: ÉCRIRE-SOURCE (fichier -- )
    R\ lire fichier/source
    TÉLÉTOUTE
    ECRIRE-FICHIER "source.txt" ;

: ÉCRIRE-COMPILÉ (fichier -- )
    R\ charger et compiler
    TOUT COPIER
    ECRIRE "Code compilé :" CR
    TOUT ECRIRE ;

: RUN (fichier -- )
    R\ charger et exécuter
    interpréter ;

: INTERPRÈTE (fichier -- )
    "interpréter.forth" COMPILE ;

: MAIN (fichier -- )
    "Chargez un fichier Forth : " PROMPT
    CLAIRE
    LIT "compile.forth" LIT = SI "Fichier déjà chargé : " CR
    SINON CHARGER-FICHIER
    FIN
    "Compiler (O/N) ? " PROMPT
    CLAIRE
    47 = SI "Compilation en cours..." CR
    COMPILER
    FIN

MAIN
```

**Explication du code :**

Ce code Forth effectue les opérations suivantes :

* **CHARGER-FICHIER** : lit un fichier Forth et l'écrit dans le fichier "compile.forth".
* **COMPILER** : charge et compile le fichier "compile.forth".
* **ÉCRIRE-SOURCE** : lit un fichier Forth et l'écrit dans le fichier "source.txt".
* **ÉCRIRE-COMPILÉ** : charge et compile le fichier "compile.forth", puis affiche le code compilé.
* **RUN** : charge et exécute le fichier "compile.forth" (interpréteur).
* **INTERPRÈTE** : compile le fichier "interpréter.forth", qui met en œuvre l'interpréteur Forth.
* **MAIN** : demande à l'utilisateur un fichier Forth à charger, le charge et le compile si nécessaire, puis lui demande s'il souhaite exécuter l'interpréteur.