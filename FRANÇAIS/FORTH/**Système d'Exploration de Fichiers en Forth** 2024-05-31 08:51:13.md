**Système d'exploration de fichiers en FORTH**

```forth
: fichier-ouvrir ( chemin -- fichier )
  create ( -- fich ) dup 0 cell+ ( -- fich addr )
  CREATE >FILE ( -- )
  load-string dup cell+ @ ( -- chemin addr ) 0 rot file-open ( -- fich err )
  ?dup if cell+ 0 swap ( -- err fich )  drop if ;

: fichier-écrire ( fichier taille tampon -- )
  over 0 swap 2dup + file-write ;

: fichier-lire ( fichier taille tampon -- )
  over 0 swap 2dup + file-read ;

: répertoire-lister ( chemin -- )
  create ( -- dir ) dup 0 cell+ ( -- dir addr )
  create ( -- handle )
  load-string dup cell+ @ ( -- chemin addr ) 0 rot folder-open ( -- dir err )
  ?dup if cell+ 0 swap ( -- err dir )  drop if
  begin
    0 rot ( -- handle )
    directory-entry? if
      dup load-string cell+ @ ( -- path )
      load-string 0 >cell
      begin
        cell <
        over ( -- i )
        cell @ ( -- c )
        load-string cell+ @ ( -- suffix )
        c #= if load-string cell+ @ ( -- ext )
          load-string cell+ @ ( -- .)
          load-string cell+ @ ( -- espace )
          load-string 0 >cell
          begin
            cell <
            over ( -- i )
            cell @ ( -- c )
            load-string cell+ @ ( -- path )
          repeat
          load-string 0 >cell
          begin
            cell <
            over ( -- i )
            cell @ ( -- c )
            cell @ ( -- ext )
          repeat
          ' cr .
          load-string cell+ @ ( -- sep )
          '.' path sep 2drop .
          load-string cell+ @ ( -- sep )
          ext sep 2drop .
        if
      repeat
      1 directory-next ( -- handle )
    else drop
    until
  ;
```

**Explication du code :**

Ce code fournit un système d'exploration de fichiers en FORTH. Il comporte trois fonctions principales :

* `fichier-ouvrir` : Ouvre un fichier spécifié par son chemin et retourne un identificateur de fichier.
* `fichier-écrire` : Écrit des données dans un fichier ouvert.
* `répertoire-lister` : Liste les fichiers et répertoires dans un répertoire spécifié.

**Utilisation :**

Pour utiliser ce code, commencez par ouvrir un fichier :

```forth
fichier-ouvrir "mon_fichier.txt" Fichier
```

Vous pouvez ensuite écrire des données dans le fichier :

```forth
"Ceci est une ligne de texte" fichier-écrire
```

Et enfin, vous pouvez lister les fichiers et répertoires dans un répertoire :

```forth
répertoire-lister "mon_répertoire"
```