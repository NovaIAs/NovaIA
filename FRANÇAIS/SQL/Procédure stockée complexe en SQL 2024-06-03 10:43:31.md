**Création d'une procédure stockée complexe**

```sql
CREATE PROCEDURE MaProcedureStockéeComplexe (
  @Paramètre1 INT,
  @Paramètre2 VARCHAR(50),
  @Paramètre3 DATETIME
)
AS
BEGIN
  -- Instructions complexes
  IF @Paramètre1 > 10
  BEGIN
    UPDATE MaTable SET Champ1 = Champ1 + 1 WHERE Champ2 = @Paramètre2;
  END
  ELSE
  BEGIN
    INSERT INTO MaTable (Champ1, Champ2, Champ3) VALUES (@Paramètre1, @Paramètre2, @Paramètre3);
  END

  SELECT * FROM MaTable WHERE Champ2 = @Paramètre2;
END
```

**Explications :**

Cette procédure stockée complexe permet d'effectuer des mises à jour et des insertions dans une table en fonction des valeurs des paramètres fournis.

* **Ligne 1 :** Déclaration des paramètres d'entrée.
* **Ligne 2 :** Début de la procédure stockée.
* **Lignes 3-10 :** Instructions conditionnelles qui déterminent l'action à effectuer en fonction de la valeur de `@Paramètre1`.
* **Lignes 11-13 :** Mise à jour de la table si `@Paramètre1` est supérieur à 10.
* **Lignes 15-17 :** Insertion dans la table si `@Paramètre1` n'est pas supérieur à 10.
* **Lignes 19-21 :** Sélection des enregistrements de la table filtrés par `@Paramètre2`.
* **Ligne 22 :** Fin de la procédure stockée.

**Utilisation :**

Pour utiliser cette procédure stockée, appelez-la depuis un autre code SQL ou une application en fournissant les valeurs des paramètres. Par exemple :

```sql
EXEC MaProcedureStockéeComplexe 12, 'Valeur2', '2023-03-08';
```