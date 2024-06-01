**Création de la table de base**

```sql
CREATE TABLE commande (
  id_commande INT PRIMARY KEY AUTO_INCREMENT,
  date_commande DATE NOT NULL,
  montant_total DECIMAL(10, 2) NOT NULL,
  id_client INT NOT NULL,
  CONSTRAINT FK_client FOREIGN KEY (id_client) REFERENCES client (id_client)
);
```

**Création de la table des détails de commande**

```sql
CREATE TABLE detail_commande (
  id_detail_commande INT PRIMARY KEY AUTO_INCREMENT,
  id_commande INT NOT NULL,
  id_produit INT NOT NULL,
  quantite INT NOT NULL,
  prix_unitaire DECIMAL(10, 2) NOT NULL,
  CONSTRAINT FK_commande FOREIGN KEY (id_commande) REFERENCES commande (id_commande),
  CONSTRAINT FK_produit FOREIGN KEY (id_produit) REFERENCES produit (id_produit)
);
```

**Création de la vue des commandes détaillées**

```sql
CREATE VIEW commande_detail AS
SELECT
  c.id_commande,
  c.date_commande,
  c.montant_total,
  c.id_client,
  dc.id_produit,
  dc.quantite,
  dc.prix_unitaire
FROM commande c
JOIN detail_commande dc ON c.id_commande = dc.id_commande;
```

**Création de la procédure stockée pour insérer une commande**

```sql
DELIMITER $$
CREATE PROCEDURE inserer_commande (
  IN date_commande DATE,
  IN montant_total DECIMAL(10, 2),
  IN id_client INT
)
BEGIN
  INSERT INTO commande (date_commande, montant_total, id_client)
  VALUES (date_commande, montant_total, id_client);
  
  GET LAST_INSERT_ID();
END $$
DELIMITER ;
```

**Création de la fonction de calcul du montant total d'une commande**

```sql
CREATE FUNCTION get_montant_total (id_commande INT)
RETURNS DECIMAL(10, 2)
BEGIN
  DECLARE montant_total DECIMAL(10, 2) DEFAULT 0.00;

  SELECT SUM(quantite * prix_unitaire)
  INTO montant_total
  FROM detail_commande
  WHERE id_commande = id_commande;
  
  RETURN montant_total;
END;
```

**Explication du code**

Ce code crée une base de données relationnelle pour gérer des commandes et leurs détails. Il comprend :

* Une table `commande` pour stocker les informations sur les commandes (ID, date, montant total, ID client).
* Une table `detail_commande` pour stocker les détails des produits commandés (ID, ID de commande, ID de produit, quantité, prix unitaire).
* Une vue `commande_detail` qui combine les données des deux tables pour une vue complète des commandes.
* Une procédure stockée `inserer_commande` pour insérer de nouvelles commandes.
* Une fonction `get_montant_total` pour calculer le montant total d'une commande.