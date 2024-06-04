**Classe Gestionnaire de Stock**

```objective-c
@interface GestionnaireStock : NSObject

- (instancetype)initWithCapacité:(NSUInteger)capacité;

- (void)ajouterProduit:(Produit *)produit;
- (void)retirerProduit:(Produit *)produit;
- (NSUInteger)nombreDeProduits;
- (BOOL)estVide;

@end
```

**Classe Produit**

```objective-c
@interface Produit : NSObject

@property (nonatomic, copy) NSString *nom;
@property (nonatomic) float prix;
@property (nonatomic) NSUInteger quantité;

@end
```

**Implantation de la classe Gestionnaire de Stock**

```objective-c
@implementation GestionnaireStock

- (instancetype)initWithCapacité:(NSUInteger)capacité {
    if (self = [super init]) {
        _produits = [NSMutableArray arrayWithCapacity:capacité];
    }
    return self;
}

- (void)ajouterProduit:(Produit *)produit {
    [_produits addObject:produit];
}

- (void)retirerProduit:(Produit *)produit {
    [_produits removeObject:produit];
}

- (NSUInteger)nombreDeProduits {
    return _produits.count;
}

- (BOOL)estVide {
    return _produits.count == 0;
}

@end
```

**Implantation de la classe Produit**

```objective-c
@implementation Produit

@end
```

**Exemple d'utilisation**

```objective-c
// Créer un gestionnaire de stock avec une capacité de 100 produits
GestionnaireStock *stock = [[GestionnaireStock alloc] initWithCapacité:100];

// Ajouter des produits au stock
Produit *produit1 = [[Produit alloc] init];
produit1.nom = @"Pomme";
produit1.prix = 1.25;
produit1.quantité = 10;

[stock ajouterProduit:produit1];

Produit *produit2 = [[Produit alloc] init];
produit2.nom = @"Orange";
produit2.prix = 1.50;
produit2.quantité = 15;

[stock ajouterProduit:produit2];

// Vérifier le nombre de produits dans le stock
NSLog(@"Nombre de produits : %lu", [stock nombreDeProduits]);

// Vérifier si le stock est vide
if ([stock estVide]) {
    NSLog(@"Le stock est vide");
} else {
    NSLog(@"Le stock n'est pas vide");
}

// Retirer un produit du stock
[stock retirerProduit:produit1];

// Vérifier le nombre de produits dans le stock après la suppression
NSLog(@"Nombre de produits après suppression : %lu", [stock nombreDeProduits]);
```

**Explication du code**

Ce code implémente un gestionnaire de stock simple en Objective-C. La classe `GestionnaireStock` gère une collection de produits, et les méthodes `ajouterProduit:` et `retirerProduit:` permettent d'ajouter et de retirer des produits du stock. La classe `Produit` représente un produit unique avec un nom, un prix et une quantité.

L'exemple d'utilisation montre comment créer un gestionnaire de stock, ajouter des produits, vérifier le nombre de produits et retirer un produit du stock.