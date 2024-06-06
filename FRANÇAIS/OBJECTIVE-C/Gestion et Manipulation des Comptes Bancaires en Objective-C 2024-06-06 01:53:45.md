**Gestionnaire de comptes en banque**

**Implémentation de la classe CompteBancaire**

```objective-c
@implementation CompteBancaire

- (instancetype)initWithNom:(NSString *)nom solde:(double)solde {
    self = [super init];
    if (self) {
        _nom = nom;
        _solde = solde;
    }
    return self;
}

- (NSString *)description {
    return [NSString stringWithFormat:@"Compte %@ : Solde %.2f €", _nom, _solde];
}

- (BOOL)debiter:(double)montant {
    if (montant > 0 && montant <= _solde) {
        _solde -= montant;
        return YES;
    }
    return NO;
}

- (BOOL)crediter:(double)montant {
    if (montant > 0) {
        _solde += montant;
        return YES;
    }
    return NO;
}

@end
```

**Interface de la classe Banque**

```objective-c
@interface Banque : NSObject

- (instancetype)initWithNom:(NSString *)nom;

- (void)ajouterCompte:(CompteBancaire *)compte;
- (void)supprimerCompte:(CompteBancaire *)compte;
- (CompteBancaire *)trouverCompteParNom:(NSString *)nom;

@end
```

**Implémentation de la classe Banque**

```objective-c
@implementation Banque

- (instancetype)initWithNom:(NSString *)nom {
    self = [super init];
    if (self) {
        _nom = nom;
        _comptes = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)ajouterCompte:(CompteBancaire *)compte {
    [_comptes addObject:compte];
}

- (void)supprimerCompte:(CompteBancaire *)compte {
    [_comptes removeObject:compte];
}

- (CompteBancaire *)trouverCompteParNom:(NSString *)nom {
    for (CompteBancaire *compte in _comptes) {
        if ([compte.nom isEqualToString:nom]) {
            return compte;
        }
    }
    return nil;
}

@end
```

**Exemple d'utilisation**

```objective-c
// Créer une banque
Banque *banque = [[Banque alloc] initWithNom:@"Ma Banque"];

// Créer des comptes bancaires
CompteBancaire *compte1 = [[CompteBancaire alloc] initWithNom:@"Compte 1" solde:100];
CompteBancaire *compte2 = [[CompteBancaire alloc] initWithNom:@"Compte 2" solde:200];

// Ajouter les comptes à la banque
[banque ajouterCompte:compte1];
[banque ajouterCompte:compte2];

// Trouver un compte par son nom
CompteBancaire *compteTrouve = [banque trouverCompteParNom:@"Compte 1"];

// Afficher les informations du compte trouvé
NSLog(@"%@", compteTrouve.description);

// Débiter et créditer des montants
[compteTrouve debiter:50];
[compteTrouve crediter:100];

// Afficher le solde après les opérations
NSLog(@"Solde après les opérations : %.2f €", compteTrouve.solde);
```

**Explication**

* **CompteBancaire** : Représente un compte bancaire avec un nom et un solde.
* **Banque** : Gère une collection de comptes bancaires et permet d'ajouter, supprimer et trouver des comptes.
* **Exemple d'utilisation** : Montre comment créer une banque, ajouter des comptes, trouver un compte par son nom et effectuer des opérations de débit et de crédit.

Ce code démontre des concepts avancés de programmation orientée objet, tels que l'héritage, le polymorphisme et la gestion des collections.