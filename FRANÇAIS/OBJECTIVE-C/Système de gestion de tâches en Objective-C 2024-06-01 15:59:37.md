**Objectif du code :** Implémenter un système simple de gestion de tâches en Objective-C.

**Classe Tâche**

```objective-c
@interface Tache : NSObject

@property (nonatomic, strong) NSString *titre;
@property (nonatomic, strong) NSString *description;
@property (nonatomic, assign) NSDate *dateCreation;
@property (nonatomic, assign) NSDate *dateEcheance;
@property (nonatomic, assign) BOOL termine;

@end
```

**Classe GestionnaireTaches**

```objective-c
@interface GestionnaireTaches : NSObject

@property (nonatomic, strong) NSMutableArray *taches;

- (void)ajouterTache:(Tache *)tache;
- (void)supprimerTache:(Tache *)tache;
- (NSArray *)obtenirTaches;
- (NSArray *)obtenirTachesTerminees;
- (NSArray *)obtenirTachesNonTerminees;

@end
```

**Implémentation de Tâche**

```objective-c
@implementation Tache

- (instancetype)initWithTitre:(NSString *)titre
                    description:(NSString *)description
                dateCreation:(NSDate *)dateCreation
                dateEcheance:(NSDate *)dateEcheance
                     termine:(BOOL)termine {
    self = [super init];
    if (self) {
        self.titre = titre;
        self.description = description;
        self.dateCreation = dateCreation;
        self.dateEcheance = dateEcheance;
        self.termine = termine;
    }
    return self;
}

@end
```

**Implémentation de GestionnaireTaches**

```objective-c
@implementation GestionnaireTaches

- (instancetype)init {
    self = [super init];
    if (self) {
        self.taches = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)ajouterTache:(Tache *)tache {
    [self.taches addObject:tache];
}

- (void)supprimerTache:(Tache *)tache {
    [self.taches removeObject:tache];
}

- (NSArray *)obtenirTaches {
    return self.taches;
}

- (NSArray *)obtenirTachesTerminees {
    return [self.taches filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"termine == YES"]];
}

- (NSArray *)obtenirTachesNonTerminees {
    return [self.taches filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"termine == NO"]];
}

@end
```

**Exemple d'utilisation**

```objective-c
// Créer un gestionnaire de tâches
GestionnaireTaches *gestionnaireTaches = [[GestionnaireTaches alloc] init];

// Créer une tâche
Tache *tache1 = [[Tache alloc] initWithTitre:@"Faire les courses"
                                    description:@"Acheter du pain, du lait et du fromage"
                                dateCreation:[NSDate date]
                                dateEcheance:[NSDate dateWithTimeIntervalSinceNow:60 * 60 * 24]
                                     termine:NO];

// Ajouter la tâche au gestionnaire
[gestionnaireTaches ajouterTache:tache1];

// Obtenir la liste des tâches
NSArray *taches = [gestionnaireTaches obtenirTaches];

// Afficher les tâches
for (Tache *tache in taches) {
    NSLog(@"Titre : %@, Description : %@", tache.titre, tache.description);
}
```

Ce code permet de créer et de gérer des tâches, notamment en les ajoutant, en les supprimant et en les filtrant en fonction de leur statut terminé ou non terminé. Il utilise des propriétés, des méthodes et des NSArray pour stocker et manipuler les données de tâche.