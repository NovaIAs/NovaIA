**Gestionnaire d'Étudiants et de Cours**

**Classe Étudiant**

```objective-c
@interface Etudiant : NSObject
@property (nonatomic, strong) NSString *nom;
@property (nonatomic, strong) NSString *prenom;
@property (nonatomic, strong) NSNumber *age;
@property (nonatomic, strong) NSArray *coursInscrits;
@end

@implementation Etudiant
- (instancetype)initWithNom:(NSString *)nom prenom:(NSString *)prenom age:(NSNumber *)age coursInscrits:(NSArray *)coursInscrits {
    self = [super init];
    if (self) {
        _nom = nom;
        _prenom = prenom;
        _age = age;
        _coursInscrits = coursInscrits;
    }
    return self;
}
@end
```

**Classe Cours**

```objective-c
@interface Cours : NSObject
@property (nonatomic, strong) NSString *nom;
@property (nonatomic, strong) NSNumber *credits;
@property (nonatomic, strong) NSDate *dateDebut;
@property (nonatomic, strong) NSDate *dateFin;
@property (nonatomic, strong) NSArray *etudiantsInscrits;
@end

@implementation Cours
- (instancetype)initWithNom:(NSString *)nom credits:(NSNumber *)credits dateDebut:(NSDate *)dateDebut dateFin:(NSDate *)dateFin etudiantsInscrits:(NSArray *)etudiantsInscrits {
    self = [super init];
    if (self) {
        _nom = nom;
        _credits = credits;
        _dateDebut = dateDebut;
        _dateFin = dateFin;
        _etudiantsInscrits = etudiantsInscrits;
    }
    return self;
}
@end
```

**Gestionnaire**

```objective-c
@interface Gestionnaire : NSObject
@property (nonatomic, strong) NSMutableArray *etudiants;
@property (nonatomic, strong) NSMutableArray *cours;

- (void)ajouterEtudiant:(Etudiant *)etudiant;
- (void)supprimerEtudiant:(Etudiant *)etudiant;
- (void)modifierEtudiant:(Etudiant *)etudiant;
- (NSArray *)trouverEtudiantsParNom:(NSString *)nom;
- (NSArray *)trouverEtudiantsParAge:(NSNumber *)age;
- (NSArray *)trouverCoursParNom:(NSString *)nom;
- (NSArray *)trouverCoursParNombreDeCredits:(NSNumber *)nombreDeCredits;
- (NSArray *)trouverCoursParDates:(NSDate *)dateDebut dateFin:(NSDate *)dateFin;
- (NSArray *)trouverCoursAvecEtudiantsInscrits;
- (void)inscrireEtudiant:(Etudiant *)etudiant dansCours:(Cours *)cours;
- (void)desinscrireEtudiant:(Etudiant *)etudiant deCours:(Cours *)cours;
@end

@implementation Gestionnaire
- (instancetype)init {
    self = [super init];
    if (self) {
        _etudiants = [NSMutableArray array];
        _cours = [NSMutableArray array];
    }
    return self;
}

- (void)ajouterEtudiant:(Etudiant *)etudiant {
    [_etudiants addObject:etudiant];
}

- (void)supprimerEtudiant:(Etudiant *)etudiant {
    [_etudiants removeObject:etudiant];
}

- (void)modifierEtudiant:(Etudiant *)etudiant {
    NSUInteger index = [_etudiants indexOfObject:etudiant];
    if (index != NSNotFound) {
        [_etudiants replaceObjectAtIndex:index withObject:etudiant];
    }
}

- (NSArray *)trouverEtudiantsParNom:(NSString *)nom {
    NSPredicate *predicate = [NSPredicate predicateWithFormat:@"nom = %@", nom];
    return [_etudiants filteredArrayUsingPredicate:predicate];
}

- (NSArray *)trouverEtudiantsParAge:(NSNumber *)age {
    NSPredicate *predicate = [NSPredicate predicateWithFormat:@"age = %@", age];
    return [_etudiants filteredArrayUsingPredicate:predicate];
}

- (NSArray *)trouverCoursParNom:(NSString *)nom {
    NSPredicate *predicate = [NSPredicate predicateWithFormat:@"nom = %@", nom];
    return [_cours filteredArrayUsingPredicate:predicate];
}

- (NSArray *)trouverCoursParNombreDeCredits:(NSNumber *)nombreDeCredits {
    NSPredicate *predicate = [NSPredicate predicateWithFormat:@"credits = %@", nombreDeCredits];
    return [_cours filteredArrayUsingPredicate:predicate];
}

- (NSArray *)trouverCoursParDates:(NSDate *)dateDebut dateFin:(NSDate *)dateFin {
    NSPredicate *predicate = [NSPredicate predicateWithFormat:@"dateDebut >= %@ AND dateFin <= %@", dateDebut, dateFin];
    return [_cours filteredArrayUsingPredicate:predicate];
}

- (NSArray *)trouverCoursAvecEtudiantsInscrits {
    return [_cours filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"etudiantsInscrits.@count > 0"]];
}

- (void)inscrireEtudiant:(Etudiant *)etudiant dansCours:(Cours *)cours {
    etudiant.coursInscrits = [etudiant.coursInscrits arrayByAddingObject:cours];
    cours.etudiantsInscrits = [cours.etudiantsInscrits arrayByAddingObject:etudiant];
}

- (void)desinscrireEtudiant:(Etudiant *)etudiant deCours:(Cours *)cours {
    etudiant.coursInscrits = [etudiant.coursInscrits filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"nom != %@", cours.nom]];
    cours.etudiantsInscrits = [cours.etudiantsInscrits filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"nom != %@", etudiant.nom]];
}
@end
```

**Utilisation**

```objective-c
// Créer le gestionnaire
Gestionnaire *gestionnaire = [[Gestionnaire alloc] init];

// Créer des étudiants
Etudiant *etudiant1 = [[Etudiant alloc] initWithNom:@"Dupont" prenom:@"Jean" age:@20 coursInscrits:nil];
Etudiant *etudiant2 = [[Etudiant alloc] initWithNom:@"Martin" prenom:@"Marie" age:@21 coursInscrits:nil];

// Créer des cours
Cours *cours1 = [[Cours alloc] initWithNom:@"Mathématiques" credits:@5 dateDebut:[NSDate date] dateFin:[NSDate dateWithTimeIntervalSinceNow:60*60*24*7] etudiantsInscrits:nil];
Cours *cours2 = [[Cours alloc] initWithNom:@"Informatique" credits:@3 dateDebut:[NSDate dateWithTimeIntervalSinceNow:60*60*24] dateFin:[NSDate dateWithTimeIntervalSinceNow:60*60*24*7*2] etudiantsInscrits:nil];

// Ajouter les étudiants et les cours au gestionnaire
[gestionnaire ajouterEtudiant:etudiant1];
[gestionnaire ajouterEtudiant:etudiant2];
[gestionnaire ajouterCours:cours1];
[gestionnaire ajouterCours:cours2];

// Inscrire les étudiants dans les cours
[gestionnaire inscrireEtudiant:etudiant1 dansCours:cours1];
[gestionnaire inscrireEtudiant:etudiant1 dansCours:cours2];
[gestionnaire inscrireEtudiant:etudiant2 dansCours:cours1];

// Obtenir une liste des étudiants inscrits dans un cours
NSArray *etudiantsInscritsDansCours1 = [gestionnaire trouverCoursParNom:@"Mathématiques"].etudiantsInscrits;

// Obtenir une liste des cours auxquels un étudiant est inscrit
NSArray *coursInscritsEtudiant1 = etudiant1.coursInscrits;

// Supprimer un étudiant du gestionnaire
[gestionnaire supprimerEtudiant:etudiant2];

// Supprimer un cours du gestionnaire
[gestionnaire supprimerCours:cours2];
```

**Explications**

* Les classes `Etudiant` et `Cours` modélisent les entités principales du système.
* La classe `Gestionnaire` fournit une interface pour gérer les étudiants et les cours, ainsi que leurs inscriptions.
* Les méthodes de recherche utilisent des prédicats pour filtrer les données en fonction de critères spécifiques.
* Les collections `NSMutableArray` sont utilisées pour stocker les listes d'étudiants et de cours.
* Les relations entre les étudiants et les cours sont gérées en mettant à jour les propriétés `coursInscrits` et `etudiantsInscrits` respectives.
* Le code est conçu pour être extensible et convivial pour le développeur, permettant la manipulation et la gestion des données de manière efficace.