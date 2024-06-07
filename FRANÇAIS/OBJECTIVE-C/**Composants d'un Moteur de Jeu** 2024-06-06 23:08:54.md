**Classe Moteur de Jeu**

```objective-c
@interface MoteurJeu : NSObject

// Propriétés de base
@property (nonatomic, strong) NSString *nom;
@property (nonatomic, assign) NSInteger nombreNiveaux;

// Méthodes d'initialisation
- (instancetype)initWithNom:(NSString *)nom nombreNiveaux:(NSInteger)nombreNiveaux;

// Méthodes de jeu
- (void)demarrerJeu;
- (void)chargerNiveau:(NSInteger)numeroNiveau;
- (void)mettreEnPauseJeu;
- (void)reprendreJeu;
- (void)arreterJeu;

@end
```

**Classe Niveau**

```objective-c
@interface Niveau : NSObject

// Propriétés de base
@property (nonatomic, strong) NSString *nom;
@property (nonatomic, assign) NSInteger largeur;
@property (nonatomic, assign) NSInteger hauteur;
@property (nonatomic, strong) NSArray<Personnage *> *personnages;
@property (nonatomic, strong) NSArray<Obstacle *> *obstacles;

// Méthodes d'initialisation
- (instancetype)initWithNom:(NSString *)nom largeur:(NSInteger)largeur hauteur:(NSInteger)hauteur;

// Autres méthodes
- (void)ajouterPersonnage:(Personnage *)personnage;
- (void)ajouterObstacle:(Obstacle *)obstacle;
- (void)supprimerPersonnage:(Personnage *)personnage;
- (void)supprimerObstacle:(Obstacle *)obstacle;

@end
```

**Classe Personnage**

```objective-c
@interface Personnage : NSObject

// Propriétés de base
@property (nonatomic, strong) NSString *nom;
@property (nonatomic, assign) CGPoint position;
@property (nonatomic, assign) NSInteger pointsDeVie;

// Méthodes d'initialisation
- (instancetype)initWithNom:(NSString *)nom position:(CGPoint)position pointsDeVie:(NSInteger)pointsDeVie;

// Autres méthodes
- (void)seDeplacerVers:(CGPoint)destination;
- (void)attaquer:(Personnage *)cible;

@end
```

**Classe Obstacle**

```objective-c
@interface Obstacle : NSObject

// Propriétés de base
@property (nonatomic, strong) NSString *nom;
@property (nonatomic, assign) CGPoint position;
@property (nonatomic, assign) NSInteger taille;

// Méthodes d'initialisation
- (instancetype)initWithNom:(NSString *)nom position:(CGPoint)position taille:(NSInteger)taille;

@end
```

**Exemple d'utilisation**

```objective-c
// Créer un moteur de jeu avec 5 niveaux
MoteurJeu *moteur = [[MoteurJeu alloc] initWithNom:@"Mon Jeu" nombreNiveaux:5];

// Créer des niveaux et les ajouter au moteur
for (NSInteger i = 0; i < moteur.nombreNiveaux; i++) {
    Niveau *niveau = [[Niveau alloc] initWithNom:[NSString stringWithFormat:@"Niveau %ld", i] largeur:100 hauteur:100];
    [moteur ajouterNiveau:niveau];
}

// Créer des personnages et les ajouter au niveau 1
Personnage *personnage1 = [[Personnage alloc] initWithNom:@"Héros" position:CGPointMake(50, 50) pointsDeVie:100];
Personnage *personnage2 = [[Personnage alloc] initWithNom:@"Ennemi" position:CGPointMake(75, 75) pointsDeVie:50];
[moteur.niveaux[0] ajouterPersonnage:personnage1];
[moteur.niveaux[0] ajouterPersonnage:personnage2];

// Démarrer le jeu
[moteur demarrerJeu];

// Traiter les événements du jeu dans une boucle principale
while (moteur.etatJeu == EtatJeuEnCours) {
    // Vérifier les entrées des utilisateurs et mettre à jour les positions des personnages
    
    // Gérer les collisions entre les personnages et les obstacles
    
    // Mettre à jour l'affichage du jeu
}
```

**Explication du code**

Ce code représente un moteur de jeu de base qui permet de créer des niveaux, des personnages et des obstacles. Il fournit des méthodes pour démarrer, mettre en pause, reprendre et arrêter le jeu. Le code peut être utilisé pour créer des jeux simples en 2D, tels que des jeux de tir ou des jeux d'aventure.