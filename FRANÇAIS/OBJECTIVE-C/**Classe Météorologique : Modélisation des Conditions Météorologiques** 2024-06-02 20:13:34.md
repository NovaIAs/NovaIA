**Classe Météorologique**

```objective-c
@interface Météorologie : NSObject

// Propriétés
@property NSString *localisation;
@property float températureActuelle;
@property float températureMaximale;
@property float températureMinimale;
@property float humidité;
@property float vitesseVent;
@property float pressionAtmosphérique;
@property NSString *icône;

// Méthodes
- (instancetype)initWithLocalisation:(NSString *)localisation;
- (void)actualiserConditionsMétéorologiques;
- (NSString *)conditionMétéorologiqueActuelle;

@end
```

**Explication de la classe Météorologie:**

Cette classe représente les conditions météorologiques d'un lieu donné. Elle contient des propriétés pour stocker des données telles que la localisation, la température actuelle, la température maximale/minimale, l'humidité, la vitesse du vent, la pression atmosphérique et une icône indiquant les conditions météorologiques actuelles. La classe fournit également des méthodes pour initialiser l'objet et actualiser les conditions météorologiques.

**Utilisation de la classe Météorologie:**

```objective-c
// Créer un objet Météorologie
Météorologie *météo = [[Météorologie alloc] initWithLocalisation:@"Paris"];

// Actualiser les conditions météorologiques
[météo actualiserConditionsMétéorologiques];

// Obtenir la condition météorologique actuelle
NSString *conditionActuelle = [météo conditionMétéorologiqueActuelle];
```

**Classe Voiture**

```objective-c
@interface Voiture : NSObject

// Propriétés
@property NSString *marque;
@property NSString *modèle;
@property int annéeModèle;
@property double kilométrage;
@property float consommationEssence;
@property BOOL estHybride;

// Méthodes
- (instancetype)initWithMarque:(NSString *)marque modèle:(NSString *)modèle;
- (void)rouler:(double)kilomètres;
- (void)faireLePlein;
- (float)autonomieRestante;

@end
```

**Explication de la classe Voiture:**

Cette classe représente une voiture. Elle contient des propriétés pour stocker des données telles que la marque, le modèle, l'année modèle, le kilométrage, la consommation d'essence et si la voiture est hybride. La classe fournit également des méthodes pour initialiser l'objet, conduire la voiture, faire le plein et calculer l'autonomie restante.

**Utilisation de la classe Voiture:**

```objective-c
// Créer un objet Voiture
Voiture *voiture = [[Voiture alloc] initWithMarque:@"Toyota" modèle:@"Camry"];

// Conduire la voiture
[voiture rouler:100];

// Faire le plein
[voiture faireLePlein];

// Calculer l'autonomie restante
float autonomie = [voiture autonomieRestante];
```

**Classe CompteBancaire**

```objective-c
@interface CompteBancaire : NSObject

// Propriétés
@property NSString *numéroCompte;
@property double solde;
@property float tauxIntérêt;

// Méthodes
- (instancetype)initWithNuméroCompte:(NSString *)numéroCompte solde:(double)solde;
- (void)déposer:(double)montant;
- (void)retirer:(double)montant;
- (double)calculerIntérêt;

@end
```

**Explication de la classe CompteBancaire:**

Cette classe représente un compte bancaire. Elle contient des propriétés pour stocker des données telles que le numéro de compte, le solde et le taux d'intérêt. La classe fournit également des méthodes pour initialiser l'objet, déposer et retirer de l'argent, et calculer l'intérêt.

**Utilisation de la classe CompteBancaire:**

```objective-c
// Créer un objet CompteBancaire
CompteBancaire *compte = [[CompteBancaire alloc] initWithNuméroCompte:@"123456789" solde:1000];

// Déposer de l'argent
[compte déposer:500];

// Retirer de l'argent
[compte retirer:200];

// Calculer l'intérêt
double intérêt = [compte calculerIntérêt];
```