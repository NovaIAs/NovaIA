```objective-c
#import <Foundation/Foundation.h>

@interface Personne : NSObject
{
    NSString *_nom;
    NSString *_prenom;
    NSDate *_dateNaissance;
}

@property (nonatomic, strong) NSString *nom;
@property (nonatomic, strong) NSString *prenom;
@property (nonatomic, strong) NSDate *dateNaissance;

- (instancetype)initWithNom:(NSString *)nom prenom:(NSString *)prenom dateNaissance:(NSDate *)dateNaissance;

@end

@implementation Personne

- (instancetype)initWithNom:(NSString *)nom prenom:(NSString *)prenom dateNaissance:(NSDate *)dateNaissance
{
    if (self = [super init]) {
        _nom = nom;
        _prenom = prenom;
        _dateNaissance = dateNaissance;
    }
    return self;
}

@end

@interface Voiture : NSObject
{
    NSString *_marque;
    NSString *_modele;
    int _annee;
}

@property (nonatomic, strong) NSString *marque;
@property (nonatomic, strong) NSString *modele;
@property (nonatomic, assign) int annee;

- (instancetype)initWithMarque:(NSString *)marque modele:(NSString *)modele annee:(int)annee;

@end

@implementation Voiture

- (instancetype)initWithMarque:(NSString *)marque modele:(NSString *)modele annee:(int)annee
{
    if (self = [super init]) {
        _marque = marque;
        _modele = modele;
        _annee = annee;
    }
    return self;
}

@end

@interface Maison : NSObject
{
    NSString *_adresse;
    float _superficie;
    int _nombrePieces;
}

@property (nonatomic, strong) NSString *adresse;
@property (nonatomic, assign) float superficie;
@property (nonatomic, assign) int nombrePieces;

- (instancetype)initWithAdresse:(NSString *)adresse superficie:(float)superficie nombrePieces:(int)nombrePieces;

@end

@implementation Maison

- (instancetype)initWithAdresse:(NSString *)adresse superficie:(float)superficie nombrePieces:(int)nombrePieces
{
    if (self = [super init]) {
        _adresse = adresse;
        _superficie = superficie;
        _nombrePieces = nombrePieces;
    }
    return self;
}

@end

@interface Famille : NSObject
{
    NSArray *_personnes;
    NSArray *_voitures;
    Maison *_maison;
}

@property (nonatomic, strong) NSArray *personnes;
@property (nonatomic, strong) NSArray *voitures;
@property (nonatomic, strong) Maison *maison;

- (instancetype)initWithPersonnes:(NSArray *)personnes voitures:(NSArray *)voitures maison:(Maison *)maison;

@end

@implementation Famille

- (instancetype)initWithPersonnes:(NSArray *)personnes voitures:(NSArray *)voitures maison:(Maison *)maison
{
    if (self = [super init]) {
        _personnes = personnes;
        _voitures = voitures;
        _maison = maison;
    }
    return self;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Création d'une personne
        Personne *personne1 = [[Personne alloc] initWithNom:@"Dupont" prenom:@"Jean" dateNaissance:[NSDate dateWithTimeIntervalSince1970:631152000]];
        
        // Création d'une voiture
        Voiture *voiture1 = [[Voiture alloc] initWithMarque:@"Peugeot" modele:@"208" annee:2015];
        
        // Création d'une maison
        Maison *maison1 = [[Maison alloc] initWithAdresse:@"1 rue de la Paix" superficie:120.5 nombrePieces:5];
        
        // Création d'une famille
        Famille *famille1 = [[Famille alloc] initWithPersonnes:@[personne1] voitures:@[voiture1] maison:maison1];
        
        // Affichage des informations de la famille
        NSLog(@"Nom de la famille : %@", famille1.personnes.firstObject.nom);
        NSLog(@"Prénom de la première personne : %@", famille1.personnes.firstObject.prenom);
        NSLog(@"Date de naissance de la première personne : %@", famille1.personnes.firstObject.dateNaissance);
        NSLog(@"Marque de la première voiture : %@", famille1.voitures.firstObject.marque);
        NSLog(@"Adresse de la maison : %@", famille1.maison.adresse);
    }
    return 0;
}
```

**Explication du code :**

Ce code est écrit en Objective-C, un langage de programmation orienté objet utilisé pour développer des applications macOS, iOS et tvOS.

Le code crée un modèle d'objet pour représenter une famille, avec des informations sur les personnes, les voitures et la maison de la famille. Les classes `Personne`, `Voiture` et `Maison` sont définies comme des sous-classes de la classe mère `NSObject` et disposent de leurs propres propriétés et méthodes pour gérer les données.

La classe `Famille` regroupe les instances des classes `Personne`, `Voiture` et `Maison`, permettant de stocker toutes les informations sur la famille dans un seul objet.

La fonction `main` est le point d'entrée du programme. Elle crée des instances des classes `Personne`, `Voiture`, `Maison` et `Famille`, puis affiche les informations sur la famille dans la console.

Ce code est un exemple de programmation orientée objet, qui permet de modéliser des données réelles et de gérer leurs relations et interactions.