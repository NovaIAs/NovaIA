```objective-c
#import <Foundation/Foundation.h>

@interface Personne : NSObject
@property (strong, nonatomic) NSString *nom;
@property (strong, nonatomic) NSDate *dateNaissance;
@end

@implementation Personne
- (instancetype)initWithNom:(NSString *)nom dateNaissance:(NSDate *)dateNaissance {
    if (self = [super init]) {
        _nom = nom;
        _dateNaissance = dateNaissance;
    }
    return self;
}
@end

@interface Voiture : NSObject
@property (strong, nonatomic) NSString *marque;
@property (strong, nonatomic) NSArray<Personne *> *passagers;
@end

@implementation Voiture
- (instancetype)initWithMarque:(NSString *)marque passagers:(NSArray<Personne *> *)passagers {
    if (self = [super init]) {
        _marque = marque;
        _passagers = passagers;
    }
    return self;
}
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Personne *john = [[Personne alloc] initWithNom:@"John Doe" dateNaissance:[NSDate dateWithTimeIntervalSince1970:0]];
        Personne *jane = [[Personne alloc] initWithNom:@"Jane Smith" dateNaissance:[NSDate dateWithTimeIntervalSince1970:0]];
        
        Voiture *voiture = [[Voiture alloc] initWithMarque:@"Tesla" passagers:@[john, jane]];
        
        for (Personne *passager in voiture.passagers) {
            NSLog(@"Passager : %@ (%@)", passager.nom, passager.dateNaissance);
        }
    }
    return 0;
}
```

**Explication du code :**

* **Classes et objets :**
    * Les classes `Personne` et `Voiture` représentent respectivement les personnes et les voitures.
    * Les objets `john` et `jane` sont des instances de la classe `Personne`, représentant deux personnes.
    * L'objet `voiture` est une instance de la classe `Voiture`, représentant une voiture avec les personnes `john` et `jane` comme passagers.

* **Propriétés :**
    * Les propriétés `nom` et `dateNaissance` dans la classe `Personne` stockent le nom et la date de naissance d'une personne.
    * La propriété `marque` dans la classe `Voiture` stocke la marque de la voiture.
    * La propriété `passagers` dans la classe `Voiture` est un tableau contenant les objets `Personne` représentant les passagers de la voiture.

* **Initialisation :**
    * Les méthodes d'initialisation `initWithNom:dateNaissance:` et `initWithMarque:passagers:` sont utilisées pour créer des instances des classes `Personne` et `Voiture`.

* **Itération :**
    * La boucle `for` parcourt le tableau `passagers` de la voiture et imprime le nom et la date de naissance de chaque passager.

* **Gestion de la mémoire automatique (ARC) :**
    * Le mot-clé `@autoreleasepool` est utilisé pour gérer automatiquement la mémoire des objets créés à l'intérieur du bloc. Cela garantit que les objets sont libérés de la mémoire lorsqu'ils ne sont plus référencés.