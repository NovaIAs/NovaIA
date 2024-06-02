**Classe Gestionnaire de Fichiers Avancée**

```objective-c
#import <Foundation/Foundation.h>

@interface GestionnaireFichiers : NSObject

- (BOOL)creerFichier:(NSString *)chemin erreur:(NSError **)erreur;
- (BOOL)ecrireDansFichier:(NSString *)chemin contenu:(NSData *)contenu erreur:(NSError **)erreur;
- (NSData *)lireFichier:(NSString *)chemin erreur:(NSError **)erreur;
- (BOOL)supprimerFichier:(NSString *)chemin erreur:(NSError **)erreur;
- (BOOL)deplacerFichier:(NSString *)cheminSource vers:(NSString *)cheminDestination erreur:(NSError **)erreur;
- (BOOL)copierFichier:(NSString *)cheminSource vers:(NSString *)cheminDestination erreur:(NSError **)erreur;
- (NSArray<NSString *> *)listerFichiersDansRepertoire:(NSString *)chemin erreur:(NSError **)erreur;
- (NSDictionary<NSString *, NSNumber *> *)obtenirAttributsFichier:(NSString *)chemin erreur:(NSError **)erreur;

@end

@implementation GestionnaireFichiers

- (BOOL)creerFichier:(NSString *)chemin erreur:(NSError **)erreur {
    NSFileManager *gestionnaireFichiers = [NSFileManager defaultManager];
    return [gestionnaireFichiers createFileAtPath:chemin contents:nil attributes:nil];
}

- (BOOL)ecrireDansFichier:(NSString *)chemin contenu:(NSData *)contenu erreur:(NSError **)erreur {
    NSFileManager *gestionnaireFichiers = [NSFileManager defaultManager];
    return [gestionnaireFichiers createFileAtPath:chemin contents:contenu attributes:nil];
}

- (NSData *)lireFichier:(NSString *)chemin erreur:(NSError **)erreur {
    NSFileManager *gestionnaireFichiers = [NSFileManager defaultManager];
    return [gestionnaireFichiers contentsAtPath:chemin];
}

- (BOOL)supprimerFichier:(NSString *)chemin erreur:(NSError **)erreur {
    NSFileManager *gestionnaireFichiers = [NSFileManager defaultManager];
    return [gestionnaireFichiers removeItemAtPath:chemin error:erreur];
}

- (BOOL)deplacerFichier:(NSString *)cheminSource vers:(NSString *)cheminDestination erreur:(NSError **)erreur {
    NSFileManager *gestionnaireFichiers = [NSFileManager defaultManager];
    return [gestionnaireFichiers moveItemAtPath:cheminSource toPath:cheminDestination error:erreur];
}

- (BOOL)copierFichier:(NSString *)cheminSource vers:(NSString *)cheminDestination erreur:(NSError **)erreur {
    NSFileManager *gestionnaireFichiers = [NSFileManager defaultManager];
    return [gestionnaireFichiers copyItemAtPath:cheminSource toPath:cheminDestination error:erreur];
}

- (NSArray<NSString *> *)listerFichiersDansRepertoire:(NSString *)chemin erreur:(NSError **)erreur {
    NSFileManager *gestionnaireFichiers = [NSFileManager defaultManager];
    return [gestionnaireFichiers contentsOfDirectoryAtPath:chemin error:erreur];
}

- (NSDictionary<NSString *, NSNumber *> *)obtenirAttributsFichier:(NSString *)chemin erreur:(NSError **)erreur {
    NSFileManager *gestionnaireFichiers = [NSFileManager defaultManager];
    return [gestionnaireFichiers attributesOfItemAtPath:chemin error:erreur];
}

@end
```

**Exemple d'Utilisation**

```objective-c
GestionnaireFichiers *gestionnaire = [[GestionnaireFichiers alloc] init];

NSString *cheminFichier = @"/tmp/test.txt";

if ([gestionnaire creerFichier:cheminFichier erreur:nil]) {
    NSData *contenu = [@"Bonjour le monde !" dataUsingEncoding:NSUTF8StringEncoding];
    if ([gestionnaire ecrireDansFichier:cheminFichier contenu:contenu erreur:nil]) {
        NSData *contenuLu = [gestionnaire lireFichier:cheminFichier erreur:nil];
        NSString *contenuTexte = [[NSString alloc] initWithData:contenuLu encoding:NSUTF8StringEncoding];
        NSLog(@"Contenu du fichier : %@", contenuTexte);
    }
} else {
    NSLog(@"Impossible de créer le fichier");
}
```