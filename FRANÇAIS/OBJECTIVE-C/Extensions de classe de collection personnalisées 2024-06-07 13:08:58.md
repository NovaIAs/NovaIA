**Classe d'extension de collection personnalisée**

```objective-c
#import <Foundation/Foundation.h>

@interface CollectionExtensions : NSObject

+ (BOOL)isPrefixOfCollection:(id<NSFastEnumeration>)collection inCollection:(id<NSFastEnumeration>)collection;
+ (BOOL)isSuffixOfCollection:(id<NSFastEnumeration>)collection inCollection:(id<NSFastEnumeration>)collection;

+ (BOOL)allElementsInCollection:(id<NSFastEnumeration>)collection areEqualTo:(id)object;
+ (BOOL)anyElementInCollection:(id<NSFastEnumeration>)collection isEqualTo:(id)object;

+ (id)firstElementInCollection:(id<NSFastEnumeration>)collection thatPassesTest:(BOOL (^)(id element))validation;
+ (id)firstElementInCollection:(id<NSFastEnumeration>)collection thatFailsTest:(BOOL (^)(id element))validation;

@end
```

**Implantation**

```objective-c
#import "CollectionExtensions.h"

@implementation CollectionExtensions

+ (BOOL)isPrefixOfCollection:(id<NSFastEnumeration>)prefix inCollection:(id<NSFastEnumeration>)collection {
    NSRange range = NSMakeRange(0, [prefix count]);
    return [[collection subarrayWithRange:range] isEqualToArray:[prefix allObjects]];
}

+ (BOOL)isSuffixOfCollection:(id<NSFastEnumeration>)suffix inCollection:(id<NSFastEnumeration>)collection {
    NSRange range = NSMakeRange([collection count] - [suffix count], [suffix count]);
    return [[collection subarrayWithRange:range] isEqualToArray:[suffix allObjects]];
}

+ (BOOL)allElementsInCollection:(id<NSFastEnumeration>)collection areEqualTo:(id)object {
    for (id element in collection) {
        if (![element isEqual:object]) {
            return NO;
        }
    }
    return YES;
}

+ (BOOL)anyElementInCollection:(id<NSFastEnumeration>)collection isEqualTo:(id)object {
    for (id element in collection) {
        if ([element isEqual:object]) {
            return YES;
        }
    }
    return NO;
}

+ (id)firstElementInCollection:(id<NSFastEnumeration>)collection thatPassesTest:(BOOL (^)(id element))validation {
    for (id element in collection) {
        if (validation(element)) {
            return element;
        }
    }
    return nil;
}

+ (id)firstElementInCollection:(id<NSFastEnumeration>)collection thatFailsTest:(BOOL (^)(id element))validation {
    for (id element in collection) {
        if (!validation(element)) {
            return element;
        }
    }
    return nil;
}

@end
```

**Utilisation**

```objective-c
NSArray *nombres = @[@1, @2, @3, @4, @5];

// Vérifie si [1, 2] est un préfixe de nombres
if ([CollectionExtensions isPrefixOfCollection:@[@1, @2] inCollection:nombres]) {
    NSLog(@"Oui, [1, 2] est un préfixe de nombres");
}

// Vérifie si [4, 5] est un suffixe de nombres
if ([CollectionExtensions isSuffixOfCollection:@[@4, @5] inCollection:nombres]) {
    NSLog(@"Oui, [4, 5] est un suffixe de nombres");
}

// Vérifie si tous les nombres de la collection sont égaux à 3
if ([CollectionExtensions allElementsInCollection:nombres areEqualTo:@3]) {
    NSLog(@"Oui, tous les nombres de la collection sont égaux à 3");
}

// Vérifie si l'un des nombres de la collection est égal à 2
if ([CollectionExtensions anyElementInCollection:nombres isEqualTo:@2]) {
    NSLog(@"Oui, l'un des nombres de la collection est égal à 2");
}

// Renvoie le premier nombre de la collection supérieur à 3
id premierNombreSupérieurÀ3 = [CollectionExtensions firstElementInCollection:nombres thatPassesTest:^BOOL(id element) {
    return [element integerValue] > 3;
}];
NSLog(@"Premier nombre supérieur à 3 : %@", premierNombreSupérieurÀ3);

// Renvoie le premier nombre de la collection qui n'est pas divisible par 2
id premierNombreNonDivisiblePar2 = [CollectionExtensions firstElementInCollection:nombres thatFailsTest:^BOOL(id element) {
    return [element integerValue] % 2 == 0;
}];
NSLog(@"Premier nombre non divisible par 2 : %@", premierNombreNonDivisiblePar2);
```