**Code complexe en Objective-C**

**Objectif:** Construire un moteur de recommandation basé sur le filtrage collaboratif.

**Explication du code:**

**1. Définition du modèle de données:**

```objective-c
@interface RecommandationEngine : NSObject

@property (nonatomic, strong) NSDictionary<NSNumber *, NSSet<NSNumber *> *> *userRatings;
@property (nonatomic, strong) NSDictionary<NSNumber *, NSSet<NSNumber *> *> *itemRatings;

@end
```

* `userRatings`: Dictionnaire mappant les identifiants des utilisateurs aux ensembles d'éléments notés.
* `itemRatings`: Dictionnaire mappant les identifiants des éléments aux ensembles d'utilisateurs qui les ont notés.

**2. Chargement des données:**

```objective-c
- (void)loadData {
    // Charger les données d'un fichier ou d'une base de données.
    
    for (NSDictionary *row in data) {
        NSNumber *userID = row[@"userID"];
        NSNumber *itemID = row[@"itemID"];
        NSNumber *rating = row[@"rating"];
        
        NSMutableSet *userRatedItems = [userRatings[userID] mutableCopy] ?: [NSMutableSet new];
        [userRatedItems addObject:itemID];
        userRatings[userID] = userRatedItems;
        
        NSMutableSet *itemRatedUsers = [itemRatings[itemID] mutableCopy] ?: [NSMutableSet new];
        [itemRatedUsers addObject:userID];
        itemRatings[itemID] = itemRatedUsers;
    }
}
```

* Charge les données de classement et construit les dictionnaires `userRatings` et `itemRatings`.

**3. Calcul de la similarité utilisateur-utilisateur:**

```objective-c
- (CGFloat)userSimilarity:(NSNumber *)user1ID user2ID:(NSNumber *)user2ID {
    // Calculer la similarité d'intérêt entre deux utilisateurs à l'aide du coefficient de corrélation de Pearson.
    
    NSSet<NSNumber *> *user1RatedItems = userRatings[user1ID];
    NSSet<NSNumber *> *user2RatedItems = userRatings[user2ID];
    
    CGFloat numerator = 0.0f;
    CGFloat user1Mean = 0.0f;
    CGFloat user2Mean = 0.0f;
    
    for (NSNumber *itemID in user1RatedItems) {
        CGFloat rating1 = [userRatings[user1ID] containsObject:itemID] ? [userRatings[user1ID] objectForKey:itemID].floatValue : 0.0f;
        CGFloat rating2 = [userRatings[user2ID] containsObject:itemID] ? [userRatings[user2ID] objectForKey:itemID].floatValue : 0.0f;
        
        numerator += (rating1 - user1Mean) * (rating2 - user2Mean);
        user1Mean += rating1;
        user2Mean += rating2;
    }
    
    CGFloat denominator = sqrtf(numerator * numerator + ((user1RatedItems.count - 1) * (user1Mean * user1Mean)) + ((user2RatedItems.count - 1) * (user2Mean * user2Mean)));
    
    return denominator == 0.0f ? 0.0f : numerator / denominator;
}
```

* Calcule la similarité d'intérêt entre deux utilisateurs en utilisant le coefficient de corrélation de Pearson.

**4. Calcul de la similarité élément-élément:**

```objective-c
- (CGFloat)itemSimilarity:(NSNumber *)item1ID item2ID:(NSNumber *)item2ID {
    // Calculer la similarité entre deux éléments en fonction du nombre d'utilisateurs qui les ont classés ensemble.
    
    NSSet<NSNumber *> *item1RatedUsers = itemRatings[item1ID];
    NSSet<NSNumber *> *item2RatedUsers = itemRatings[item2ID];
    
    NSSet *intersection = [item1RatedUsers intersectSet:item2RatedUsers];
    
    return intersection.count / (userRatings.count * 0.5f);
}
```

* Calcule la similarité entre deux éléments en fonction du nombre d'utilisateurs qui les ont classés ensemble.

**5. Génération de recommandations:**

```objective-c
- (NSArray<NSNumber *> *)recommendationsForUser:(NSNumber *)userID {
    // Générer une liste d'éléments recommandés pour un utilisateur.
    
    NSMutableSet *recommendedItems = [NSMutableSet new];
    NSArray<NSNumber *> *userRatedItems = userRatings[userID];
    
    for (NSNumber *itemID in userRatedItems) {
        NSSet<NSNumber *> *similarItems = [itemRatings objectForKey:itemID];
        
        for (NSNumber *item in similarItems) {
            if (![userRatedItems containsObject:item]) {
                [recommendedItems addObject:item];
            }
        }
    }
    
    return [recommendedItems sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
        NSNumber *item1 = obj1;
        NSNumber *item2 = obj2;
        
        return [item1 compare:item2];
    }];
}
```

* Génère une liste d'éléments recommandés pour un utilisateur en fonction de la similarité des éléments classés par des utilisateurs similaires.