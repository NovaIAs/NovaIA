**Classe `Modèle`**

Cette classe représente le modèle de données pour notre application. Elle contient les données et la logique métier.

```objective-c
@interface Modèle : NSObject

@property (nonatomic, strong) NSArray *éléments;

- (void)ajouterÉlément:(NSString *)élément;
- (void)supprimerÉlément:(NSString *)élément;

@end
```

**Classe `Contrôleur`**

Cette classe contrôle le comportement de l'interface utilisateur et communique avec le modèle.

```objective-c
@interface Contrôleur : UIViewController

@property (nonatomic, strong) Modèle *modèle;

- (void)viewDidLoad;
- (void)ajouterÉlémentAction:(id)sender;
- (void)supprimerÉlémentAction:(id)sender;

@end
```

**Classe `Vue`**

Cette classe représente l'interface utilisateur.

```objective-c
@interface Vue : UIView

@property (nonatomic, strong) UITableView *tableView;

- (instancetype)initWithFrame:(CGRect)frame modèle:(Modèle *)modèle;

@end
```

**Classe `Cellule`**

Cette classe représente les cellules du tableau.

```objective-c
@interface Cellule : UITableViewCell

@property (nonatomic, strong) UILabel *étiquette;

- (instancetype)initWithStyle:(UITableViewCellStyle)style reuseIdentifier:(NSString *)reuseIdentifier;

@end
```

**Implémentation des classes**

Voici la mise en œuvre des méthodes des classes :

**`Modèle`**

```objective-c
@implementation Modèle

- (instancetype)init {
    self = [super init];
    if (self) {
        _éléments = [NSArray array];
    }
    return self;
}

- (void)ajouterÉlément:(NSString *)élément {
    NSMutableArray *élémentsMutables = [_éléments mutableCopy];
    [élémentsMutables addObject:élément];
    _éléments = [élémentsMutables copy];
}

- (void)supprimerÉlément:(NSString *)élément {
    NSMutableArray *élémentsMutables = [_éléments mutableCopy];
    [élémentsMutables removeObject:élément];
    _éléments = [élémentsMutables copy];
}

@end
```

**`Contrôleur`**

```objective-c
@implementation Contrôleur

- (void)viewDidLoad {
    [super viewDidLoad];
    _modèle = [[Modèle alloc] init];
    
    _tableView = [[UITableView alloc] initWithFrame:self.view.bounds style:UITableViewStylePlain];
    _tableView.delegate = self;
    _tableView.dataSource = self;
    [self.view addSubview:_tableView];
}

- (void)ajouterÉlémentAction:(id)sender {
    [_modèle ajouterÉlément:@"Nouvel élément"];
    [_tableView reloadData];
}

- (void)supprimerÉlémentAction:(id)sender {
    NSIndexPath *indexPath = [_tableView indexPathForSelectedRow];
    [_modèle supprimerÉlément:_modèle.éléments[indexPath.row]];
    [_tableView reloadData];
}

@end
```

**`Vue`**

```objective-c
@implementation Vue

- (instancetype)initWithFrame:(CGRect)frame modèle:(Modèle *)modèle {
    self = [super initWithFrame:frame];
    if (self) {
        _modèle = modèle;
        _tableView = [[UITableView alloc] initWithFrame:self.bounds style:UITableViewStylePlain];
        _tableView.delegate = self;
        _tableView.dataSource = self;
        [self addSubview:_tableView];
    }
    return self;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return _modèle.éléments.count;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    Cellule *cellule = [tableView dequeueReusableCellWithIdentifier:@"Cellule" forIndexPath:indexPath];
    cellule.étiquette.text = _modèle.éléments[indexPath.row];
    return cellule;
}

@end
```

**`Cellule`**

```objective-c
@implementation Cellule

- (instancetype)initWithStyle:(UITableViewCellStyle)style reuseIdentifier:(NSString *)reuseIdentifier {
    self = [super initWithStyle:style reuseIdentifier:reuseIdentifier];
    if (self) {
        _étiquette = [[UILabel alloc] initWithFrame:self.contentView.bounds];
        _étiquette.textAlignment = NSTextAlignmentCenter;
        _étiquette.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
        [self.contentView addSubview:_étiquette];
    }
    return self;
}

@end
```

**Délégués de la table**

```objective-c
@implementation Contrôleur

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return _modèle.éléments.count;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    Cellule *cellule = [tableView dequeueReusableCellWithIdentifier:@"Cellule" forIndexPath:indexPath];
    cellule.étiquette.text = _modèle.éléments[indexPath.row];
    return cellule;
}

@end
```