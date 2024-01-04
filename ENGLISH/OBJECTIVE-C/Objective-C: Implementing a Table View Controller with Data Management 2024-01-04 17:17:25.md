```objective-c
#import <Foundation/Foundation.h>

// Define a complex data structure to represent a person.
@interface Person : NSObject

@property (nonatomic, copy) NSString *name;
@property (nonatomic, assign) NSInteger age;
@property (nonatomic, strong) NSArray *hobbies;

@end

@implementation Person

- (instancetype)initWithName:(NSString *)name age:(NSInteger)age hobbies:(NSArray *)hobbies {
    self = [super init];
    if (self) {
        _name = [name copy];
        _age = age;
        _hobbies = [hobbies copy];
    }
    return self;
}

@end

// Define a class to manage a collection of people.
@interface PeopleManager : NSObject

@property (nonatomic, strong) NSMutableArray *people;

- (void)addPerson:(Person *)person;
- (void)removePerson:(Person *)person;
- (NSArray *)getAllPeople;

@end

@implementation PeopleManager

- (instancetype)init {
    self = [super init];
    if (self) {
        _people = [NSMutableArray array];
    }
    return self;
}

- (void)addPerson:(Person *)person {
    [_people addObject:person];
}

- (void)removePerson:(Person *)person {
    [_people removeObject:person];
}

- (NSArray *)getAllPeople {
    return [_people copy];
}

@end

// Define a protocol to represent a data source for a table view.
@protocol UITableViewDataSource

- (NSInteger)numberOfRowsInSection:(NSInteger)section;
- (UITableViewCell *)cellForRowAtIndexPath:(NSIndexPath *)indexPath;

@end

// Define a class to represent a table view controller.
@interface TableViewController : UITableViewController

@property (nonatomic, strong) PeopleManager *peopleManager;

@end

@implementation TableViewController

- (instancetype)initWithPeopleManager:(PeopleManager *)peopleManager {
    self = [super init];
    if (self) {
        _peopleManager = peopleManager;
    }
    return self;
}

- (NSInteger)numberOfRowsInSection:(NSInteger)section {
    return [_peopleManager getAllPeople].count;
}

- (UITableViewCell *)cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"PersonCell" forIndexPath:indexPath];

    Person *person = [_peopleManager getAllPeople][indexPath.row];
    cell.textLabel.text = person.name;
    cell.detailTextLabel.text = [NSString stringWithFormat:@"%ld years old", person.age];

    return cell;
}

@end

// Define a class to represent an application delegate.
@interface AppDelegate : UIResponder <UIApplicationDelegate>

@property (nonatomic, strong) UIWindow *window;

@end

@implementation AppDelegate

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    // Create a window and set its root view controller.
    self.window = [[UIWindow alloc] initWithFrame:[UIScreen mainScreen].bounds];
    self.window.rootViewController = [[TableViewController alloc] initWithPeopleManager:[[PeopleManager alloc] init]];

    // Make the window visible.
    [self.window makeKeyAndVisible];

    return YES;
}

@end

// Define the main function.
int main(int argc, char *argv[]) {
    @autoreleasepool {
        return UIApplicationMain(argc, argv, nil, NSStringFromClass([AppDelegate class]));
    }
}
```

Explanation:

1. We define a `Person` class to represent a person with properties like name, age, and hobbies.

2. The `PeopleManager` class manages a collection of people, allowing us to add, remove, and retrieve people.

3. We define a `UITableViewDataSource` protocol to represent a data source for a table view.

4. The `TableViewController` class is a view controller that displays a table view of people using the `PeopleManager`.

5. The `AppDelegate` class is the application delegate that creates the application window and sets its root view controller.

6. The `main` function is the entry point of the application.

This code demonstrates various aspects of Objective-C programming, including classes, protocols, data structures, inheritance, and the MVC (Model-View-Controller) design pattern.