```objective-c
// Import required frameworks
#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>

// Define the core data stack
NSManagedObjectModel *managedObjectModel;
NSPersistentStoreCoordinator *persistentStoreCoordinator;
NSManagedObjectContext *managedObjectContext;

// Define the entity and its attributes
NSEntityDescription *entityDescription = [NSEntityDescription entityForName:@"Employee" inManagedObjectContext:managedObjectContext];
[entityDescription addAttributeWithName:@"name" type:NSStringAttributeType attributeValueClassName:nil];
[entityDescription addAttributeWithName:@"age" type:NSInteger64AttributeType attributeValueClassName:nil];

// Create the fetch request
NSFetchRequest *fetchRequest = [[NSFetchRequest alloc] init];
[fetchRequest setEntity:entityDescription];

//Execute the fetch request
NSError *error = nil;
NSArray *results = [managedObjectContext executeFetchRequest:fetchRequest error:&error];

//Process the results
for (NSManagedObject *employee in results) {
    NSLog(@"Name: %@", [employee valueForKey:@"name"]);
    NSLog(@"Age: %@", [employee valueForKey:@"age"]);
}

//Code to handle errors
if (error) {
    NSLog(@"Error: %@", error);
}

// Save changes to the managed object context
[managedObjectContext save:&error];

//Handle errors
if (error) {
    NSLog(@"Error: %@", error);
}
```

Explanation:

1. We start by importing the necessary frameworks.

2. We define the core data stack which includes the managed object model, persistent store coordinator, and managed object context.

3. We define an entity called "Employee" and add attributes like name and age.

4. We create a fetch request to retrieve all employees from the managed object context.

5. We execute the fetch request and handle any errors that may occur.

6. We process the results of the fetch request by printing the name and age of each employee.

7. Finally, we save changes to the managed object context and handle any errors that may occur.