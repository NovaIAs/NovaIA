Here's a complex and differentiated code in Objective-C that you may find challenging:

```objective-c
#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>

@interface Employee : NSManagedObject
@property (nonatomic, retain) NSString *name;
@property (nonatomic, retain) NSNumber *salary;
@property (nonatomic, retain) NSDate *hireDate;
@end

@implementation Employee
@end

@interface Department : NSManagedObject
@property (nonatomic, retain) NSString *name;
@property (nonatomic, retain) NSSet *employees;
@end

@implementation Department
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Create a managed object context
        NSManagedObjectContext *context = [[NSManagedObjectContext alloc] initWithConcurrencyType:NSMainQueueConcurrencyType];
        
        // Create a managed object model
        NSManagedObjectModel *model = [[NSManagedObjectModel alloc] init];
        
        // Define the entities
        NSEntityDescription *employeeEntity = [[NSEntityDescription alloc] init];
        [employeeEntity setName:@"Employee"];
        [employeeEntity setManagedObjectClassName:@"Employee"];
        
        NSAttributeDescription *nameAttribute = [[NSAttributeDescription alloc] init];
        [nameAttribute setName:@"name"];
        [nameAttribute setAttributeType:NSStringAttributeType];
        
        NSAttributeDescription *salaryAttribute = [[NSAttributeDescription alloc] init];
        [salaryAttribute setName:@"salary"];
        [salaryAttribute setAttributeType:NSDecimalAttributeType];
        
        NSAttributeDescription *hireDateAttribute = [[NSAttributeDescription alloc] init];
        [hireDateAttribute setName:@"hireDate"];
        [hireDateAttribute setAttributeType:NSDateAttributeType];
        
        [employeeEntity setProperties:@[nameAttribute, salaryAttribute, hireDateAttribute]];
        
        NSEntityDescription *departmentEntity = [[NSEntityDescription alloc] init];
        [departmentEntity setName:@"Department"];
        [departmentEntity setManagedObjectClassName:@"Department"];
        
        NSAttributeDescription *departmentNameAttribute = [[NSAttributeDescription alloc] init];
        [departmentNameAttribute setName:@"name"];
        [departmentNameAttribute setAttributeType:NSStringAttributeType];
        
        NSRelationshipDescription *employeesRelationship = [[NSRelationshipDescription alloc] init];
        [employeesRelationship setName:@"employees"];
        [employeesRelationship setDestinationEntity:employeeEntity];
        [employeesRelationship setCardinality:NSMinZeroMaxMany];
        
        [departmentEntity setProperties:@[departmentNameAttribute, employeesRelationship]];
        
        // Add the entities to the model
        [model setEntities:@[employeeEntity, departmentEntity]];
        
        // Create a persistent store coordinator
        NSPersistentStoreCoordinator *coordinator = [[NSPersistentStoreCoordinator alloc] initWithManagedObjectModel:model];
        
        // Add a persistent store
        NSURL *storeURL = [[NSFileManager defaultManager] URLForUbiquityContainerIdentifier:nil];
        storeURL = [storeURL URLByAppendingPathComponent:@"Employee.sqlite"];
        NSError *error = nil;
        NSPersistentStore *store = [coordinator addPersistentStoreWithType:NSSQLiteStoreType configuration:nil URL:storeURL options:nil error:&error];
        
        if (error) {
            NSLog(@"Error creating persistent store: %@", error);
            return 1;
        }
        
        // Set the context's coordinator
        [context setPersistentStoreCoordinator:coordinator];
        
        // Create a fetch request
        NSFetchRequest *request = [[NSFetchRequest alloc] init];
        [request setEntity:employeeEntity];
        
        // Execute the fetch request
        NSError *fetchError = nil;
        NSArray *employees = [context executeFetchRequest:request error:&fetchError];
        
        if (fetchError) {
            NSLog(@"Error fetching employees: %@", fetchError);
            return 1;
        }
        
        // Print the employee names
        for (Employee *employee in employees) {
            NSLog(@"Employee: %@\nSalary: %@\nHire Date: %@", employee.name, employee.salary, employee.hireDate);
        }
        
        // Create a new employee
        Employee *newEmployee = [NSEntityDescription insertNewObjectForEntityForName:@"Employee" inManagedObjectContext:context];
        [newEmployee setName:@"John Smith"];
        [newEmployee setSalary:@100000];
        [newEmployee setHireDate:[NSDate date]];
        
        // Save the context
        NSError *saveError = nil;
        if ([context save:&saveError]) {
            NSLog(@"Saved successfully");
        } else {
            NSLog(@"Error saving: %@", saveError);
            return 1;
        }
    }
    return 0;
}
```

This code demonstrates a complex and differentiated use of Core Data, Apple's framework for managing persistent data. A managed object model is defined, along with associated entities (Employee and Department) and their properties. A managed object context is created and used to perform fetch and save operations. A new employee is created and saved to the persistent store. The code includes error handling and logging to assist with debugging.

The key points of this code are:

* It uses Core Data to manage persistent data.
* It defines a managed object model with two entities (Employee and Department) and their properties.
* It creates a managed object context and uses it to perform fetch and save operations.
* It creates and saves a new employee to the persistent store.
* It includes error handling and logging to assist with debugging.

This code is relatively complex and may require some knowledge of Core Data to understand fully. However, it demonstrates a sophisticated use of the framework and can be used as a starting point for building more complex data management applications.