```objective-c
// Class Definition: Person
@interface Person : NSObject

// Properties
@property NSString *name;
@property NSInteger age;
@property NSString *occupation;

// Initializer
- (instancetype)initWithName:(NSString *)name age:(NSInteger)age occupation:(NSString *)occupation;

// Methods
- (void)introduceSelf;

@end

// Implementation: Person
@implementation Person

// Initializer Implementation
- (instancetype)initWithName:(NSString *)name age:(NSInteger)age occupation:(NSString *)occupation {
    if (self = [super init]) {
        _name = name;
        _age = age;
        _occupation = occupation;
    }
    return self;
}

// Method Implementation: introduceSelf
- (void)introduceSelf {
    NSLog(@"Hello, my name is %@. I am %ld years old and I work as a %@.", _name, _age, _occupation);
}

@end

// Class Definition: Student
@interface Student : Person

// Properties
@property NSString *school;
@property NSInteger grade;

// Initializer
- (instancetype)initWithName:(NSString *)name age:(NSInteger)age occupation:(NSString *)occupation school:(NSString *)school grade:(NSInteger)grade;

// Methods
- (void)study;

@end

// Implementation: Student
@implementation Student

// Initializer Implementation
- (instancetype)initWithName:(NSString *)name age:(NSInteger)age occupation:(NSString *)occupation school:(NSString *)school grade:(NSInteger)grade {
    if (self = [super initWithName:name age:age occupation:occupation]) {
        _school = school;
        _grade = grade;
    }
    return self;
}

// Method Implementation: study
- (void)study {
    NSLog(@"I am studying hard to get good grades.");
}

@end

// Class Definition: Teacher
@interface Teacher : Person

// Properties
@property NSString *subject;
@property NSString *school;

// Initializer
- (instancetype)initWithName:(NSString *)name age:(NSInteger)age occupation:(NSString *)occupation subject:(NSString *)subject school:(NSString *)school;

// Methods
- (void)teach;

@end

// Implementation: Teacher
@implementation Teacher

// Initializer Implementation
- (instancetype)initWithName:(NSString *)name age:(NSInteger)age occupation:(NSString *)occupation subject:(NSString *)subject school:(NSString *)school {
    if (self = [super initWithName:name age:age occupation:occupation]) {
        _subject = subject;
        _school = school;
    }
    return self;
}

// Method Implementation: teach
- (void)teach {
    NSLog(@"I am teaching %@ to my students.", _subject);
}

@end

// Class Definition: School
@interface School : NSObject

// Properties
@property NSString *name;
@property NSArray<Student *> *students;
@property NSArray<Teacher *> *teachers;

// Initializer
- (instancetype)initWithName:(NSString *)name students:(NSArray<Student *> *)students teachers:(NSArray<Teacher *> *)teachers;

// Methods
- (void)startSchoolDay;
- (void)endSchoolDay;

@end

// Implementation: School
@implementation School

// Initializer Implementation
- (instancetype)initWithName:(NSString *)name students:(NSArray<Student *> *)students teachers:(NSArray<Teacher *> *)teachers {
    if (self = [super init]) {
        _name = name;
        _students = students;
        _teachers = teachers;
    }
    return self;
}

// Method Implementation: startSchoolDay
- (void)startSchoolDay {
    NSLog(@"The school day is starting at %@", _name);
    for (Student *student in _students) {
        NSLog(@"%@ is entering the school.", student.name);
    }
    for (Teacher *teacher in _teachers) {
        NSLog(@"%@ is entering the school.", teacher.name);
    }
}

// Method Implementation: endSchoolDay
- (void)endSchoolDay {
    NSLog(@"The school day is ending at %@", _name);
    for (Student *student in _students) {
        NSLog(@"%@ is leaving the school.", student.name);
    }
    for (Teacher *teacher in _teachers) {
        NSLog(@"%@ is leaving the school.", teacher.name);
    }
}

@end

// Main Function
int main(int argc, const char * argv[]) {
    // Create a school
    School *school = [[School alloc] initWithName:@"Springfield Elementary"
                                         students:@[[[Student alloc] initWithName:@"Bart Simpson" age:10 occupation:@"Student" school:@"Springfield Elementary" grade:4],
                                                   [[Student alloc] initWithName:@"Lisa Simpson" age:8 occupation:@"Student" school:@"Springfield Elementary" grade:2],
                                                   [[Student alloc] initWithName:@"Nelson Muntz" age:11 occupation:@"Student" school:@"Springfield Elementary" grade:5],
                                                   [[Student alloc] initWithName:@"Ralph Wiggum" age:9 occupation:@"Student" school:@"Springfield Elementary" grade:3]]
                                         teachers:@[[[Teacher alloc] initWithName:@"Homer Simpson" age:39 occupation:@"Teacher" subject:@"Math" school:@"Springfield Elementary"],
                                                   [[Teacher alloc] initWithName:@"Marge Simpson" age:38 occupation:@"Teacher" subject:@"Science" school:@"Springfield Elementary"],
                                                   [[Teacher alloc] initWithName:@"Edna Krabappel" age:45 occupation:@"Teacher" subject:@"English" school:@"Springfield Elementary"],
                                                   [[Teacher alloc] initWithName:@"Seymour Skinner" age:52 occupation:@"Principal" subject:@"Administration" school:@"Springfield Elementary"]]];

    // Start the school day
    [school startSchoolDay];

    // Teach a few classes
    Teacher *homer = school.teachers[0];
    [homer teach];
    Teacher *marge = school.teachers[1];
    [marge teach];

    // Study a bit
    Student *bart = school.students[0];
    [bart study];
    Student *lisa = school.students[1];
    [lisa study];

    // End the school day
    [school endSchoolDay];

    return 0;
}
```

Explanation:

1. **Class Definitions**: `Person`, `Student`, `Teacher`, and `School` are defined as Objective-C classes.

2. **Properties**: Each class has properties that represent the attributes of its instances.

3. **Initializers**: Each class has an initializer method that initializes its instances with the provided values.

4. **Methods**: Each class has methods that define the behavior of its instances.

5. **Inheritance**: `Student` and `Teacher` inherit from `Person`, inheriting its properties and methods.

6. **School Class**: `School` manages a collection of students and teachers, representing a school environment.

7. **Main Function**: The `main` function serves as the entry point of the program.

8. **School Creation**: A school object is created with students and teachers.

9. **School Day**: The school day is started and ended, simulating the school routine.

10. **Teaching and Studying**: Teachers teach, and students study, demonstrating the dynamic behavior of the objects.

This code showcases object-oriented programming concepts like inheritance, encapsulation, and polymorphism in Objective-C.