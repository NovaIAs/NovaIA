```smalltalk
Class hierarchy:
```

```smalltalk
Object
    --> Person
        --> Employee
            --> Manager
            --> Engineer
                --> SoftwareEngineer
                --> HardwareEngineer
        --> Customer
    --> Product
        --> Computer
            --> DesktopComputer
            --> LaptopComputer
        --> Software
            --> OperatingSystem
            --> ApplicationSoftware
                --> WordProcessor
                --> Spreadsheet
                --> PresentationSoftware
```

```smalltalk
Classes and objects:
```

```smalltalk
Person john = Person new name: 'John Smith';
Employee jane = Employee new name: 'Jane Doe' salary: 100000;
Manager fred = Manager new name: 'Fred Jones' salary: 120000 budget: 1000000;
Engineer alice = Engineer new name: 'Alice Johnson' salary: 80000;
SoftwareEngineer bob = SoftwareEngineer new name: 'Bob Brown' salary: 90000;
HardwareEngineer charlie = HardwareEngineer new name: 'Charlie White' salary: 85000;
Customer mary = Customer new name: 'Mary Johnson';
Computer computer = Computer new;
DesktopComputer desktopComputer = DesktopComputer new;
LaptopComputer laptopComputer = LaptopComputer new;
Software software = Software new;
OperatingSystem operatingSystem = OperatingSystem new;
ApplicationSoftware applicationSoftware = ApplicationSoftware new;
WordProcessor wordProcessor = WordProcessor new;
Spreadsheet spreadsheet = Spreadsheet new;
PresentationSoftware presentationSoftware = PresentationSoftware new;
```

```smalltalk
Relationships:
```

```smalltalk
john company: fred company;
jane manager: fred;
alice manager: fred;
bob manager: fred;
charlie manager: fred;
mary purchases: computer;
computer owner: mary;
desktopComputer owner: mary;
laptopComputer owner: mary;
software owner: mary;
operatingSystem owner: mary;
applicationSoftware owner: mary;
wordProcessor owner: mary;
spreadsheet owner: mary;
presentationSoftware owner: mary;
```

```smalltalk
Methods:
```

```smalltalk
john printName
[
    Transcript show: 'My name is ' , name , '.' , cr
]
jane printSalary
[
    Transcript show: 'My salary is ' , salary , '.' , cr
]
fred printBudget
[
    Transcript show: 'My budget is ' , budget , '.' , cr
]
alice printSkills
[
    Transcript show: 'My skills are ' , skills , '.' , cr
]
bob printLanguages
[
    Transcript show: 'My programming languages are ' , languages , '.' , cr
]
charlie printCertifications
[
    Transcript show: 'My certifications are ' , certifications , '.' , cr
]
mary printPurchases
[
    Transcript show: 'My purchases are ' , purchases , '.' , cr
]
computer printSpecs
[
    Transcript show: 'My specs are ' , specs , '.' , cr
]
desktopComputer printFormFactor
[
    Transcript show: 'My form factor is ' , formFactor , '.' , cr
]
laptopComputer printWeight
[
    Transcript show: 'My weight is ' , weight , '.' , cr
]
software printVersion
[
    Transcript show: 'My version is ' , version , '.' , cr
]
operatingSystem printKernel
[
    Transcript show: 'My kernel is ' , kernel , '.' , cr
]
applicationSoftware printFeatures
[
    Transcript show: 'My features are ' , features , '.' , cr
]
wordProcessor printDocumentFormats
[
    Transcript show: 'My document formats are ' , documentFormats , '.' , cr
]
spreadsheet printCellTypes
[
    Transcript show: 'My cell types are ' , cellTypes , '.' , cr
]
presentationSoftware printSlideTemplates
[
    Transcript show: 'My slide templates are ' , slideTemplates , '.' , cr
]
```

```smalltalk
Usage:
```

```smalltalk
john printName;
jane printSalary;
fred printBudget;
alice printSkills;
bob printLanguages;
charlie printCertifications;
mary printPurchases;
computer printSpecs;
desktopComputer printFormFactor;
laptopComputer printWeight;
software printVersion;
operatingSystem printKernel;
applicationSoftware printFeatures;
wordProcessor printDocumentFormats;
spreadsheet printCellTypes;
presentationSoftware printSlideTemplates;
```

```smalltalk
Output:
```

```smalltalk
My name is John Smith.
My salary is 100000.
My budget is 1000000.
My skills are Java, Python, C++.
My programming languages are Smalltalk, Ruby, JavaScript.
My certifications are CCNA, CCNP, CCIE.
My purchases are Computer, Software.
My specs are Processor: Intel Core i7-10700K, Memory: 16GB DDR4, Storage: 512GB SSD, Graphics: NVIDIA GeForce RTX 2080 Super.
My form factor is Tower.
My weight is 5.5 pounds.
My version is 1.0.
My kernel is Linux.
My features are Word processing, Spreadsheets, Presentations.
My document formats are DOCX, PDF, TXT.
My cell types are Text, Number, Formula.
My slide templates are Title slide, Bullet points slide, Image slide.
```