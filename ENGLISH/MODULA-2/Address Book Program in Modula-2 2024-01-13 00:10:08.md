```modula-2
MODULE Main;
FROM Terminal IMPORT WriteString, WriteLn, ReadChar;
FROM Strings IMPORT Concat, SubString, Length;
FROM Formats IMPORT Int;

TYPE

  -- Define a record type to store information about a person
  Person = RECORD
    name: ARRAY[0..80] OF CHAR;
    age: CARDINAL;
    address: ARRAY[0..80] OF CHAR;
  END;

VAR

  -- Define an array of Person records to store a list of people
  people: ARRAY[0..99] OF Person;

  -- Define a variable to keep track of the number of people in the list
  numPeople: CARDINAL;

PROCEDURE GetPersonInfo(VAR person: Person);
  -- Get information about a person from the user
  -- person: The Person record to store the information in
BEGIN
    WriteString("Enter the person's name: ");
    ReadString(person.name);

    WriteString("Enter the person's age: ");
    ReadCard(person.age);

    WriteString("Enter the person's address: ");
    ReadString(person.address);
  END GetPersonInfo;

PROCEDURE AddPerson(person: Person);
  -- Add a new person to the list of people
  -- person: The Person record to add
BEGIN
  IF numPeople < 100 THEN
    people[numPeople] := person;
    numPeople := numPeople + 1;
  ELSE
    WriteString("The list of people is full.");
  END;
END AddPerson;

PROCEDURE PrintPeople;
  -- Print the list of people
BEGIN
  FOR i := 0 TO numPeople - 1 DO
    WriteString(Concat(Concat("Person ", Int(i + 1, 2)), ": "));
    WriteString(Concat(Concat(Concat(people[i].name, ", "), Int(people[i].age, 2)), " years old, "));
    WriteString(people[i].address);
    WriteLn;
  END;
END PrintPeople;

PROCEDURE FindPersonByName(name: ARRAY OF CHAR): CARDINAL;
  -- Find the index of a person in the list by their name
  -- name: The name of the person to search for
  VAR
    i: CARDINAL;
  BEGIN
    FOR i := 0 TO numPeople - 1 DO
      IF name = people[i].name THEN
        RETURN i;
      END;
    END;

    RETURN -1;
  END FindPersonByName;

PROCEDURE DeletePerson(index: CARDINAL);
  -- Delete a person from the list by their index
  -- index: The index of the person to delete
BEGIN
  FOR i := index TO numPeople - 2 DO
    people[i] := people[i + 1];
  END;

  numPeople := numPeople - 1;
END DeletePerson;

PROCEDURE UpdatePerson(index: CARDINAL);
  -- Update the information about a person in the list by their index
  -- index: The index of the person to update
BEGIN
  VAR
    person: Person;
  BEGIN
    GetPersonInfo(person);
    people[index] := person;
  END;
END UpdatePerson;

PROCEDURE PrintMenu;
  -- Print the menu of options for the user
BEGIN
  WriteString("1. Add a person");
  WriteLn;
  WriteString("2. Print the list of people");
  WriteLn;
  WriteString("3. Find a person by name");
  WriteLn;
  WriteString("4. Delete a person");
  WriteLn;
  WriteString("5. Update a person");
  WriteLn;
  WriteString("6. Quit");
  WriteLn;
END PrintMenu;

PROCEDURE GetChoice: CARDINAL;
  -- Get the user's choice from the menu
BEGIN
  VAR
    choice: CHAR;
  BEGIN
    WriteString("Enter your choice: ");
    ReadChar(choice);
    RETURN ORD(choice) - ORD('0');
  END;
END GetChoice;

BEGIN
  numPeople := 0;

  LOOP
    PrintMenu;
    CASE GetChoice OF
      1:
        VAR
          person: Person;
        BEGIN
          GetPersonInfo(person);
          AddPerson(person);
        END;
      2:
        PrintPeople;
      3:
        VAR
          name: ARRAY[0..80] OF CHAR;
        BEGIN
          WriteString("Enter the name of the person to search for: ");
          ReadString(name);
          VAR
            index: CARDINAL;
          BEGIN
            index := FindPersonByName(name);
            IF index = -1 THEN
              WriteString("Person not found.");
            ELSE
              WriteString(Concat(Concat("Person found at index ", Int(index, 2)), ": "));
              WriteString(Concat(Concat(Concat(people[index].name, ", "), Int(people[index].age, 2)), " years old, "));
              WriteString(people[index].address);
              WriteLn;
            END;
          END;
        END;
      4:
        VAR
          index: CARDINAL;
        BEGIN
          WriteString("Enter the index of the person to delete: ");
          ReadCard(index);
          DeletePerson(index);
        END;
      5:
        VAR
          index: CARDINAL;
        BEGIN
          WriteString("Enter the index of the person to update: ");
          ReadCard(index);
          UpdatePerson(index);
        END;
      6:
        EXIT;
    END;
  END;
END Main.
```

This code is a simple address book program written in Modula-2. It allows the user to add, print, find, delete, and update people in the address book. The program uses a menu-driven interface to allow the user to easily interact with the program.

The program uses a number of Modula-2 features, including:

* **Records:** The Person record type is used to store information about a person.
* **Arrays:** The people array is used to store a list of people.
* **Procedures:** The program uses a number of procedures to perform different tasks, such as getting information about a person, adding a person to the list, printing the list of people, and finding a person by name.
* **Control structures:** The program uses a loop to allow the user to repeatedly interact with the program. The program also uses a case statement to allow the user to select different options from the menu.
* **Input and output:** The program uses the Terminal module to get input from the user and to print output to the console.

The program is well-commented and easy to understand. It is a good example of how to use Modula-2 to write a simple program.