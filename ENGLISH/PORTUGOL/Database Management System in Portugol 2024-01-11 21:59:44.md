```portugol
program Complex_Code_in_Portugol;
const
  MAX_SIZE = 100;  { Maximum size of arrays and strings }
  MAX_RECORDS = 50; { Maximum number of records in a database }

type
  RecordType = record
    Name: string[MAX_SIZE];
    Age: integer;
    Occupation: string[MAX_SIZE];
  end;

var
  Database: array[1..MAX_RECORDS] of RecordType;
  NumRecords: integer;

procedure InitializeDatabase;
var
  i: integer;
begin
  for i := 1 to MAX_RECORDS do
  begin
    Database[i].Name := '';
    Database[i].Age := 0;
    Database[i].Occupation := '';
  end;

  NumRecords := 0;
end;

procedure AddRecord;
var
  Name, Occupation: string;
  Age: integer;
begin
  Write('Enter name: ');
  Readln(Name);

  Write('Enter age: ');
  Readln(Age);

  Write('Enter occupation: ');
  Readln(Occupation);

  NumRecords := NumRecords + 1;
  Database[NumRecords].Name := Name;
  Database[NumRecords].Age := Age;
  Database[NumRecords].Occupation := Occupation;
end;

procedure DisplayDatabase;
var
  i: integer;
begin
  for i := 1 to NumRecords do
  begin
    Writeln('Name: ', Database[i].Name);
    Writeln('Age: ', Database[i].Age);
    Writeln('Occupation: ', Database[i].Occupation);
  end;
end;

procedure SearchDatabase;
var
  Name: string;
  Found: boolean;
  i: integer;
begin
  Write('Enter name to search for: ');
  Readln(Name);

  Found := false;
  i := 1;
  while (i <= NumRecords) and (not Found) do
  begin
    if Database[i].Name = Name then
    begin
      Found := true;
      Writeln('Record found:');
      Writeln('Name: ', Database[i].Name);
      Writeln('Age: ', Database[i].Age);
      Writeln('Occupation: ', Database[i].Occupation);
    end;

    i := i + 1;
  end;

  if not Found then
  begin
    Writeln('Record not found.');
  end;
end;

begin
  InitializeDatabase;

  repeat
    Writeln('1. Add record');
    Writeln('2. Display database');
    Writeln('3. Search database');
    Writeln('0. Exit');
    Write('Enter choice: ');
    Readln(Choice);

    case Choice of
      1: AddRecord;
      2: DisplayDatabase;
      3: SearchDatabase;
      0: Exit;
    end;
  until Choice = 0;
end.
```

This complex code in Portugol is a database management system that allows the user to add, display, and search records.

The program first initializes the database with an array of empty records and a variable to keep track of the number of records in the database.

The user is then presented with a menu of options:

* Add record: This option allows the user to enter information for a new record, such as name, age, and occupation. The record is then added to the database.
* Display database: This option displays all of the records in the database.
* Search database: This option allows the user to search for a record by name. If the record is found, its information is displayed.
* Exit: This option exits the program.

The user can select an option by entering the corresponding number. The program then performs the selected action.

This code demonstrates the use of arrays, records, loops, and conditional statements in Portugol. It also shows how to create a simple user interface with a menu of options.