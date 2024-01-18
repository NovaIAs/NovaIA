```modula-2
MODULE PetHotel;
FROM Terminal IMPORT WriteString, WriteLn, ReadChar;

TYPE Room = RECORD
    number: INTEGER;
    name: ARRAY[0..15] OF CHAR;
    size: CHAR;
    price: REAL;
);

VAR
    rooms: ARRAY[1..10] OF Room;
    numRooms: INTEGER;

PROCEDURE InitializeRooms;
VAR
    i: INTEGER;
BEGIN
    numRooms := 0;
    rooms[1].number := 101;
    rooms[1].name := "Cozy Corner";
    rooms[1].size := 'S';
    rooms[1].price := 20.00;
    rooms[2].number := 102;
    rooms[2].name := "Happy Hound";
    rooms[2].size := 'M';
    rooms[2].price := 30.00;
    rooms[3].number := 103;
    rooms[3].name := "Frisky Feline";
    rooms[3].size := 'S';
    rooms[3].price := 25.00;
    rooms[4].number := 104;
    rooms[4].name := "Playful Pup";
    rooms[4].size := 'M';
    rooms[4].price := 35.00;
    rooms[5].number := 105;
    rooms[5].name := "Purrfect Palace";
    rooms[5].size := 'L';
    rooms[5].price := 40.00;
    rooms[6].number := 106;
    rooms[6].name := "Tail-Wagging Retreat";
    rooms[6].size := 'L';
    rooms[6].price := 45.00;
    rooms[7].number := 107;
    rooms[7].name := "Woof-tastic Villa";
    rooms[7].size := 'XL';
    rooms[7].price := 50.00;
    rooms[8].number := 108;
    rooms[8].name := "Meow Manor";
    rooms[8].size := 'XL';
    rooms[8].price := 55.00;
    rooms[9].number := 109;
    rooms[9].name := "Pet Paradise";
    rooms[9].size := 'XXL';
    rooms[9].price := 60.00;
    rooms[10].number := 110;
    rooms[10].name := "Animal Sanctuary";
    rooms[10].size := 'XXL';
    rooms[10].price := 65.00;
    numRooms := 10;
END InitializeRooms;

PROCEDURE ShowRooms;
VAR
    i: INTEGER;
BEGIN
    WriteString("Available Rooms:");
    WriteLn;
    FOR i := 1 TO numRooms DO
        WriteString("Room Number: ");
        WriteInt(rooms[i].number, 0);
        WriteLn;
        WriteString("Room Name: ");
        WriteString(rooms[i].name);
        WriteLn;
        WriteString("Room Size: ");
        WriteChar(rooms[i].size);
        WriteLn;
        WriteString("Room Price: ");
        WriteReal(rooms[i].price, 2, 0);
        WriteLn;
        WriteString("------------------------");
        WriteLn;
    END;
END ShowRooms;

PROCEDURE GetCustomerInfo(VAR name: ARRAY[0..15] OF CHAR; VAR petName: ARRAY[0..15] OF CHAR; VAR petType: ARRAY[0..15] OF CHAR; VAR arrivalDate: ARRAY[0..10] OF CHAR; VAR departureDate: ARRAY[0..10] OF CHAR; VAR roomNumber: INTEGER);
VAR
    choice: CHAR;
BEGIN
    WriteString("Customer Name: ");
    ReadString(name, 16);
    WriteString("Pet Name: ");
    ReadString(petName, 16);
    WriteString("Pet Type: ");
    ReadString(petType, 16);
    WriteString("Arrival Date (dd/mm/yyyy): ");
    ReadString(arrivalDate, 11);
    WriteString("Departure Date (dd/mm/yyyy): ");
    ReadString(departureDate, 11);
    WriteString("Room Number: ");
    ReadInt(roomNumber, 0);
END GetCustomerInfo;

PROCEDURE ReserveRoom(name: ARRAY[0..15] OF CHAR; petName: ARRAY[0..15] OF CHAR; petType: ARRAY[0..15] OF CHAR; arrivalDate: ARRAY[0..10] OF CHAR; departureDate: ARRAY[0..10] OF CHAR; roomNumber: INTEGER);
VAR
    roomFound: BOOLEAN;
    roomIndex: INTEGER;
BEGIN
    roomFound := FALSE;
    FOR roomIndex := 1 TO numRooms DO
        IF rooms[roomIndex].number = roomNumber THEN
            IF rooms[roomIndex].size = 'S' AND petType = 'Dog' THEN
                WriteString("Sorry, this room is too small for a dog.");
                WriteLn;
            ELSIF rooms[roomIndex].size = 'S' AND petType = 'Cat' THEN
                roomFound := TRUE;
                EXIT;
            ELSIF rooms[roomIndex].size = 'M' AND (petType = 'Dog' OR petType = 'Cat') THEN
                roomFound := TRUE;
                EXIT;
            ELSIF rooms[roomIndex].size = 'L' AND petType = 'Dog' THEN
                roomFound := TRUE;
                EXIT;
            ELSIF rooms[roomIndex].size = 'XL' AND (petType = 'Dog' OR petType = 'Cat') THEN
                roomFound := TRUE;
                EXIT;
            ELSIF rooms[roomIndex].size = 'XXL' AND petType = 'Dog' THEN
                roomFound := TRUE;
                EXIT;
            ELSE
                WriteString("Sorry, this room is not suitable for your pet.");
                WriteLn;
            END;
        END;
    END;
    IF roomFound THEN
        WriteString("Reservation Confirmed!");
        WriteLn;
        WriteString("Customer Name: ");
        WriteString(name);
        WriteLn;
        WriteString("Pet Name: ");
        WriteString(petName);
        WriteLn;
        WriteString("Pet Type: ");
        WriteString(petType);
        WriteLn;
