```
System: Hotel Management System

Actors:
1. Guest
2. Hotel Staff (Receptionist, Manager, Housekeeping, Maintenance)
3. System Administrator

Use Cases:
1. Guest:
   - Book a Room
   - Check In
   - Check Out
   - Request Room Service
   - Make a Complaint

2. Hotel Staff:
   - Receptionist:
     - Manage Guest Check-In/Check-Out
     - Handle Guest Inquiries
     - Manage Room Assignments
   - Manager:
     - Manage Hotel Operations
     - Set Room Rates
     - Track Hotel Revenue
   - Housekeeping:
     - Clean and Maintain Guest Rooms
     - Replenish Room Amenities
   - Maintenance:
     - Repair and Maintain Hotel Equipment
     - Address Guest Maintenance Requests

3. System Administrator:
   - Manage User Accounts
   - Configure System Settings
   - Monitor System Performance

Classes:
1. Guest:
   - Attributes:
     - Guest ID
     - Name
     - Address
     - Contact Information
     - Room Number
     - Check-In Date
     - Check-Out Date
   - Methods:
     - Book Room()
     - Check In()
     - Check Out()
     - Request Room Service()
     - Make Complaint()

2. Hotel Staff:
   - Receptionist:
     - Attributes:
       - Employee ID
       - Name
       - Role
       - Shift
     - Methods:
       - Manage Guest Check-In/Check-Out
       - Handle Guest Inquiries
       - Manage Room Assignments
   - Manager:
     - Attributes:
       - Employee ID
       - Name
       - Role
       - Years of Service
     - Methods:
       - Manage Hotel Operations
       - Set Room Rates
       - Track Hotel Revenue
   - Housekeeping:
     - Attributes:
       - Employee ID
       - Name
       - Role
       - Shift
     - Methods:
       - Clean and Maintain Guest Rooms
       - Replenish Room Amenities
   - Maintenance:
     - Attributes:
       - Employee ID
       - Name
       - Role
       - Shift
     - Methods:
       - Repair and Maintain Hotel Equipment
       - Address Guest Maintenance Requests

3. System Administrator:
   - Attributes:
     - Administrator ID
     - Name
     - Role
     - System Access Level
   - Methods:
     - Manage User Accounts
     - Configure System Settings
     - Monitor System Performance

Sequence Diagrams:
1. Guest Booking a Room:
   - Guest accesses the hotel website or contacts the hotel directly.
   - Guest selects the desired room type, check-in date, check-out date, and number of guests.
   - Guest provides personal information and payment details.
   - Hotel system checks for room availability and processes the booking.
   - Confirmation email is sent to the guest.

2. Guest Checking In:
   - Guest arrives at the hotel and approaches the reception desk.
   - Receptionist verifies the guest's identity and reservation details.
   - Receptionist assigns a room key and provides the guest with check-in instructions.
   - Guest goes to the assigned room and settles in.

3. Guest Checking Out:
   - Guest approaches the reception desk.
   - Receptionist verifies the guest's identity and room number.
   - Receptionist prepares the guest's bill and collects payment.
   - Receptionist issues a checkout receipt to the guest.
   - Guest returns the room key and departs from the hotel.

4. Guest Requesting Room Service:
   - Guest dials the room service number or uses the in-room dining menu.
   - Guest selects the desired items from the menu.
   - Room service staff delivers the ordered items to the guest's room.
   - Guest enjoys the room service.

5. Guest Making a Complaint:
   - Guest contacts the reception desk or the hotel manager.
   - Guest explains the complaint and provides any necessary details.
   - Hotel staff investigates the complaint and takes appropriate action to resolve the issue.
   - Guest is informed of the resolution and any compensation offered by the hotel.

Database Schema:
1. Guest Table:
   - Columns:
     - Guest ID (Primary Key)
     - Name
     - Address
     - Contact Information
   - Constraints:
     - Guest ID is a unique identifier for each guest.

2. Room Table:
   - Columns:
     - Room Number (Primary Key)
     - Room Type
     - Max Occupancy
     - Amenities
     - Price
   - Constraints:
     - Room Number is a unique identifier for each room.

3. Reservation Table:
   - Columns:
     - Reservation ID (Primary Key)
     - Guest ID
     - Room Number
     - Check-In Date
     - Check-Out Date
     - Number of Guests
     - Total Cost
   - Constraints:
     - Reservation ID is a unique identifier for each reservation.

4. Hotel Staff Table:
   - Columns:
     - Employee ID (Primary Key)
     - Name
     - Role
     - Shift
     - Years of Service
   - Constraints:
     - Employee ID is a unique identifier for each hotel staff member.

5. System Administrator Table:
   - Columns:
     - Administrator ID (Primary Key)
     - Name
     - Role
     - System Access Level
   - Constraints:
     - Administrator ID is a unique identifier for each system administrator.

6. Room Service Table:
   - Columns:
     - Order ID (Primary Key)
     - Guest ID
     - Room Number
     - Order Date
     - Order Time
     - Items Ordered
     - Total Price
   - Constraints:
     - Order ID is a unique identifier for each room service order.

7. Complaint Table:
   - Columns:
     - Complaint ID (Primary Key)
     - Guest ID
     - Room Number
     - Complaint Date
     - Complaint Time
     - Complaint Description
     - Resolution
   - Constraints:
     - Complaint ID is a unique identifier for each guest complaint.

The hotel management system uses this UML diagram and various components to manage the hotel's operations effectively, providing a seamless experience for guests and ensuring efficient operations for the hotel staff.
```