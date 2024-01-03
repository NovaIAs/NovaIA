Certainly! Here's a complex and differentiated code written in ADA that implements a multi-threaded application using synchronized tasks to simulate a distributed system. This code demonstrates the concept of message passing and synchronization between concurrent tasks.

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Task_Identification; use Ada.Task_Identification;

procedure Distributed_System is

   -- Shared message buffer
   type Message_Buffer is limited private;
   type Message_Ptr is access Message_Buffer;
   type Message_Buffer is record
      Message : String(1 .. 100);
      Next    : Message_Ptr;
   end record;

   -- Task type representing each node in the distributed system
   task type Node is
      entry Send(Message : in String);
      entry Receive(Message : out String);
   end Node;

   -- Implementation of the shared message buffer
   type Message_Buffer is record
      Message : String(1 .. 100);
      Next    : Message_Ptr;
   end record;

   -- Task body for each node
   task body Node is
      Buffer : Message_Buffer; -- Local buffer for each node
   begin
      loop
         accept Send(Message : in String) do
            -- Add message to the shared buffer
            declare
               New_Message : Message_Ptr := new Message_Buffer;
            begin
               New_Message.Message := Message;
               New_Message.Next    := Buffer.Next;
               Buffer.Next         := New_Message;
            end;
         end Send;

         accept Receive(Message : out String) do
            -- Retrieve message from the shared buffer
            declare
               Current_Message : Message_Ptr := Buffer.Next;
            begin
               if Current_Message /= null then
                  Message := Current_Message.Message;
                  Buffer.Next := Current_Message.Next;
                  deallocate(Current_Message);
               end if;
            end;
         end Receive;

         -- Simulate some processing time
         delay Duration(milliseconds => 100);
      end loop;
   end Node;

   -- Number of nodes in the distributed system
   Num_Nodes : constant := 4;

   -- Array to hold references to all the nodes
   Nodes : array(1 .. Num_Nodes) of Node;

begin
   -- Create nodes in the distributed system
   for I in 1 .. Num_Nodes loop
      Nodes(I) := new Node;
   end loop;

   -- Example usage: Sending messages between nodes
   Nodes(1).Send("Hello from Node 1!");
   Nodes(2).Send("Hello from Node 2!");

   -- Wait for messages to be received
   delay Duration(seconds => 1);

   -- Retrieve messages from each node
   for I in 1 .. Num_Nodes loop
      declare
         Received_Message : String;
      begin
         Nodes(I).Receive(Received_Message);
         Put_Line("Node " & I'Img & " received: " & Received_Message);
      end;
   end loop;

   -- Clean up nodes
   for I in 1 .. Num_Nodes loop
      Nodes(I).Terminate;
   end loop;

   -- Wait for nodes to terminate
   delay Duration(milliseconds => 100);

end Distributed_System;
```

This code simulates a distributed system with multiple nodes, where each node is represented by a separate task. The `Node` task has two entries: `Send` and `Receive`. The `Send` entry allows a node to send a message to the shared message buffer, while the `Receive` entry allows a node to retrieve a message from the buffer.

The shared message buffer is implemented as a singly linked list of message buffers. Each node has its local buffer, and when a message is sent, it is added to the shared buffer. When a node wants to receive a message, it retrieves the message from the buffer.

In this example, four nodes are created, and two messages are sent from nodes 1 and 2. After a delay, the messages are retrieved from each node and displayed on the console.

Please note that this code is a simplified simulation and may not be suitable for a real distributed system. It serves as an example to demonstrate the concepts of tasking, message passing, and synchronization in ADA.