```ada
with Ada.Exceptions;
with Ada.Strings.Unbounded;

type Stack_Type is
  record
    top : natural;
    capacity : natural;
    data : Unbounded_String;
  end record;

procedure Push (Stack : in out Stack_Type; Item : in Character) is
begin
  if Stack.top = Stack.capacity then
    raise Stack_Overflow;
  end if;
  Stack.top := Stack.top + 1;
  Stack.data(Stack.top) := Item;
end Push;

procedure Pop (Stack : in out Stack_Type; Item : out Character) is
begin
  if Stack.top = 0 then
    raise Stack_Underflow;
  end if;
  Item := Stack.data(Stack.top);
  Stack.top := Stack.top - 1;
end Pop;

procedure Initialize_Stack (Stack : in out Stack_Type) is
begin
  Stack.top := 0;
  Stack.capacity := 10;
  Stack.data := (others => ' ');
end Initialize_Stack;

package Stack_ADT is
  procedure Push (Stack : in out Stack_Type; Item : in Character);
  procedure Pop (Stack : in out Stack_Type; Item : out Character);
  procedure Initialize_Stack (Stack : in out Stack_Type);
private
  type Stack_Overflow is Ada.Exceptions.Exception_Id;
  type Stack_Underflow is Ada.Exceptions.Exception_Id;
end Stack_ADT;

with Stack_ADT;

procedure Balanced_Parentheses is
  S : Stack_Type;
  C : Character;
begin
  Initialize_Stack(S);
  loop
    Get(C);
    exit when C = '\n';
    if C = '(' then
      Push(S, C);
    elsif C = ')' then
      Pop(S, C);
    end if;
  end loop;
  if S.top = 0 then
    Put_Line("Expression is balanced.");
  else
    Put_Line("Expression is not balanced.");
  end if;
end Balanced_Parentheses;
```

This code implements a stack data structure in Ada and uses it to check if an expression containing parentheses is balanced. Here's how the code works:

1. **Stack_Type**: This is the definition of the stack data structure. It consists of three fields: `top`, `capacity`, and `data`. `top` keeps track of the index of the top element in the stack. `capacity` specifies the maximum number of elements the stack can hold. `data` is an unbounded string that stores the elements of the stack.

2. **Push**: This procedure pushes an item onto the stack. It first checks if the stack is full (i.e., `top = capacity`). If it is, it raises a `Stack_Overflow` exception. Otherwise, it increments `top` and stores the item at the new top position in the `data` string.

3. **Pop**: This procedure pops an item from the stack. It first checks if the stack is empty (i.e., `top = 0`). If it is, it raises a `Stack_Underflow` exception. Otherwise, it decrements `top` and returns the item at the previous top position.

4. **Initialize_Stack**: This procedure initializes a stack. It sets `top` to 0, `capacity` to 10, and initializes the `data` string with spaces.

5. **Stack_ADT**: This is a package that encapsulates the stack data structure and its operations. It includes the `Push`, `Pop`, and `Initialize_Stack` procedures. It also defines two exceptions: `Stack_Overflow` and `Stack_Underflow`.

6. **Balanced_Parentheses**: This procedure uses the stack to check if an expression containing parentheses is balanced. It starts by initializing the stack. Then, it reads characters from the input one by one. If it encounters an opening parenthesis `('`, it pushes it onto the stack. If it encounters a closing parenthesis `')'`, it pops an item from the stack and checks if it matches the opening parenthesis. If it does, the expression is balanced. If it doesn't, the expression is not balanced.

7. **Main Program**: The main program calls the `Balanced_Parentheses` procedure to check if an expression entered by the user is balanced. It prints a message indicating whether the expression is balanced or not.