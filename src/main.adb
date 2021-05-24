--Task 4
--1) Proof of Unlock operation can only be performed when the calculator is in the locked state:
-- A pragama assertion is added in line (?) before the Unlock operation happens. It confirm that
-- LockedState is True before changing it to False.

--2) Proof of Lock operation successfully updates the PIN with a new PIN supplied:
-- A pragma assertion is added in line (?) after the value of PIN.From_String(S) is passed to
-- PINOriginal. This assertion checks if the master PIN is successfully changed to the new one.
-- And this assertion is measured for each iteration of the while loop.

--3) Proof of "+", "-", "*", "/", load, store, remove, and lock operations can only ever be 
--   performed when the calculator is unlocked:
-- Our implementation of the calculator is designed to distinguish between Locked and Unlocked states
-- by a if statement in line (?) which regulates above operations to be executed only when LockedState
-- is False. To let SPARK prove this, we have added pragma assertion before each of above operations to
-- check if LockedState = False holds.


pragma SPARK_Mode (On);

with StringToInteger;
with VariableStore;
with MyCommandLine;
with MyString;
with MyStringTokeniser;
with PIN; use PIN; -- added use clause for use in pragma assertion
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers;
with Ada.Long_Long_Integer_Text_IO;

with SimpleStack;


procedure Main with SPARK_Mode is
   StackSize : Integer := 512;
   DB : VariableStore.Database;
   PINOriginal  : PIN.PIN := PIN.From_String("0000");
   package Lines is new MyString(Max_MyString_Length => 2048);
   S  : Lines.MyString;
   package SS is new SimpleStack(512, Integer, 0);
   Stack : SS.SimpleStack;
   LockedState : Boolean := False;
   use type Ada.Containers.Count_Type;
begin
   VariableStore.Init(DB);
   SS.init(Stack);
   if MyCommandLine.Argument_Count = 1 then 
      declare
         S : String := MyCommandLine.Argument(1);

      begin
         if S'Length = 4 and (for all I in S'Range => S(I) >= '0' and S(I) <= '9') then 
            PINOriginal := PIN.From_String(S);
            LockedState := True;
         end if;
      end;
   else
      Put("Master PIN not supplied. Application aborted...");
      return;
   end if;
   
   while True loop
      if LockedState = True then
         Put("locked > ");
      else
         Put("unlocked > ");
      end if;
        
      Lines.Get_Line(S);

      declare
         T : MyStringTokeniser.TokenArray(1..2) := (others => (Start => 1, Length => 0));
         NumTokens : Natural;
         subtype CommandString is Lines.MyString;
         subtype InputString is Lines.MyString;
         Command: CommandString := Lines.From_STRING("");
         Input: InputString := Lines.From_STRING("");
      begin
         MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
         for I in 1..NumTokens loop
            declare
               token : String := Lines.To_String(Lines.Substring(S,T(I).Start,T(I).Start+T(I).Length-1));
            begin
               if I = 1 then
                  Command := Lines.From_STRING(token);
               elsif I = 2 then
                  Input := Lines.From_STRING(token);
               end if;

            end;
         end loop;

         if LockedState then
            if Lines.To_String(Command) = "unlock" then
               declare
                  S : String := Lines.To_String(Input);
               begin
                  if S'Length = 4 and (for all I in S'Range => S(I) >= '0' and S(I) <= '9') then
                     declare
                        PINAttempt  : PIN.PIN := PIN.From_String(S); 
                     begin
                        If PIN."="(PINAttempt, PINOriginal) then
                           
                           pragma Assert(LockedState = True); -- Unlock can only perform when its Locked
                           
                           LockedState := False;
                        else 
                           Put("Wrong PIN. Please try again.");New_Line;
                        end if;
                     end;
                  else 
                     Put("You must enter a valid PIN.");New_Line;
                  end if;
               end;
            elsif Lines.To_String(Command) = "lock" then
                Put("Already locked.");New_Line;
            end if;
         else
            if Lines.To_String(Command) = "lock" then
               declare
                   S : String := Lines.To_String(Input);
               begin
                  if S'Length = 4 and (for all I in S'Range => S(I) >= '0' and S(I) <= '9') then
                     
                     pragma Assert(LockedState = False); -- Added to secure operations
                     
                     PINOriginal := PIN.From_String(S);
                     LockedState := True;
                     
                     pragma Assert(LockedState = True and PINOriginal = PIN.From_String(S)); -- check if PIN is updated after locking
                     
                  else 
                     Put("You must enter a valid PIN.");New_Line;
                  end if;
               end;
            elsif Lines.To_String(Command) = "push" then
               if SS.Size(Stack) < StackSize - 1 then
                  declare
                     inputInteger : Integer := StringToInteger.From_String(Lines.To_String(Input));
                  begin
                     if inputInteger >= 0 then
                        
                        pragma Assert(LockedState = False); -- Added to secure operations
                        
                        SS.Push(Stack, inputInteger);
                     else
                        Put("Negative number is not allowed.");
                        return;
                     end if;
                  end;
               else 
                  Put("Stack is full.");New_Line;
                  return;
               end if;
            elsif Lines.To_String(Command) = "pop" then
               if SS.Size(Stack) > 0 then
                  
                  pragma Assert(LockedState = False); -- Added to secure operations
                  
                  SS.Pop(Stack);
               else 
                  Put("Cannot pop on an empty stack.");New_Line;
                  return;
               end if;
            elsif Lines.To_String(Command) = "+" 
              or Lines.To_String(Command) = "-" 
              or Lines.To_String(Command) = "*" 
              or Lines.To_String(Command) = "/" then
               if SS.Size(Stack) > 1 then
                  declare 
                     integerA : Integer;
                     integerB : Integer;
                  begin
                     SS.PopWithResult(Stack, integerA);
                     SS.PopWithResult(Stack, integerB);
                     if Lines.To_String(Command) = "+" and (integerB >= 0 and then integerA < Integer'Last - integerB) then
                        pragma Assert(LockedState = False); -- Added to secure operations
                        SS.Push(Stack, integerA + integerB);
                     elsif Lines.To_String(Command) = "-" and (integerB >= 0 and then integerA > Integer'First + integerB) then
                        pragma Assert(LockedState = False); -- Added to secure operations
                        SS.Push(Stack, integerA - integerB);
                     elsif (Lines.To_String(Command) = "*" and (integerA >= 0 and integerB >= 0)) and then
                       (Long_Long_Integer(integerA) * Long_Long_Integer(integerB)) <= Long_Long_Integer(Integer'Last) then
                        pragma Assert(LockedState = False); -- Added to secure operations
                        SS.Push(Stack, integerA * integerB);
                     elsif Lines.To_String(Command) = "/" then
                        if integerB /= 0 and integerA > 0 and integerB > 0 then
                           pragma Assert(LockedState = False); -- Added to secure operations
                           SS.Push(Stack, integerA / integerB);
                        else
                           Put("Division by zero is disallowed.");New_Line;
                           return;
                        end if;
                     end if;
                  end;
               else
                  Put("Cannot apply arithmatic operations with less than 2 operands in the stack.");New_Line;
                  return;
               end if;
            elsif Lines.To_String(Command) = "store" then
               if SS.Size(Stack) > 0 then
                  declare 
                     S : String := Lines.To_String(Input);
                  begin
                     if S'Length > 0 and S'Length < VariableStore.Max_Variable_Length then
                        declare
                           VariableName : VariableStore.Variable := VariableStore.From_String(S);
                           poppedInteger : Integer;
                        begin
                           SS.PopWithResult(Stack, poppedInteger);
                           if VariableStore.Length(DB) < VariableStore.Max_Entries then
                              pragma Assert(LockedState = False); -- Added to secure operations
                              VariableStore.Put(DB,VariableName,poppedInteger);
                           end if;
                        end;
                     end if;
                  end;
               else
                  Put("Stack is empty");New_Line;
                  return;
               end if;
            elsif Lines.To_String(Command) = "list" then
               
               pragma Assert(LockedState = False); -- Added to secure operations
               
               VariableStore.Print(DB);
            elsif Lines.To_String(Command) = "load" then
               declare 
                  S : String := Lines.To_String(Input);
               begin
                  if S'Length > 0 and S'Length < VariableStore.Max_Variable_Length then
                     declare
                        VariableName : VariableStore.Variable := VariableStore.From_String(S);
                     begin
                        if VariableStore.Has_Variable(DB, VariableName) then
                           if SS.Size(Stack) < StackSize - 1 then
                              
                              pragma Assert(LockedState = False); -- Added to secure operations
                              
                              SS.Push(Stack, VariableStore.Get(DB,VariableName));
                           end if;
                        else
                           Put("Unable to find variable "); Put(Lines.To_String(Input));New_Line;
                           return;
                        end if;
                     end;
                  end if;
               end;
            elsif Lines.To_String(Command) = "remove" then
                declare 
                     S : String := Lines.To_String(Input);
                  begin
                     if S'Length > 0 and S'Length < VariableStore.Max_Variable_Length then
               declare
                  VariableName : VariableStore.Variable := VariableStore.From_String(S);
                     begin
                        pragma Assert(LockedState = False); -- Added to secure operations
                  If VariableStore.Has_Variable(DB,VariableName) then
                     VariableStore.Remove(DB,VariableName);
                  else 
                     Put("Variable "); Put(Lines.To_String(Input)); Put(" does not exist.");New_Line;
                     return;
                  end if;
                     end;
                  end if;
               end;
            end if;
         end if;
      end;
      
   end loop;

   
   
     
   
   --  Put(MyCommandLine.Command_Name); Put_Line(" is running!");
   --  Put("I was invoked with "); Put(MyCommandLine.Argument_Count,0); Put_Line(" arguments.");
   --  for Arg in 1..MyCommandLine.Argument_Count loop
   --     Put("Argument "); Put(Arg,0); Put(": """);
   --     Put(MyCommandLine.Argument(Arg)); Put_Line("""");
   --  end loop;
   --  
   --  VariableStore.Init(DB);
   --  Put_Line("Adding an entry to the database");
   --  VariableStore.Put(DB,V1,10);
   --  
   --  Put_Line("Reading the entry:");
   --  Put(VariableStore.Get(DB,V1));
   --  New_Line;
   --  
   --  Put_Line("Printing out the database: ");
   --  VariableStore.Print(DB);
   --  
   --  Put_Line("Removing the entry");
   --  VariableStore.Remove(DB,V1);
   --  If VariableStore.Has_Variable(DB,V1) then
   --     Put_Line("Entry still present! It is: ");
   --     Put(VariableStore.Get(DB,V1));
   --     New_Line;
   --  else
   --     Put_Line("Entry successfully removed");
   --  end if;
   --  
   --  Put_Line("Reading a line of input. Enter some text (at most 3 tokens): ");
   --  Lines.Get_Line(S);
   --  
   --  Put_Line("Splitting the text into at most 5 tokens");
   --  declare
   --     T : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
   --     NumTokens : Natural;
   --  begin
   --     MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
   --     Put("You entered "); Put(NumTokens); Put_Line(" tokens.");
   --     for I in 1..NumTokens loop
   --        declare
   --           TokStr : String := Lines.To_String(Lines.Substring(S,T(I).Start,T(I).Start+T(I).Length-1));
   --        begin
   --           Put("Token "); Put(I); Put(" is: """);
   --           Put(TokStr); Put_Line("""");
   --        end;
   --     end loop;
   --     if NumTokens > 3 then
   --        Put_Line("You entered too many tokens --- I said at most 3");
   --     end if;
   --  end;
   --  
   --  If PIN."="(PIN1,PIN2) then
   --     Put_Line("The two PINs are equal, as expected.");
   --  end if;
   --  
   --  declare
   --     Smallest_Integer : Integer := StringToInteger.From_String("-2147483648");
   --     R : Long_Long_Integer :=
   --       Long_Long_Integer(Smallest_Integer) * Long_Long_Integer(Smallest_Integer);
   --  begin
   --     Put_Line("This is -(2 ** 32) (where ** is exponentiation) :");
   --     Put(Smallest_Integer); New_Line;
   --  
   --     if R < Long_Long_Integer(Integer'First) or
   --        R > Long_Long_Integer(Integer'Last) then
   --        Put_Line("Overflow would occur when trying to compute the square of this number");
   --     end if;
   --  
   --  end;
   --  Put_Line("2 ** 32 is too big to fit into an Integer...");
   --  Put_Line("Hence when trying to parse it from a string, it is treated as 0:");
   --  Put(StringToInteger.From_String("2147483648")); New_Line;
   
      
end Main;
