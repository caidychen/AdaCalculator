pragma SPARK_Mode (On);

with StringToInteger;
with VariableStore;
with MyCommandLine;
with MyString;
with MyStringTokeniser;
with PIN;
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
                           LockedState := False;
                        else 
                           Put("Wrong PIN. Please try again.");New_Line;
                        end if;
                     end;
                  else 
                     Put("You must enter a valid PIN.");New_Line;
                  end if;
               end;
            end if;
         else
            if Lines.To_String(Command) = "lock" then
               declare
                   S : String := Lines.To_String(Input);
               begin
                  if S'Length = 4 and (for all I in S'Range => S(I) >= '0' and S(I) <= '9') then
                     PINOriginal := PIN.From_String(S);
                     LockedState := True;
                  else 
                     Put("You must enter a valid PIN.");New_Line;
                  end if;
               end;
            elsif Lines.To_String(Command) = "push" then
               if SS.Size(Stack) < StackSize - 1 then
                  declare
                     inputInteger : Integer := StringToInteger.From_String(Lines.To_String(Input));
                  begin
                     if inputInteger > 0 then
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
                     if Lines.To_String(Command) = "+" and integerA >= 0 and integerB < Integer'Last - integerA then
                        SS.Push(Stack, integerB + integerA);
                     elsif Lines.To_String(Command) = "-" then
                        SS.Push(Stack, integerB - integerA);
                     elsif Lines.To_String(Command) = "*" then
                        SS.Push(Stack, integerB * integerA);
                     elsif Lines.To_String(Command) = "/" then
                        if integerA /= 0 then
                           SS.Push(Stack, integerB / integerA);
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
