pragma SPARK_Mode (On);

with StringToInteger;
with VariableStore;
with MyCommandLine;
with MyString;
with MyStringTokeniser;
with PIN;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Long_Long_Integer_Text_IO;

with SimpleStack;


procedure Main is
   DB : VariableStore.Database;
   V1 : VariableStore.Variable := VariableStore.From_String("Var1");
   PIN1  : PIN.PIN := PIN.From_String("1234");
   PIN2  : PIN.PIN := PIN.From_String("1234");
   package Lines is new MyString(Max_MyString_Length => 2048);
   S  : Lines.MyString;
   
   package SS is new SimpleStack(100, Integer, 0);
   Stack : SS.SimpleStack;
   
begin

   SS.init(Stack);

   while True loop
      Put("unlocked > "); Lines.Get_Line(S);

      declare
         T : MyStringTokeniser.TokenArray(1..2) := (others => (Start => 1, Length => 0));
         NumTokens : Natural;
         subtype CommandString is Lines.MyString;
         subtype InputString is Lines.MyString;
         command: CommandString;
         input: InputString;
      begin
         MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
         for I in 1..NumTokens loop
            declare
               token : String := Lines.To_String(Lines.Substring(S,T(I).Start,T(I).Start+T(I).Length-1));
            begin
               if I = 1 then
                  command := Lines.From_STRING(token);
               elsif I = 2 then
                  input := Lines.From_STRING(token);
               end if;

            end;
         end loop;

         if Lines.To_String(Command) = "push" then
            declare
               inputInteger : Integer := StringToInteger.From_String(Lines.To_String(Input));
            begin
               SS.Push(Stack, inputInteger);
            end;
         elsif Lines.To_String(Command) = "pop" then
            declare
               poppedInteger : Integer := StringToInteger.From_String(Lines.To_String(Input));
            begin
               SS.Pop(Stack, poppedInteger);
               Put_Line("Popped: "); Ada.Integer_Text_IO.Put(poppedInteger);New_Line; -- Dont forget to delete this print!!
            end;
         elsif Lines.To_String(Command) = "+" 
           or Lines.To_String(Command) = "-" 
           or Lines.To_String(Command) = "*" 
           or Lines.To_String(Command) = "/" then
            declare 
               integerA : Integer;
               integerB : Integer;
            begin
               SS.Pop(Stack, integerA);
               SS.Pop(Stack, integerB);
               if Lines.To_String(Command) = "+" then
                 SS.Push(Stack, integerB + integerA);
               elsif Lines.To_String(Command) = "-" then
                 SS.Push(Stack, integerB - integerA);
               elsif Lines.To_String(Command) = "*" then
                 SS.Push(Stack, integerB * integerA);
               elsif Lines.To_String(Command) = "/" then
                  SS.Push(Stack, integerB / integerA);
               end if;
            end;
                
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
