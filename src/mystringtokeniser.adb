
package body MyStringTokeniser with SPARK_Mode is



   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) is
      Index : Positive; -- The index of the entire string that's being pointed at
      Extent : TokenExtent;
      OutIndex : Integer := Tokens'First; -- The index of tokens array that's being pointed at
   begin
      Count := 0; -- Tokens array count
      if (S'First > S'Last) then
         return;
      end if;
      Index := S'First;
      while OutIndex <= Tokens'Last and Index <= S'Last and Count < Tokens'Length loop
         pragma Loop_Invariant
         -- For all indices from Tokens'First to OutIndex - 1:
         -- The index of the starting point of each token must be equal to or greater than
         -- the index of the first letter from the input string.
         -- Every non-negative token's length must be equal to or smaller than the length of the input string minus
         -- the starting index of the token.
         -- For example, a token named "var" from "store var" starts from index 7 and ends at 9.
         -- In this case 3 - 1 <= 9 - 7 holds.
           (for all J in Tokens'First..OutIndex-1 =>
              (Tokens(J).Start >= S'First and
                   Tokens(J).Length > 0) and then
            Tokens(J).Length-1 <= S'Last - Tokens(J).Start);

         pragma Loop_Invariant (OutIndex = Tokens'First + Count); -- At the beginning of the loop, OutIndex = Tokens'First and Count = 0
                                                                  -- After each loop, OutIndex is incremented by 1, so is Count. Therefore Invariant always holds.
                                                                  -- At the end of the loop, the invariant still holds.

         -- look for start of next token and skip all the whitespaces
         while (Index >= S'First and Index < S'Last) and then Is_Whitespace(S(Index)) loop
            Index := Index + 1;
         end loop;
         if (Index >= S'First and Index <= S'Last) and then not Is_Whitespace(S(Index)) then
            -- found the start of the token
            Extent.Start := Index;
            Extent.Length := 0;

            -- look for end of this token
            while Positive'Last - Extent.Length >= Index and then (Index+Extent.Length >= S'First and Index+Extent.Length <= S'Last) and then not Is_Whitespace(S(Index+Extent.Length)) loop
               Extent.Length := Extent.Length + 1;
            end loop;

            -- append the token into the array and increment count by 1
            Tokens(OutIndex) := Extent;
            Count := Count + 1;

            -- if the current index of token array has reached the pre-defined array boundary, exit the loop.
            -- otherwise, keep looking for the next token
            if (OutIndex = Tokens'Last) then
               return;
            else
               OutIndex := OutIndex + 1;
            end if;

            -- check for end of string, avoids overflow when incrementing Index
            if S'Last - Extent.Length < Index then
               return;
            else
               Index := Index + Extent.Length;
            end if;
         end if;
      end loop;
   end Tokenise;

end MyStringTokeniser;
