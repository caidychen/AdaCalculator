with Ada.Characters.Latin_1;

package MyStringTokeniser with SPARK_Mode is

   type TokenExtent is record
      Start : Positive;
      Length : Natural;
   end record;

   type TokenArray is array(Positive range <>) of TokenExtent;

   function Is_Whitespace(Ch : Character) return Boolean is
     (Ch = ' ' or Ch = Ada.Characters.Latin_1.LF or
        Ch = Ada.Characters.Latin_1.HT);

   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) with
     Pre => (if S'Length > 0 then S'First <= S'Last) and Tokens'First <= Tokens'Last,
     Post => Count <= Tokens'Length and   -- 'Count' represents NumTokens as index. This helps eliminates
                                          -- 'array index out of bounds' when proving with SPARK.

     (for all Index in Tokens'First..Tokens'First+(Count-1) =>   -- For all tokenised tokens from a string:
          (Tokens(Index).Start >= S'First and                    -- the first letter of every tokens from
                                                                 -- that string must have a index number
                                                                 -- euqal to or greater then the first
                                                                 -- index number of the string.
          Tokens(Index).Length > 0) and then                     -- Tokens must have lengths greater than
                                                                 -- zero otherwise there is no token at all.
        Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start); -- This is identical to that the first Loop_Invariant
                                                                 -- specified in .adb file. Here specified as post-condition
                                                                 -- to satisfy logic. ******** TODO: Does this sounds right? *********



end MyStringTokeniser;
