package body core_math_operations with SPARK_Mode is

   function add(integerA : in Integer; integerB : in Integer) return Integer is
   begin
      return integerA + integerB;
   end add;
     
   function minus(integerA : in Integer; integerB : in Integer) return Integer is
   begin
      return integerA - integerB;
   end minus;
   
   function multiply(integerA : in Integer; integerB : in Integer) return Integer is
   begin
      return integerA * integerB;
   end multiply;
   
   function divide(integerA : in Integer; integerB : in Integer) return Integer is
   begin
      return integerA / integerB;
   end divide;
   

end core_math_operations;
