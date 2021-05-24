package core_math_operations with SPARK_Mode is

   function add(integerA : in Integer; integerB : in Integer) return Integer with
     Pre => 
       (if integerB >= 0 then (integerA > Integer'First + integerB and integerA < Integer'Last - integerB)) and 
       (if integerB < 0 then (integerA > Integer'First - integerB and integerA < Integer'Last + integerB)),
     Post => add'Result = integerA + integerB;
   
   function minus(integerA : in Integer; integerB : in Integer) return Integer with
     Pre => 
       (if integerB >= 0 then (integerA < Integer'Last - integerB and integerA > Integer'First + integerB)) and
       (if integerB < 0 then (integerA < Integer'Last + integerB and integerA > Integer'First - integerB)),
     Post => minus'Result = integerA - integerB;
   
   function multiply(integerA : in Integer; integerB : in Integer) return Integer with 
     Pre =>
       (if ((integerA >= 0 and integerB >= 0) or (integerA <= 0 and integerB <= 0)) then 
          (Long_Long_Integer(integerA) * Long_Long_Integer(integerB) <= Long_Long_Integer(Integer'Last))) and
     (if ((integerA >= 0 and integerB <= 0) or (integerA <= 0 and integerB >= 0)) then 
              (Long_Long_Integer(integerA) * Long_Long_Integer(integerB) >= Long_Long_Integer(Integer'First))),
     Post => multiply'Result = integerA * integerB;
   
   function divide(integerA : in Integer; integerB : in Integer) return Integer with
     Pre => 
       integerB /= 0 and
       (integerB > 0 or 
       (if integerB < 0 then integerA > Integer'First)),
     Post => divide'Result = integerA / integerB;

end core_math_operations;
