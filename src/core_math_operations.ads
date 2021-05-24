package core_math_operations with SPARK_Mode is

   function add(integerA : in Integer; integerB : in Integer) return Integer with
    Pre =>
       (Long_Long_Integer(integerA) + Long_Long_Integer(integerB) <= Long_Long_Integer(Integer'Last)) and
       (Long_Long_Integer(integerA) + Long_Long_Integer(integerB) >= Long_Long_Integer(Integer'First)),
       Post => add'Result = integerA + integerB;
   
   function minus(integerA : in Integer; integerB : in Integer) return Integer with
     Pre => 
       (Long_Long_Integer(integerA) - Long_Long_Integer(integerB) <= Long_Long_Integer(Integer'Last)) and
       (Long_Long_Integer(integerA) - Long_Long_Integer(integerB) >= Long_Long_Integer(Integer'First)),
       Post => minus'Result = integerA - integerB;
   
   function multiply(integerA : in Integer; integerB : in Integer) return Integer with 
     Pre =>
       (Long_Long_Integer(integerA) * Long_Long_Integer(integerB) <= Long_Long_Integer(Integer'Last)) and
       (Long_Long_Integer(integerA) * Long_Long_Integer(integerB) >= Long_Long_Integer(Integer'First)),
     Post => multiply'Result = integerA * integerB;
   
   function divide(integerA : in Integer; integerB : in Integer) return Integer with
     Pre => 
       integerB /= 0 and then
       (
          (Long_Long_Integer(integerA) / Long_Long_Integer(integerB) <= Long_Long_Integer(Integer'Last)) and
            (Long_Long_Integer(integerA) / Long_Long_Integer(integerB) >= Long_Long_Integer(Integer'First))
       ),
     Post => divide'Result = integerA / integerB;

end core_math_operations;
