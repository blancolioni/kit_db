with Kit.Db.Kit_Enumeration;
with Kit.Db.Kit_Float;
with Kit.Db.Kit_Integer;
with Kit.Db.Kit_Long_Integer;
with Kit.Db.Kit_Literal;
with Kit.Db.Kit_Long_Float;
with Kit.Db.Kit_Bounded_String;
with Kit.Db.Kit_Type;

package body Kit.Db.Database.Types is

   ------------------
   -- Create_Types --
   ------------------

   procedure Create_Types is
   begin
      Kit_Float.Create (4, "float");
      Kit_Integer.Create
        (4,
         "natural",
         0,
         2147483647);
      declare
         Enum : constant Kit_Enumeration_Reference := Kit_Enumeration.Create
            (1, "boolean");
      begin
         Kit_Literal.Create
           ("false",
            Enum,
            0);
         Kit_Literal.Create
           ("true",
            Enum,
            1);
      end;
      Kit_Long_Integer.Create
        (8,
         "integer_64",
         -9223372036854775808,
         9223372036854775807);
      declare
         Enum : constant Kit_Enumeration_Reference := Kit_Enumeration.Create
            (4, "record_type");
      begin
         Kit_Literal.Create
           ("r_none",
            Enum,
            0);
         Kit_Literal.Create
           ("r_kit_root_record",
            Enum,
            1);
         Kit_Literal.Create
           ("r_kit_record",
            Enum,
            2);
         Kit_Literal.Create
           ("r_kit_record_base",
            Enum,
            3);
         Kit_Literal.Create
           ("r_kit_type",
            Enum,
            4);
         Kit_Literal.Create
           ("r_kit_integer",
            Enum,
            5);
         Kit_Literal.Create
           ("r_kit_long_integer",
            Enum,
            6);
         Kit_Literal.Create
           ("r_kit_float",
            Enum,
            7);
         Kit_Literal.Create
           ("r_kit_long_float",
            Enum,
            8);
         Kit_Literal.Create
           ("r_kit_reference",
            Enum,
            9);
         Kit_Literal.Create
           ("r_kit_string",
            Enum,
            10);
         Kit_Literal.Create
           ("r_kit_fixed_string",
            Enum,
            11);
         Kit_Literal.Create
           ("r_kit_bounded_string",
            Enum,
            12);
         Kit_Literal.Create
           ("r_kit_enumeration",
            Enum,
            13);
         Kit_Literal.Create
           ("r_kit_literal",
            Enum,
            14);
         Kit_Literal.Create
           ("r_kit_field",
            Enum,
            15);
         Kit_Literal.Create
           ("r_kit_display_field",
            Enum,
            16);
         Kit_Literal.Create
           ("r_kit_key",
            Enum,
            17);
         Kit_Literal.Create
           ("r_kit_key_field",
            Enum,
            18);
      end;
      Kit_Integer.Create
        (4,
         "integer",
         -2147483648,
         2147483647);
      Kit_Integer.Create
        (4,
         "positive",
         1,
         2147483647);
      Kit_Long_Float.Create (8, "long_float");
      Kit_Bounded_String.Create
        (64,
         64,
         "bounded_string_64");
      Kit_Type.Create (32, "text");
   end Create_Types;

end Kit.Db.Database.Types;
