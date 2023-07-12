package body Kit.Db.Table_Names is

   ----------------
   -- Table_Name --
   ----------------

   function Table_Name (Table : Table_Reference) return String is
   begin
      case Table is
         when 1 =>
               return "Kit_Root_Record";
         when 2 =>
               return "Kit_Record";
         when 3 =>
               return "Kit_Record_Base";
         when 4 =>
               return "Kit_Type";
         when 5 =>
               return "Kit_Integer";
         when 6 =>
               return "Kit_Long_Integer";
         when 7 =>
               return "Kit_Float";
         when 8 =>
               return "Kit_Long_Float";
         when 9 =>
               return "Kit_Reference";
         when 10 =>
               return "Kit_String";
         when 11 =>
               return "Kit_Fixed_String";
         when 12 =>
               return "Kit_Bounded_String";
         when 13 =>
               return "Kit_Enumeration";
         when 14 =>
               return "Kit_Literal";
         when 15 =>
               return "Kit_Field";
         when 16 =>
               return "Kit_Display_Field";
         when 17 =>
               return "Kit_Key";
         when 18 =>
               return "Kit_Key_Field";
      end case;
   end Table_Name;

   ------------------
   -- Table_Source --
   ------------------

   function Table_Source (Table : Table_Reference) return String is
   begin
      case Table is
         when 1 =>
               return "kit_root_record.adb";
         when 2 =>
               return "kit_record.adb";
         when 3 =>
               return "kit_record_base.adb";
         when 4 =>
               return "kit_type.adb";
         when 5 =>
               return "kit_integer.adb";
         when 6 =>
               return "kit_long_integer.adb";
         when 7 =>
               return "kit_float.adb";
         when 8 =>
               return "kit_long_float.adb";
         when 9 =>
               return "kit_reference.adb";
         when 10 =>
               return "kit_string.adb";
         when 11 =>
               return "kit_fixed_string.adb";
         when 12 =>
               return "kit_bounded_string.adb";
         when 13 =>
               return "kit_enumeration.adb";
         when 14 =>
               return "kit_literal.adb";
         when 15 =>
               return "kit_field.adb";
         when 16 =>
               return "kit_display_field.adb";
         when 17 =>
               return "kit_key.adb";
         when 18 =>
               return "kit_key_field.adb";
      end case;
   end Table_Source;

end Kit.Db.Table_Names;
