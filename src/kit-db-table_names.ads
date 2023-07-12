package Kit.Db.Table_Names is

   type Table_Reference is private;
   function Table_Name (Table : Table_Reference) return String;
   function Table_Source (Table : Table_Reference) return String;
   Kit_Root_Record : constant Table_Reference;
   Kit_Record : constant Table_Reference;
   Kit_Record_Base : constant Table_Reference;
   Kit_Type : constant Table_Reference;
   Kit_Integer : constant Table_Reference;
   Kit_Long_Integer : constant Table_Reference;
   Kit_Float : constant Table_Reference;
   Kit_Long_Float : constant Table_Reference;
   Kit_Reference : constant Table_Reference;
   Kit_String : constant Table_Reference;
   Kit_Fixed_String : constant Table_Reference;
   Kit_Bounded_String : constant Table_Reference;
   Kit_Enumeration : constant Table_Reference;
   Kit_Literal : constant Table_Reference;
   Kit_Field : constant Table_Reference;
   Kit_Display_Field : constant Table_Reference;
   Kit_Key : constant Table_Reference;
   Kit_Key_Field : constant Table_Reference;

private

   type Table_Reference is new Positive range 1 .. 18;
   Kit_Root_Record : constant Table_Reference := 1;
   Kit_Record : constant Table_Reference := 2;
   Kit_Record_Base : constant Table_Reference := 3;
   Kit_Type : constant Table_Reference := 4;
   Kit_Integer : constant Table_Reference := 5;
   Kit_Long_Integer : constant Table_Reference := 6;
   Kit_Float : constant Table_Reference := 7;
   Kit_Long_Float : constant Table_Reference := 8;
   Kit_Reference : constant Table_Reference := 9;
   Kit_String : constant Table_Reference := 10;
   Kit_Fixed_String : constant Table_Reference := 11;
   Kit_Bounded_String : constant Table_Reference := 12;
   Kit_Enumeration : constant Table_Reference := 13;
   Kit_Literal : constant Table_Reference := 14;
   Kit_Field : constant Table_Reference := 15;
   Kit_Display_Field : constant Table_Reference := 16;
   Kit_Key : constant Table_Reference := 17;
   Kit_Key_Field : constant Table_Reference := 18;

end Kit.Db.Table_Names;
