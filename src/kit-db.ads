private with Marlowe;
private with Marlowe.Key_Storage;
private with Kit.Mutex;

package Kit.Db is

   type Integer_64 is range -2 ** 63 .. 2 ** 63 - 1;
   type Record_Type is (R_None, R_Kit_Root_Record, R_Kit_Record,
                        R_Kit_Record_Base, R_Kit_Type, R_Kit_Integer,
                        R_Kit_Long_Integer, R_Kit_Float, R_Kit_Long_Float,
                        R_Kit_Reference, R_Kit_String, R_Kit_Fixed_String,
                        R_Kit_Bounded_String, R_Kit_Enumeration,
                        R_Kit_Literal, R_Kit_Field, R_Kit_Display_Field,
                        R_Kit_Key, R_Kit_Key_Field);

   type Kit_Root_Record_Reference is private;
   Null_Kit_Root_Record_Reference : constant Kit_Root_Record_Reference;
   function To_String (Item : Kit_Root_Record_Reference) return String;

   type Kit_Record_Reference is private;
   Null_Kit_Record_Reference : constant Kit_Record_Reference;
   function To_String (Item : Kit_Record_Reference) return String;

   type Kit_Record_Base_Reference is private;
   Null_Kit_Record_Base_Reference : constant Kit_Record_Base_Reference;
   function To_String (Item : Kit_Record_Base_Reference) return String;

   type Kit_Type_Reference is private;
   Null_Kit_Type_Reference : constant Kit_Type_Reference;
   function To_String (Item : Kit_Type_Reference) return String;

   type Kit_Integer_Reference is private;
   Null_Kit_Integer_Reference : constant Kit_Integer_Reference;
   function To_String (Item : Kit_Integer_Reference) return String;

   type Kit_Long_Integer_Reference is private;
   Null_Kit_Long_Integer_Reference : constant Kit_Long_Integer_Reference;
   function To_String (Item : Kit_Long_Integer_Reference) return String;

   type Kit_Float_Reference is private;
   Null_Kit_Float_Reference : constant Kit_Float_Reference;
   function To_String (Item : Kit_Float_Reference) return String;

   type Kit_Long_Float_Reference is private;
   Null_Kit_Long_Float_Reference : constant Kit_Long_Float_Reference;
   function To_String (Item : Kit_Long_Float_Reference) return String;

   type Kit_Reference_Reference is private;
   Null_Kit_Reference_Reference : constant Kit_Reference_Reference;
   function To_String (Item : Kit_Reference_Reference) return String;

   type Kit_String_Reference is private;
   Null_Kit_String_Reference : constant Kit_String_Reference;
   function To_String (Item : Kit_String_Reference) return String;

   type Kit_Fixed_String_Reference is private;
   Null_Kit_Fixed_String_Reference : constant Kit_Fixed_String_Reference;
   function To_String (Item : Kit_Fixed_String_Reference) return String;

   type Kit_Bounded_String_Reference is private;
   Null_Kit_Bounded_String_Reference : constant Kit_Bounded_String_Reference;
   function To_String (Item : Kit_Bounded_String_Reference) return String;

   type Kit_Enumeration_Reference is private;
   Null_Kit_Enumeration_Reference : constant Kit_Enumeration_Reference;
   function To_String (Item : Kit_Enumeration_Reference) return String;

   type Kit_Literal_Reference is private;
   Null_Kit_Literal_Reference : constant Kit_Literal_Reference;
   function To_String (Item : Kit_Literal_Reference) return String;

   type Kit_Field_Reference is private;
   Null_Kit_Field_Reference : constant Kit_Field_Reference;
   function To_String (Item : Kit_Field_Reference) return String;

   type Kit_Display_Field_Reference is private;
   Null_Kit_Display_Field_Reference : constant Kit_Display_Field_Reference;
   function To_String (Item : Kit_Display_Field_Reference) return String;

   type Kit_Key_Reference is private;
   Null_Kit_Key_Reference : constant Kit_Key_Reference;
   function To_String (Item : Kit_Key_Reference) return String;

   type Kit_Key_Field_Reference is private;
   Null_Kit_Key_Field_Reference : constant Kit_Key_Field_Reference;
   function To_String (Item : Kit_Key_Field_Reference) return String;

   type Record_Interface is limited interface;

   procedure X_Lock (Item : Record_Interface)
      is abstract;
   function Get
     (Item  : Record_Interface;
      Field : String)
   return String
      is abstract;

   function Identity (Item : Record_Interface) return String
      is abstract;

   type Record_Update_Interface is limited interface;

   procedure Set
     (Item  : in out Record_Update_Interface;
      Field : String;
      Value : String)
      is abstract;

   type Search_Interface is limited interface;

   function Has_Element (Item : Search_Interface) return Boolean
      is abstract;

   type Lock_Context_Record is abstract tagged private;
   type Lock_Context is access all Lock_Context_Record'Class;
   procedure X_Lock (Context : in out Lock_Context_Record)
      is abstract;
   procedure S_Lock (Context : in out Lock_Context_Record)
      is abstract;
   procedure Unlock (Context : in out Lock_Context_Record)
      is abstract;
   Database_Magic_Number : constant  := 1;

private

   package Integer_64_Storage is
     new Marlowe.Key_Storage.Integral_Storage
          (Integer_64);

   type Kit_Root_Record_Reference is new Marlowe.Database_Index;
   Null_Kit_Root_Record_Reference : constant Kit_Root_Record_Reference := 0;
   Kit_Root_Record_Table_Index : constant  := 1;

   type Kit_Record_Reference is new Marlowe.Database_Index;
   Null_Kit_Record_Reference : constant Kit_Record_Reference := 0;
   Kit_Record_Table_Index : constant  := 2;

   type Kit_Record_Base_Reference is new Marlowe.Database_Index;
   Null_Kit_Record_Base_Reference : constant Kit_Record_Base_Reference := 0;
   Kit_Record_Base_Table_Index : constant  := 3;

   type Kit_Type_Reference is new Marlowe.Database_Index;
   Null_Kit_Type_Reference : constant Kit_Type_Reference := 0;
   Kit_Type_Table_Index : constant  := 4;

   type Kit_Integer_Reference is new Marlowe.Database_Index;
   Null_Kit_Integer_Reference : constant Kit_Integer_Reference := 0;
   Kit_Integer_Table_Index : constant  := 5;

   type Kit_Long_Integer_Reference is new Marlowe.Database_Index;
   Null_Kit_Long_Integer_Reference : constant Kit_Long_Integer_Reference :=
      0;
   Kit_Long_Integer_Table_Index : constant  := 6;

   type Kit_Float_Reference is new Marlowe.Database_Index;
   Null_Kit_Float_Reference : constant Kit_Float_Reference := 0;
   Kit_Float_Table_Index : constant  := 7;

   type Kit_Long_Float_Reference is new Marlowe.Database_Index;
   Null_Kit_Long_Float_Reference : constant Kit_Long_Float_Reference := 0;
   Kit_Long_Float_Table_Index : constant  := 8;

   type Kit_Reference_Reference is new Marlowe.Database_Index;
   Null_Kit_Reference_Reference : constant Kit_Reference_Reference := 0;
   Kit_Reference_Table_Index : constant  := 9;

   type Kit_String_Reference is new Marlowe.Database_Index;
   Null_Kit_String_Reference : constant Kit_String_Reference := 0;
   Kit_String_Table_Index : constant  := 10;

   type Kit_Fixed_String_Reference is new Marlowe.Database_Index;
   Null_Kit_Fixed_String_Reference : constant Kit_Fixed_String_Reference :=
      0;
   Kit_Fixed_String_Table_Index : constant  := 11;

   type Kit_Bounded_String_Reference is new Marlowe.Database_Index;
   Null_Kit_Bounded_String_Reference : constant
      Kit_Bounded_String_Reference := 0;
   Kit_Bounded_String_Table_Index : constant  := 12;

   type Kit_Enumeration_Reference is new Marlowe.Database_Index;
   Null_Kit_Enumeration_Reference : constant Kit_Enumeration_Reference := 0;
   Kit_Enumeration_Table_Index : constant  := 13;

   type Kit_Literal_Reference is new Marlowe.Database_Index;
   Null_Kit_Literal_Reference : constant Kit_Literal_Reference := 0;
   Kit_Literal_Table_Index : constant  := 14;

   type Kit_Field_Reference is new Marlowe.Database_Index;
   Null_Kit_Field_Reference : constant Kit_Field_Reference := 0;
   Kit_Field_Table_Index : constant  := 15;

   type Kit_Display_Field_Reference is new Marlowe.Database_Index;
   Null_Kit_Display_Field_Reference : constant Kit_Display_Field_Reference :=
      0;
   Kit_Display_Field_Table_Index : constant  := 16;

   type Kit_Key_Reference is new Marlowe.Database_Index;
   Null_Kit_Key_Reference : constant Kit_Key_Reference := 0;
   Kit_Key_Table_Index : constant  := 17;

   type Kit_Key_Field_Reference is new Marlowe.Database_Index;
   Null_Kit_Key_Field_Reference : constant Kit_Key_Field_Reference := 0;
   Kit_Key_Field_Table_Index : constant  := 18;

   type Lock_Context_Record is abstract tagged
      record
         S_Locked : Boolean := False;
         X_Locked : Boolean := False;
      end record;
   Memory_Mutex : Kit.Mutex.Mutex_Type;
   Database_Mutex : Kit.Mutex.Mutex_Type;

end Kit.Db;
