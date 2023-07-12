with Kit.Mutex;
with System.Storage_Elements;
with Kit.Strings;

private package Kit.Db.Kit_Literal_Impl is

   Kit_Literal_Magic : constant  := 1450743020;

   type Kit_Literal_Database_Record is
      record
         Magic           : Integer := Kit_Literal_Magic;
         Deleted         : Boolean := False;
         Kit_Root_Record : Kit_Root_Record_Reference := 0;
         Name            : Kit.Strings.String_Type (64);
         Kit_Enumeration : Kit_Enumeration_Reference := 0;
         Value           : Integer := 0;
      end record;

   function Disk_Storage_Units return System.Storage_Elements.Storage_Count;
   procedure Read
     (Ref  : Marlowe.Database_Index;
      Item :    out Kit_Literal_Database_Record);
   procedure Write
     (Ref  : Marlowe.Database_Index;
      Item : Kit_Literal_Database_Record);
   Top_Record_Key_Mutex : Kit.Mutex.Mutex_Type;
   T1_Idx_Key_Mutex : Kit.Mutex.Mutex_Type;
   Kit_Enumeration_Key_Mutex : Kit.Mutex.Mutex_Type;
   Value_Key_Mutex : Kit.Mutex.Mutex_Type;
   Enum_Value_Key_Mutex : Kit.Mutex.Mutex_Type;
   Enum_Name_Key_Mutex : Kit.Mutex.Mutex_Type;
   File_Mutex : Kit.Mutex.Mutex_Type;
   function Enum_Value_To_Storage
     (Kit_Enumeration : Kit_Enumeration_Reference;
      Value           : Integer)
   return System.Storage_Elements.Storage_Array;
   function Partial_Enum_Value_To_Storage
     (Fill_Low        : Boolean;
      Kit_Enumeration : Kit_Enumeration_Reference)
   return System.Storage_Elements.Storage_Array;
   function Enum_Name_To_Storage
     (Kit_Enumeration : Kit_Enumeration_Reference;
      Name            : String)
   return System.Storage_Elements.Storage_Array;
   function Partial_Enum_Name_To_Storage
     (Fill_Low        : Boolean;
      Kit_Enumeration : Kit_Enumeration_Reference)
   return System.Storage_Elements.Storage_Array;

private

end Kit.Db.Kit_Literal_Impl;
