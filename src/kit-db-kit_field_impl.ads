with Kit.Mutex;
with System.Storage_Elements;
with Kit.Strings;

private package Kit.Db.Kit_Field_Impl is

   Kit_Field_Magic : constant  := 1790729970;

   type Kit_Field_Database_Record is
      record
         Magic           : Integer := Kit_Field_Magic;
         Deleted         : Boolean := False;
         Kit_Root_Record : Kit_Root_Record_Reference := 0;
         Name            : Kit.Strings.String_Type (64);
         Kit_Record      : Kit_Record_Reference := 0;
         Field_Type      : Kit_Type_Reference := 0;
         Field_Offset    : Integer := 0;
         Field_Length    : Integer := 0;
         Created         : Boolean := False;
         Readable        : Boolean := False;
         Writeable       : Boolean := False;
         Display         : Boolean := False;
         Base_Ref        : Boolean := False;
      end record;

   function Disk_Storage_Units return System.Storage_Elements.Storage_Count;
   procedure Read
     (Ref  : Marlowe.Database_Index;
      Item :    out Kit_Field_Database_Record);
   procedure Write
     (Ref  : Marlowe.Database_Index;
      Item : Kit_Field_Database_Record);
   Top_Record_Key_Mutex : Kit.Mutex.Mutex_Type;
   T1_Idx_Key_Mutex : Kit.Mutex.Mutex_Type;
   Kit_Record_Key_Mutex : Kit.Mutex.Mutex_Type;
   Record_Field_Key_Mutex : Kit.Mutex.Mutex_Type;
   Display_Field_Key_Mutex : Kit.Mutex.Mutex_Type;
   File_Mutex : Kit.Mutex.Mutex_Type;
   function Record_Field_To_Storage
     (Kit_Record : Kit_Record_Reference;
      Name       : String)
   return System.Storage_Elements.Storage_Array;
   function Partial_Record_Field_To_Storage
     (Fill_Low   : Boolean;
      Kit_Record : Kit_Record_Reference)
   return System.Storage_Elements.Storage_Array;
   function Display_Field_To_Storage
     (Kit_Record : Kit_Record_Reference;
      Display    : Boolean)
   return System.Storage_Elements.Storage_Array;
   function Partial_Display_Field_To_Storage
     (Fill_Low   : Boolean;
      Kit_Record : Kit_Record_Reference)
   return System.Storage_Elements.Storage_Array;

private

end Kit.Db.Kit_Field_Impl;
