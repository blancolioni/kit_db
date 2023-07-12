with Kit.Mutex;
with System.Storage_Elements;

private package Kit.Db.Kit_Key_Field_Impl is

   Kit_Key_Field_Magic : constant  := 1856305798;

   type Kit_Key_Field_Database_Record is
      record
         Magic           : Integer := Kit_Key_Field_Magic;
         Deleted         : Boolean := False;
         Kit_Root_Record : Kit_Root_Record_Reference := 0;
         Kit_Key         : Kit_Key_Reference := 0;
         Kit_Field       : Kit_Field_Reference := 0;
      end record;

   function Disk_Storage_Units return System.Storage_Elements.Storage_Count;
   procedure Read
     (Ref  : Marlowe.Database_Index;
      Item :    out Kit_Key_Field_Database_Record);
   procedure Write
     (Ref  : Marlowe.Database_Index;
      Item : Kit_Key_Field_Database_Record);
   Top_Record_Key_Mutex : Kit.Mutex.Mutex_Type;
   T1_Idx_Key_Mutex : Kit.Mutex.Mutex_Type;
   Kit_Key_Key_Mutex : Kit.Mutex.Mutex_Type;
   Kit_Field_Key_Mutex : Kit.Mutex.Mutex_Type;
   Key_Field_Key_Mutex : Kit.Mutex.Mutex_Type;
   File_Mutex : Kit.Mutex.Mutex_Type;
   function Key_Field_To_Storage
     (Kit_Key   : Kit_Key_Reference;
      Kit_Field : Kit_Field_Reference)
   return System.Storage_Elements.Storage_Array;
   function Partial_Key_Field_To_Storage
     (Fill_Low : Boolean;
      Kit_Key  : Kit_Key_Reference)
   return System.Storage_Elements.Storage_Array;

private

end Kit.Db.Kit_Key_Field_Impl;
