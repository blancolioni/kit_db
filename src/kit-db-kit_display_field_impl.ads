with Kit.Mutex;
with System.Storage_Elements;

private package Kit.Db.Kit_Display_Field_Impl is

   Kit_Display_Field_Magic : constant  := 1345351530;

   type Kit_Display_Field_Database_Record is
      record
         Magic           : Integer := Kit_Display_Field_Magic;
         Deleted         : Boolean := False;
         Kit_Root_Record : Kit_Root_Record_Reference := 0;
         Kit_Record      : Kit_Record_Reference := 0;
         Kit_Field       : Kit_Field_Reference := 0;
      end record;

   function Disk_Storage_Units return System.Storage_Elements.Storage_Count;
   procedure Read
     (Ref  : Marlowe.Database_Index;
      Item :    out Kit_Display_Field_Database_Record);
   procedure Write
     (Ref  : Marlowe.Database_Index;
      Item : Kit_Display_Field_Database_Record);
   Top_Record_Key_Mutex : Kit.Mutex.Mutex_Type;
   T1_Idx_Key_Mutex : Kit.Mutex.Mutex_Type;
   Kit_Record_Key_Mutex : Kit.Mutex.Mutex_Type;
   Kit_Field_Key_Mutex : Kit.Mutex.Mutex_Type;
   File_Mutex : Kit.Mutex.Mutex_Type;

private

end Kit.Db.Kit_Display_Field_Impl;
