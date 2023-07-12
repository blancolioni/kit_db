with Kit.Mutex;
with System.Storage_Elements;

private package Kit.Db.Kit_Root_Record_Impl is

   Kit_Root_Record_Magic : constant  := 683468704;

   type Kit_Root_Record_Database_Record is
      record
         Magic      : Integer := Kit_Root_Record_Magic;
         Deleted    : Boolean := False;
         Top_Record : Record_Type := R_None;
      end record;

   function Disk_Storage_Units return System.Storage_Elements.Storage_Count;
   procedure Read
     (Ref  : Marlowe.Database_Index;
      Item :    out Kit_Root_Record_Database_Record);
   procedure Write
     (Ref  : Marlowe.Database_Index;
      Item : Kit_Root_Record_Database_Record);
   Top_Record_Key_Mutex : Kit.Mutex.Mutex_Type;
   File_Mutex : Kit.Mutex.Mutex_Type;

private

end Kit.Db.Kit_Root_Record_Impl;
