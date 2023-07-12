with Kit.Mutex;
with System.Storage_Elements;

private package Kit.Db.Kit_Float_Impl is

   Kit_Float_Magic : constant  := 1796745100;

   type Kit_Float_Database_Record is
      record
         Magic           : Integer := Kit_Float_Magic;
         Deleted         : Boolean := False;
         Kit_Root_Record : Kit_Root_Record_Reference := 0;
         Kit_Type        : Kit_Type_Reference := 0;
      end record;

   function Disk_Storage_Units return System.Storage_Elements.Storage_Count;
   procedure Read
     (Ref  : Marlowe.Database_Index;
      Item :    out Kit_Float_Database_Record);
   procedure Write
     (Ref  : Marlowe.Database_Index;
      Item : Kit_Float_Database_Record);
   Top_Record_Key_Mutex : Kit.Mutex.Mutex_Type;
   T1_Idx_Key_Mutex : Kit.Mutex.Mutex_Type;
   Name_Key_Mutex : Kit.Mutex.Mutex_Type;
   T4_Idx_Key_Mutex : Kit.Mutex.Mutex_Type;
   File_Mutex : Kit.Mutex.Mutex_Type;

private

end Kit.Db.Kit_Float_Impl;
