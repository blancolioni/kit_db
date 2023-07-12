with Kit.Mutex;
with System.Storage_Elements;
with Kit.Strings;

private package Kit.Db.Kit_Type_Impl is

   Kit_Type_Magic : constant  := 228631241;

   type Kit_Type_Database_Record is
      record
         Magic           : Integer := Kit_Type_Magic;
         Deleted         : Boolean := False;
         Kit_Root_Record : Kit_Root_Record_Reference := 0;
         Size            : Integer := 0;
         Name            : Kit.Strings.String_Type (64);
      end record;

   function Disk_Storage_Units return System.Storage_Elements.Storage_Count;
   procedure Read
     (Ref  : Marlowe.Database_Index;
      Item :    out Kit_Type_Database_Record);
   procedure Write
     (Ref  : Marlowe.Database_Index;
      Item : Kit_Type_Database_Record);
   Top_Record_Key_Mutex : Kit.Mutex.Mutex_Type;
   T1_Idx_Key_Mutex : Kit.Mutex.Mutex_Type;
   Name_Key_Mutex : Kit.Mutex.Mutex_Type;
   File_Mutex : Kit.Mutex.Mutex_Type;

private

end Kit.Db.Kit_Type_Impl;
