with Kit.Mutex;
with System.Storage_Elements;
with Kit.Strings;

private package Kit.Db.Kit_Record_Impl is

   Kit_Record_Magic : constant  := 1962384374;

   type Kit_Record_Database_Record is
      record
         Magic           : Integer := Kit_Record_Magic;
         Deleted         : Boolean := False;
         Kit_Root_Record : Kit_Root_Record_Reference := 0;
         Name            : Kit.Strings.String_Type (64);
         Table_Index     : Integer := 1;
         Record_Length   : Integer := 0;
      end record;

   function Disk_Storage_Units return System.Storage_Elements.Storage_Count;
   procedure Read
     (Ref  : Marlowe.Database_Index;
      Item :    out Kit_Record_Database_Record);
   procedure Write
     (Ref  : Marlowe.Database_Index;
      Item : Kit_Record_Database_Record);
   Top_Record_Key_Mutex : Kit.Mutex.Mutex_Type;
   T1_Idx_Key_Mutex : Kit.Mutex.Mutex_Type;
   Name_Key_Mutex : Kit.Mutex.Mutex_Type;
   Table_Index_Key_Mutex : Kit.Mutex.Mutex_Type;
   File_Mutex : Kit.Mutex.Mutex_Type;

private

end Kit.Db.Kit_Record_Impl;
