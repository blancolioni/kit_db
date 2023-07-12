with Kit.Mutex;
with System.Storage_Elements;
with Kit.Strings;

private package Kit.Db.Kit_Key_Impl is

   Kit_Key_Magic : constant  := 1204280153;

   type Kit_Key_Database_Record is
      record
         Magic           : Integer := Kit_Key_Magic;
         Deleted         : Boolean := False;
         Kit_Root_Record : Kit_Root_Record_Reference := 0;
         Name            : Kit.Strings.String_Type (64);
         Kit_Record      : Kit_Record_Reference := 0;
         Is_Unique       : Boolean := False;
         Length          : Integer := 0;
      end record;

   function Disk_Storage_Units return System.Storage_Elements.Storage_Count;
   procedure Read
     (Ref  : Marlowe.Database_Index;
      Item :    out Kit_Key_Database_Record);
   procedure Write
     (Ref  : Marlowe.Database_Index;
      Item : Kit_Key_Database_Record);
   Top_Record_Key_Mutex : Kit.Mutex.Mutex_Type;
   T1_Idx_Key_Mutex : Kit.Mutex.Mutex_Type;
   Kit_Record_Key_Mutex : Kit.Mutex.Mutex_Type;
   Record_Key_Key_Mutex : Kit.Mutex.Mutex_Type;
   File_Mutex : Kit.Mutex.Mutex_Type;
   function Record_Key_To_Storage
     (Kit_Record : Kit_Record_Reference;
      Name       : String)
   return System.Storage_Elements.Storage_Array;
   function Partial_Record_Key_To_Storage
     (Fill_Low   : Boolean;
      Kit_Record : Kit_Record_Reference)
   return System.Storage_Elements.Storage_Array;

private

end Kit.Db.Kit_Key_Impl;
