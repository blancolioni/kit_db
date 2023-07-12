with Kit.Mutex;
with System.Storage_Elements;

private package Kit.Db.Kit_Record_Base_Impl is

   Kit_Record_Base_Magic : constant  := 1146515157;

   type Kit_Record_Base_Database_Record is
      record
         Magic           : Integer := Kit_Record_Base_Magic;
         Deleted         : Boolean := False;
         Kit_Root_Record : Kit_Root_Record_Reference := 0;
         Offset          : Integer := 0;
         Base            : Kit_Record_Reference := 0;
         Derived         : Kit_Record_Reference := 0;
      end record;

   function Disk_Storage_Units return System.Storage_Elements.Storage_Count;
   procedure Read
     (Ref  : Marlowe.Database_Index;
      Item :    out Kit_Record_Base_Database_Record);
   procedure Write
     (Ref  : Marlowe.Database_Index;
      Item : Kit_Record_Base_Database_Record);
   Top_Record_Key_Mutex : Kit.Mutex.Mutex_Type;
   T1_Idx_Key_Mutex : Kit.Mutex.Mutex_Type;
   Base_Key_Mutex : Kit.Mutex.Mutex_Type;
   Derived_Key_Mutex : Kit.Mutex.Mutex_Type;
   Base_Record_Key_Mutex : Kit.Mutex.Mutex_Type;
   File_Mutex : Kit.Mutex.Mutex_Type;
   function Base_Record_To_Storage
     (Base    : Kit_Record_Reference;
      Derived : Kit_Record_Reference)
   return System.Storage_Elements.Storage_Array;
   function Partial_Base_Record_To_Storage
     (Fill_Low : Boolean;
      Base     : Kit_Record_Reference)
   return System.Storage_Elements.Storage_Array;

private

end Kit.Db.Kit_Record_Base_Impl;
