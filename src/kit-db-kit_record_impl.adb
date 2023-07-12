with Marlowe.Key_Storage;
with Kit.Db.Marlowe_Keys;

package body Kit.Db.Kit_Record_Impl is

   ------------------------
   -- Disk_Storage_Units --
   ------------------------

   function Disk_Storage_Units
      return System.Storage_Elements.Storage_Count is
      use type System.Storage_Elements.Storage_Offset;
   begin
      return Kit_Record_Database_Record'Size / System.Storage_Unit;
   end Disk_Storage_Units;

   ----------
   -- Read --
   ----------

   procedure Read
     (Ref  : Marlowe.Database_Index;
      Item :    out Kit_Record_Database_Record)
   is
      Storage : System.Storage_Elements.Storage_Array (0 .. 83);
   begin
      Marlowe_Keys.Handle.Get_Record
        (Kit_Record_Table_Index,
         Ref,
         Storage'Address);
      declare
         T : Marlowe.Database_Index;
      begin
         Marlowe.Key_Storage.From_Storage (T, Storage (4 .. 11));
         Item.Kit_Root_Record := Kit_Root_Record_Reference (T);
      end;
      Marlowe.Key_Storage.Bounded_String_From_Storage
        (Item.Name.Text,
         Item.Name.Length,
         Storage (12 .. 75));
      Marlowe.Key_Storage.From_Storage (Item.Table_Index,
         Storage (76 .. 79));
      Marlowe.Key_Storage.From_Storage (Item.Record_Length,
         Storage (80 .. 83));
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Ref  : Marlowe.Database_Index;
      Item : Kit_Record_Database_Record)
   is
      Storage : System.Storage_Elements.Storage_Array (0 .. 83);
   begin
      Marlowe.Key_Storage.To_Storage
         (Marlowe.Database_Index (Item.Kit_Root_Record), Storage (4 .. 11));
      Marlowe.Key_Storage.Bounded_String_To_Storage
        (Item.Name.Text (1 .. Item.Name.Length),
         Storage (12 .. 75));
      Marlowe.Key_Storage.To_Storage (Item.Table_Index, Storage (76 .. 79));
      Marlowe.Key_Storage.To_Storage (Item.Record_Length,
         Storage (80 .. 83));
      Marlowe_Keys.Handle.Write_Record
        (Kit_Record_Table_Index,
         Ref,
         Storage'Address);
   end Write;

end Kit.Db.Kit_Record_Impl;
