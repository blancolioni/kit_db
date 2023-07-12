with Marlowe.Key_Storage;
with Kit.Db.Marlowe_Keys;

package body Kit.Db.Kit_Display_Field_Impl is

   ------------------------
   -- Disk_Storage_Units --
   ------------------------

   function Disk_Storage_Units
      return System.Storage_Elements.Storage_Count is
      use type System.Storage_Elements.Storage_Offset;
   begin
      return Kit_Display_Field_Database_Record'Size / System.Storage_Unit;
   end Disk_Storage_Units;

   ----------
   -- Read --
   ----------

   procedure Read
     (Ref  : Marlowe.Database_Index;
      Item :    out Kit_Display_Field_Database_Record)
   is
      Storage : System.Storage_Elements.Storage_Array (0 .. 27);
   begin
      Marlowe_Keys.Handle.Get_Record
        (Kit_Display_Field_Table_Index,
         Ref,
         Storage'Address);
      declare
         T : Marlowe.Database_Index;
      begin
         Marlowe.Key_Storage.From_Storage (T, Storage (4 .. 11));
         Item.Kit_Root_Record := Kit_Root_Record_Reference (T);
      end;
      declare
         T : Marlowe.Database_Index;
      begin
         Marlowe.Key_Storage.From_Storage (T, Storage (12 .. 19));
         Item.Kit_Record := Kit_Record_Reference (T);
      end;
      declare
         T : Marlowe.Database_Index;
      begin
         Marlowe.Key_Storage.From_Storage (T, Storage (20 .. 27));
         Item.Kit_Field := Kit_Field_Reference (T);
      end;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Ref  : Marlowe.Database_Index;
      Item : Kit_Display_Field_Database_Record)
   is
      Storage : System.Storage_Elements.Storage_Array (0 .. 27);
   begin
      Marlowe.Key_Storage.To_Storage
         (Marlowe.Database_Index (Item.Kit_Root_Record), Storage (4 .. 11));
      Marlowe.Key_Storage.To_Storage
         (Marlowe.Database_Index (Item.Kit_Record), Storage (12 .. 19));
      Marlowe.Key_Storage.To_Storage
         (Marlowe.Database_Index (Item.Kit_Field), Storage (20 .. 27));
      Marlowe_Keys.Handle.Write_Record
        (Kit_Display_Field_Table_Index,
         Ref,
         Storage'Address);
   end Write;

end Kit.Db.Kit_Display_Field_Impl;
