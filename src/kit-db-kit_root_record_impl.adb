with Marlowe.Key_Storage;
with Kit.Db.Marlowe_Keys;

package body Kit.Db.Kit_Root_Record_Impl is

   ------------------------
   -- Disk_Storage_Units --
   ------------------------

   function Disk_Storage_Units
      return System.Storage_Elements.Storage_Count is
      use type System.Storage_Elements.Storage_Offset;
   begin
      return Kit_Root_Record_Database_Record'Size / System.Storage_Unit;
   end Disk_Storage_Units;

   ----------
   -- Read --
   ----------

   procedure Read
     (Ref  : Marlowe.Database_Index;
      Item :    out Kit_Root_Record_Database_Record)
   is
      Storage : System.Storage_Elements.Storage_Array (0 .. 7);
   begin
      Marlowe_Keys.Handle.Get_Record
        (Kit_Root_Record_Table_Index,
         Ref,
         Storage'Address);
      declare
         T : Marlowe.Key_Storage.Unsigned_Integer;
      begin
         Marlowe.Key_Storage.From_Storage (T, Storage (4 .. 7));
         Item.Top_Record := Record_Type'Val (T);
      end;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Ref  : Marlowe.Database_Index;
      Item : Kit_Root_Record_Database_Record)
   is
      Storage : System.Storage_Elements.Storage_Array (0 .. 7);
   begin
      declare
         T : constant Marlowe.Key_Storage.Unsigned_Integer :=
            Record_Type'Pos (Item.Top_Record);
      begin
         Marlowe.Key_Storage.To_Storage (T, Storage (4 .. 7));
      end;
      Marlowe_Keys.Handle.Write_Record
        (Kit_Root_Record_Table_Index,
         Ref,
         Storage'Address);
   end Write;

end Kit.Db.Kit_Root_Record_Impl;
