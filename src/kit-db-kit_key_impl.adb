with Marlowe.Key_Storage;
with Kit.Db.Marlowe_Keys;

package body Kit.Db.Kit_Key_Impl is

   ------------------------
   -- Disk_Storage_Units --
   ------------------------

   function Disk_Storage_Units
      return System.Storage_Elements.Storage_Count is
      use type System.Storage_Elements.Storage_Offset;
   begin
      return Kit_Key_Database_Record'Size / System.Storage_Unit;
   end Disk_Storage_Units;

   -----------------------------------
   -- Partial_Record_Key_To_Storage --
   -----------------------------------

   function Partial_Record_Key_To_Storage
     (Fill_Low   : Boolean;
      Kit_Record : Kit_Record_Reference)
   return System.Storage_Elements.Storage_Array
   is
      use type System.Storage_Elements.Storage_Array;
      Partial_Key_Suffix : constant
         System.Storage_Elements.Storage_Array (1 .. 64) :=
         (others => (if Fill_Low then 0 else 255));
   begin
      return Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
         (Kit_Record)) & Partial_Key_Suffix;
   end Partial_Record_Key_To_Storage;

   ----------
   -- Read --
   ----------

   procedure Read
     (Ref  : Marlowe.Database_Index;
      Item :    out Kit_Key_Database_Record)
   is
      Storage : System.Storage_Elements.Storage_Array (0 .. 88);
   begin
      Marlowe_Keys.Handle.Get_Record
        (Kit_Key_Table_Index,
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
      declare
         T : Marlowe.Database_Index;
      begin
         Marlowe.Key_Storage.From_Storage (T, Storage (76 .. 83));
         Item.Kit_Record := Kit_Record_Reference (T);
      end;
      declare
         T : Marlowe.Key_Storage.Unsigned_Integer;
      begin
         Marlowe.Key_Storage.From_Storage (T, Storage (84 .. 84));
         Item.Is_Unique := Boolean'Val (T);
      end;
      Marlowe.Key_Storage.From_Storage (Item.Length, Storage (85 .. 88));
   end Read;

   ---------------------------
   -- Record_Key_To_Storage --
   ---------------------------

   function Record_Key_To_Storage
     (Kit_Record : Kit_Record_Reference;
      Name       : String)
   return System.Storage_Elements.Storage_Array
   is
      use type System.Storage_Elements.Storage_Array;
   begin
      return Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
         (Kit_Record)) & Marlowe.Key_Storage.To_Storage_Array (Name, 64);
   end Record_Key_To_Storage;

   -----------
   -- Write --
   -----------

   procedure Write
     (Ref  : Marlowe.Database_Index;
      Item : Kit_Key_Database_Record)
   is
      Storage : System.Storage_Elements.Storage_Array (0 .. 88);
   begin
      Marlowe.Key_Storage.To_Storage
         (Marlowe.Database_Index (Item.Kit_Root_Record), Storage (4 .. 11));
      Marlowe.Key_Storage.Bounded_String_To_Storage
        (Item.Name.Text (1 .. Item.Name.Length),
         Storage (12 .. 75));
      Marlowe.Key_Storage.To_Storage
         (Marlowe.Database_Index (Item.Kit_Record), Storage (76 .. 83));
      declare
         T : constant Marlowe.Key_Storage.Unsigned_Integer :=
            Boolean'Pos (Item.Is_Unique);
      begin
         Marlowe.Key_Storage.To_Storage (T, Storage (84 .. 84));
      end;
      Marlowe.Key_Storage.To_Storage (Item.Length, Storage (85 .. 88));
      Marlowe_Keys.Handle.Write_Record
        (Kit_Key_Table_Index,
         Ref,
         Storage'Address);
   end Write;

end Kit.Db.Kit_Key_Impl;
