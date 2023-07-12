with Marlowe.Key_Storage;
with Kit.Db.Marlowe_Keys;

package body Kit.Db.Kit_Field_Impl is

   ------------------------
   -- Disk_Storage_Units --
   ------------------------

   function Disk_Storage_Units
      return System.Storage_Elements.Storage_Count is
      use type System.Storage_Elements.Storage_Offset;
   begin
      return Kit_Field_Database_Record'Size / System.Storage_Unit;
   end Disk_Storage_Units;

   ------------------------------
   -- Display_Field_To_Storage --
   ------------------------------

   function Display_Field_To_Storage
     (Kit_Record : Kit_Record_Reference;
      Display    : Boolean)
   return System.Storage_Elements.Storage_Array
   is
      use type System.Storage_Elements.Storage_Array;
   begin
      return Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
         (Kit_Record)) & Marlowe.Key_Storage.To_Storage_Array (Boolean'Pos
         (Display), 1);
   end Display_Field_To_Storage;

   --------------------------------------
   -- Partial_Display_Field_To_Storage --
   --------------------------------------

   function Partial_Display_Field_To_Storage
     (Fill_Low   : Boolean;
      Kit_Record : Kit_Record_Reference)
   return System.Storage_Elements.Storage_Array
   is
      use type System.Storage_Elements.Storage_Array;
      Partial_Key_Suffix : constant
         System.Storage_Elements.Storage_Array (1 .. 1) :=
         (others => (if Fill_Low then 0 else 255));
   begin
      return Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
         (Kit_Record)) & Partial_Key_Suffix;
   end Partial_Display_Field_To_Storage;

   -------------------------------------
   -- Partial_Record_Field_To_Storage --
   -------------------------------------

   function Partial_Record_Field_To_Storage
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
   end Partial_Record_Field_To_Storage;

   ----------
   -- Read --
   ----------

   procedure Read
     (Ref  : Marlowe.Database_Index;
      Item :    out Kit_Field_Database_Record)
   is
      Storage : System.Storage_Elements.Storage_Array (0 .. 104);
   begin
      Marlowe_Keys.Handle.Get_Record
        (Kit_Field_Table_Index,
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
         T : Marlowe.Database_Index;
      begin
         Marlowe.Key_Storage.From_Storage (T, Storage (84 .. 91));
         Item.Field_Type := Kit_Type_Reference (T);
      end;
      Marlowe.Key_Storage.From_Storage (Item.Field_Offset,
         Storage (92 .. 95));
      Marlowe.Key_Storage.From_Storage (Item.Field_Length,
         Storage (96 .. 99));
      declare
         T : Marlowe.Key_Storage.Unsigned_Integer;
      begin
         Marlowe.Key_Storage.From_Storage (T, Storage (100 .. 100));
         Item.Created := Boolean'Val (T);
      end;
      declare
         T : Marlowe.Key_Storage.Unsigned_Integer;
      begin
         Marlowe.Key_Storage.From_Storage (T, Storage (101 .. 101));
         Item.Readable := Boolean'Val (T);
      end;
      declare
         T : Marlowe.Key_Storage.Unsigned_Integer;
      begin
         Marlowe.Key_Storage.From_Storage (T, Storage (102 .. 102));
         Item.Writeable := Boolean'Val (T);
      end;
      declare
         T : Marlowe.Key_Storage.Unsigned_Integer;
      begin
         Marlowe.Key_Storage.From_Storage (T, Storage (103 .. 103));
         Item.Display := Boolean'Val (T);
      end;
      declare
         T : Marlowe.Key_Storage.Unsigned_Integer;
      begin
         Marlowe.Key_Storage.From_Storage (T, Storage (104 .. 104));
         Item.Base_Ref := Boolean'Val (T);
      end;
   end Read;

   -----------------------------
   -- Record_Field_To_Storage --
   -----------------------------

   function Record_Field_To_Storage
     (Kit_Record : Kit_Record_Reference;
      Name       : String)
   return System.Storage_Elements.Storage_Array
   is
      use type System.Storage_Elements.Storage_Array;
   begin
      return Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
         (Kit_Record)) & Marlowe.Key_Storage.To_Storage_Array (Name, 64);
   end Record_Field_To_Storage;

   -----------
   -- Write --
   -----------

   procedure Write
     (Ref  : Marlowe.Database_Index;
      Item : Kit_Field_Database_Record)
   is
      Storage : System.Storage_Elements.Storage_Array (0 .. 104);
   begin
      Marlowe.Key_Storage.To_Storage
         (Marlowe.Database_Index (Item.Kit_Root_Record), Storage (4 .. 11));
      Marlowe.Key_Storage.Bounded_String_To_Storage
        (Item.Name.Text (1 .. Item.Name.Length),
         Storage (12 .. 75));
      Marlowe.Key_Storage.To_Storage
         (Marlowe.Database_Index (Item.Kit_Record), Storage (76 .. 83));
      Marlowe.Key_Storage.To_Storage
         (Marlowe.Database_Index (Item.Field_Type), Storage (84 .. 91));
      Marlowe.Key_Storage.To_Storage (Item.Field_Offset, Storage (92 .. 95));
      Marlowe.Key_Storage.To_Storage (Item.Field_Length, Storage (96 .. 99));
      declare
         T : constant Marlowe.Key_Storage.Unsigned_Integer :=
            Boolean'Pos (Item.Created);
      begin
         Marlowe.Key_Storage.To_Storage (T, Storage (100 .. 100));
      end;
      declare
         T : constant Marlowe.Key_Storage.Unsigned_Integer :=
            Boolean'Pos (Item.Readable);
      begin
         Marlowe.Key_Storage.To_Storage (T, Storage (101 .. 101));
      end;
      declare
         T : constant Marlowe.Key_Storage.Unsigned_Integer :=
            Boolean'Pos (Item.Writeable);
      begin
         Marlowe.Key_Storage.To_Storage (T, Storage (102 .. 102));
      end;
      declare
         T : constant Marlowe.Key_Storage.Unsigned_Integer :=
            Boolean'Pos (Item.Display);
      begin
         Marlowe.Key_Storage.To_Storage (T, Storage (103 .. 103));
      end;
      declare
         T : constant Marlowe.Key_Storage.Unsigned_Integer :=
            Boolean'Pos (Item.Base_Ref);
      begin
         Marlowe.Key_Storage.To_Storage (T, Storage (104 .. 104));
      end;
      Marlowe_Keys.Handle.Write_Record
        (Kit_Field_Table_Index,
         Ref,
         Storage'Address);
   end Write;

end Kit.Db.Kit_Field_Impl;
