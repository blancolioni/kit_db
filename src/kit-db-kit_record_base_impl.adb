with Marlowe.Key_Storage;
with Kit.Db.Marlowe_Keys;

package body Kit.Db.Kit_Record_Base_Impl is

   ----------------------------
   -- Base_Record_To_Storage --
   ----------------------------

   function Base_Record_To_Storage
     (Base    : Kit_Record_Reference;
      Derived : Kit_Record_Reference)
   return System.Storage_Elements.Storage_Array
   is
      use type System.Storage_Elements.Storage_Array;
   begin
      return Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
         (Base)) & Marlowe.Key_Storage.To_Storage_Array
         (Marlowe.Database_Index (Derived));
   end Base_Record_To_Storage;

   ------------------------
   -- Disk_Storage_Units --
   ------------------------

   function Disk_Storage_Units
      return System.Storage_Elements.Storage_Count is
      use type System.Storage_Elements.Storage_Offset;
   begin
      return Kit_Record_Base_Database_Record'Size / System.Storage_Unit;
   end Disk_Storage_Units;

   ------------------------------------
   -- Partial_Base_Record_To_Storage --
   ------------------------------------

   function Partial_Base_Record_To_Storage
     (Fill_Low : Boolean;
      Base     : Kit_Record_Reference)
   return System.Storage_Elements.Storage_Array
   is
      use type System.Storage_Elements.Storage_Array;
      Partial_Key_Suffix : constant
         System.Storage_Elements.Storage_Array (1 .. 8) :=
         (others => (if Fill_Low then 0 else 255));
   begin
      return Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
         (Base)) & Partial_Key_Suffix;
   end Partial_Base_Record_To_Storage;

   ----------
   -- Read --
   ----------

   procedure Read
     (Ref  : Marlowe.Database_Index;
      Item :    out Kit_Record_Base_Database_Record)
   is
      Storage : System.Storage_Elements.Storage_Array (0 .. 31);
   begin
      Marlowe_Keys.Handle.Get_Record
        (Kit_Record_Base_Table_Index,
         Ref,
         Storage'Address);
      declare
         T : Marlowe.Database_Index;
      begin
         Marlowe.Key_Storage.From_Storage (T, Storage (4 .. 11));
         Item.Kit_Root_Record := Kit_Root_Record_Reference (T);
      end;
      Marlowe.Key_Storage.From_Storage (Item.Offset, Storage (12 .. 15));
      declare
         T : Marlowe.Database_Index;
      begin
         Marlowe.Key_Storage.From_Storage (T, Storage (16 .. 23));
         Item.Base := Kit_Record_Reference (T);
      end;
      declare
         T : Marlowe.Database_Index;
      begin
         Marlowe.Key_Storage.From_Storage (T, Storage (24 .. 31));
         Item.Derived := Kit_Record_Reference (T);
      end;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Ref  : Marlowe.Database_Index;
      Item : Kit_Record_Base_Database_Record)
   is
      Storage : System.Storage_Elements.Storage_Array (0 .. 31);
   begin
      Marlowe.Key_Storage.To_Storage
         (Marlowe.Database_Index (Item.Kit_Root_Record), Storage (4 .. 11));
      Marlowe.Key_Storage.To_Storage (Item.Offset, Storage (12 .. 15));
      Marlowe.Key_Storage.To_Storage (Marlowe.Database_Index (Item.Base),
         Storage (16 .. 23));
      Marlowe.Key_Storage.To_Storage (Marlowe.Database_Index (Item.Derived),
         Storage (24 .. 31));
      Marlowe_Keys.Handle.Write_Record
        (Kit_Record_Base_Table_Index,
         Ref,
         Storage'Address);
   end Write;

end Kit.Db.Kit_Record_Base_Impl;
