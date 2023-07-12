with Ada.Finalization;
with System.Storage_Elements;
with Marlowe.Data_Stores;
with Marlowe.Key_Storage;
with Kit.Cache;
with Kit.Notifier;
with Kit.Strings;
with Kit.Db.Kit_Deferred_Keys;
with Kit.Db.Marlowe_Keys;
with Kit.Db.Kit_Root_Record_Cache;
with Kit.Db.Kit_Root_Record_Impl;
with Kit.Db.Kit_Field_Impl;
with Kit.Db.Kit_Field_Cache;

package body Kit.Db.Kit_Field is

   type Kit_Field_Implementation is
        limited new Ada.Finalization.Limited_Controlled
     and Kit_Field_Interface with
      record
         Finished        : Boolean;
         Forward         : Boolean;
         Read_Only       : Boolean;
         Created         : Boolean;
         Deleted         : Boolean;
         Scanning        : Boolean;
         Has_Finish      : Boolean;
         Start_Closed    : Boolean;
         Finish_Closed   : Boolean;
         Using_Key       : Boolean;
         Using_Key_Value : Boolean;
         Key_Ref         : Marlowe.Data_Stores.Key_Reference;
         M_Index         : Kit_Field_Reference;
         T1_Data         :
            Kit_Root_Record_Impl.Kit_Root_Record_Database_Record;
         T15_Data        : Kit_Field_Impl.Kit_Field_Database_Record;
      end record;
   overriding function Has_Element (Item : Kit_Field_Implementation)
      return Boolean;
   overriding procedure X_Lock (Item : Kit_Field_Implementation);
   overriding function Get_Kit_Root_Record_Reference (Item :
      Kit_Field_Implementation) return Kit_Root_Record_Reference;
   overriding function Get_Kit_Field_Reference (Item :
      Kit_Field_Implementation) return Kit_Field_Reference;
   overriding function Top_Record (Item : Kit_Field_Implementation)
      return Record_Type;
   overriding function Name (Item : Kit_Field_Implementation) return String;
   overriding function Kit_Record (Item : Kit_Field_Implementation)
      return Kit_Record_Reference;
   overriding function Field_Type (Item : Kit_Field_Implementation)
      return Kit_Type_Reference;
   overriding function Field_Offset (Item : Kit_Field_Implementation)
      return Integer;
   overriding function Field_Length (Item : Kit_Field_Implementation)
      return Integer;
   overriding function Created (Item : Kit_Field_Implementation)
      return Boolean;
   overriding function Readable (Item : Kit_Field_Implementation)
      return Boolean;
   overriding function Writeable (Item : Kit_Field_Implementation)
      return Boolean;
   overriding function Display (Item : Kit_Field_Implementation)
      return Boolean;
   overriding function Base_Ref (Item : Kit_Field_Implementation)
      return Boolean;
   overriding function Identity (Item : Kit_Field_Implementation)
      return String;
   overriding function Get
     (Item  : Kit_Field_Implementation;
      Field : String)
   return String;
   type Selection_Access is access Selection;
   type Iterator is
        new Selection_Iterator_Interfaces.Reversible_Iterator with
      record
         Container : Selection_Access;
      end record;
   overriding function First (Object : Iterator) return Cursor;

   overriding function Last (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor)
   return Cursor;
   overriding function Previous
     (Object   : Iterator;
      Position : Cursor)
   return Cursor;
   type Kit_Field_Table_Notify_Record is
        new Kit.Notifier.Table_Notify_Interface with
      record
         Handler : Kit_Field_Table_Notify_Handler;
      end record;
   overriding procedure Notify_Table_Change (Handle :
      Kit_Field_Table_Notify_Record);
   type Kit_Field_Notify_Record is
        new Kit.Notifier.Record_Notify_Interface with
      record
         Handler : Kit_Field_Notify_Handler;
      end record;
   overriding procedure Notify_Record_Change
     (Handle         : Kit_Field_Notify_Record;
      Changed_Record : Marlowe.Database_Index);
   type Kit_Field_Update_Implementation is
        limited new Kit_Field_Implementation
     and Kit_Field_Update_Interface
     and Record_Update_Interface with null record;
   overriding procedure Initialize (Item :
      in out Kit_Field_Update_Implementation);
   overriding procedure Finalize (Item :
      in out Kit_Field_Update_Implementation);
   overriding procedure Delete (Item :
      in out Kit_Field_Update_Implementation);
   overriding procedure Set
     (Item  : in out Kit_Field_Update_Implementation;
      Field : String;
      Value : String);
   overriding procedure Set_Name
     (Item  : in out Kit_Field_Update_Implementation;
      Value : String);
   overriding procedure Set_Kit_Record
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Kit_Record_Reference);
   overriding procedure Set_Kit_Record
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Kit.Db.Kit_Record.Kit_Record_Type);
   overriding procedure Set_Field_Type
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Kit_Type_Reference);
   overriding procedure Set_Field_Type
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Kit.Db.Kit_Type.Kit_Type_Type);
   overriding procedure Set_Field_Offset
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Integer);
   overriding procedure Set_Field_Length
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Integer);
   overriding procedure Set_Created
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Boolean);
   overriding procedure Set_Readable
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Boolean);
   overriding procedure Set_Writeable
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Boolean);
   overriding procedure Set_Display
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Boolean);
   overriding procedure Set_Base_Ref
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Boolean);

   --------------
   -- Base_Ref --
   --------------

   overriding function Base_Ref (Item : Kit_Field_Implementation)
      return Boolean is
      Result : Boolean
        renames Item.T15_Data.Base_Ref;
   begin
      return Result;
   end Base_Ref;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Selection;
      Position  : Cursor)
   return Constant_Reference_Type
   is
   begin
      return (Element => Container.Elements.Constant_Reference (Position
      .Current).Element);
   end Constant_Reference;

   ------------
   -- Create --
   ------------

   procedure Create
     (Name         : String;
      Kit_Record   : Kit_Record_Reference;
      Field_Type   : Kit_Type_Reference;
      Field_Offset : Integer;
      Field_Length : Integer;
      Created      : Boolean;
      Readable     : Boolean;
      Writeable    : Boolean;
      Display      : Boolean;
      Base_Ref     : Boolean)
   is
      Result : Kit_Field_Update := Create;
   begin
      Result.Set_Name (Name);
      Result.Set_Kit_Record (Kit_Record);
      Result.Set_Field_Type (Field_Type);
      Result.Set_Field_Offset (Field_Offset);
      Result.Set_Field_Length (Field_Length);
      Result.Set_Created (Created);
      Result.Set_Readable (Readable);
      Result.Set_Writeable (Writeable);
      Result.Set_Display (Display);
      Result.Set_Base_Ref (Base_Ref);
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Kit_Field_Update is
      T1_Access : Kit_Root_Record_Cache.Cache_Access;
      T15_Access : Kit_Field_Cache.Cache_Access;
   begin
      return Result : Kit_Field_Update_Implementation do
         Result.Finished := False;
         Result.Created := True;
         Result.Deleted := False;
         Result.Scanning := False;
         Result.Read_Only := False;
         Result.Using_Key := False;
         Result.M_Index := 0;
         Memory_Mutex.Lock;
         T1_Access := new Kit_Root_Record_Cache.Cache_Record;
         T1_Access.Db.Top_Record := R_Kit_Field;
         T15_Access := new Kit_Field_Cache.Cache_Record;
         Memory_Mutex.Unlock;
         Kit_Root_Record_Impl.File_Mutex.Lock;
         T15_Access.Db.Kit_Root_Record := Kit_Root_Record_Reference
            (Marlowe_Keys.Handle.Insert_Record (1));
         T1_Access.Initialise (1, Marlowe.Database_Index
            (T15_Access.Db.Kit_Root_Record));
         Kit.Cache.Insert (Kit.Cache.Cache_Entry (T1_Access));
         Kit_Root_Record_Impl.Write (Marlowe.Database_Index
            (T15_Access.Db.Kit_Root_Record), T1_Access.Db);
         T1_Access.X_Lock;
         Kit_Root_Record_Impl.File_Mutex.Unlock;
         Kit_Field_Impl.File_Mutex.Lock;
         Result.M_Index := Kit_Field_Reference
            (Marlowe_Keys.Handle.Insert_Record (15));
         T15_Access.Initialise (15, Marlowe.Database_Index (Result.M_Index));
         Kit.Cache.Insert (Kit.Cache.Cache_Entry (T15_Access));
         Kit_Field_Impl.Write (Marlowe.Database_Index (Result.M_Index),
            T15_Access.Db);
         T15_Access.X_Lock;
         Kit_Field_Impl.File_Mutex.Unlock;
         Result.T1_Data := T1_Access.Db;
         Result.T15_Data := T15_Access.Db;
         Result.Read_Only := False;
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Name         : String;
      Kit_Record   : Kit_Record_Reference;
      Field_Type   : Kit_Type_Reference;
      Field_Offset : Integer;
      Field_Length : Integer;
      Created      : Boolean;
      Readable     : Boolean;
      Writeable    : Boolean;
      Display      : Boolean;
      Base_Ref     : Boolean)
   return Kit_Field_Reference
   is
      Result : Kit_Field_Update := Create;
   begin
      Result.Set_Name (Name);
      Result.Set_Kit_Record (Kit_Record);
      Result.Set_Field_Type (Field_Type);
      Result.Set_Field_Offset (Field_Offset);
      Result.Set_Field_Length (Field_Length);
      Result.Set_Created (Created);
      Result.Set_Readable (Readable);
      Result.Set_Writeable (Writeable);
      Result.Set_Display (Display);
      Result.Set_Base_Ref (Base_Ref);
      return Result.Get_Kit_Field_Reference;
   end Create;

   -------------
   -- Created --
   -------------

   overriding function Created (Item : Kit_Field_Implementation)
      return Boolean is
      Result : Boolean
        renames Item.T15_Data.Created;
   begin
      return Result;
   end Created;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete (Item :
      in out Kit_Field_Update_Implementation) is
   begin
      Database_Mutex.Shared_Lock;
      Item.Deleted := True;
      Database_Mutex.Shared_Unlock;
   end Delete;

   -------------
   -- Display --
   -------------

   overriding function Display (Item : Kit_Field_Implementation)
      return Boolean is
      Result : Boolean
        renames Item.T15_Data.Display;
   begin
      return Result;
   end Display;

   -------------
   -- Element --
   -------------

   function Element (Item : Cursor) return Kit_Field_Reference is
   begin
      return List_Of_References.Element (Item.Current);
   end Element;

   ------------------
   -- Field_Length --
   ------------------

   overriding function Field_Length (Item : Kit_Field_Implementation)
      return Integer is
      Result : Integer
        renames Item.T15_Data.Field_Length;
   begin
      return Result;
   end Field_Length;

   ------------------
   -- Field_Offset --
   ------------------

   overriding function Field_Offset (Item : Kit_Field_Implementation)
      return Integer is
      Result : Integer
        renames Item.T15_Data.Field_Offset;
   begin
      return Result;
   end Field_Offset;

   ----------------
   -- Field_Type --
   ----------------

   overriding function Field_Type (Item : Kit_Field_Implementation)
      return Kit_Type_Reference is
      Result : Kit_Type_Reference
        renames Item.T15_Data.Field_Type;
   begin
      return Result;
   end Field_Type;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Item :
      in out Kit_Field_Update_Implementation) is
      use type System.Storage_Elements.Storage_Array;
   begin
      if Item.M_Index = 0 then
         return;
      end if;
      if Item.Deleted then
         Marlowe_Keys.Handle.Delete (Marlowe_Keys.T1_Top_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Item.Top_Record), 4)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.T15_Data.Kit_Root_Record)));
         Marlowe_Keys.Handle.Delete (Marlowe_Keys.T15_Top_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Item.Top_Record), 4)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Delete (Marlowe_Keys.T15_T1_Idx_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.T15_Data.Kit_Root_Record))
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Delete (Marlowe_Keys.T15_Kit_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.Kit_Record))
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Delete (Marlowe_Keys.T15_Record_Field_Ref,
            Kit_Field_Impl.Record_Field_To_Storage (Item.Kit_Record,
            Item.Name)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Delete (Marlowe_Keys.T15_Display_Field_Ref,
            Kit_Field_Impl.Display_Field_To_Storage (Item.Kit_Record,
            Item.Display)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Item.T1_Data.Deleted := True;
         Item.T15_Data.Deleted := True;
         Kit.Notifier.Record_Deleted (1, Marlowe.Database_Index
            (Item.T15_Data.Kit_Root_Record));
         Kit.Notifier.Record_Deleted (15, Marlowe.Database_Index
            (Item.M_Index));
      end if;
      if not (Item.Read_Only or else Item.Created) or else Item.Deleted then
         Kit.Notifier.Record_Changed (1, Marlowe.Database_Index
            (Item.T15_Data.Kit_Root_Record));
         Kit.Notifier.Record_Changed (15, Marlowe.Database_Index
            (Item.M_Index));
      end if;
      if (not Item.Read_Only) and then Item.Created then
         Database_Mutex.Shared_Lock;
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T1_Top_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Item.Top_Record), 4)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.T15_Data.Kit_Root_Record)));
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T15_Top_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Item.Top_Record), 4)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T15_T1_Idx_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.T15_Data.Kit_Root_Record))
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T15_Kit_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.Kit_Record))
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T15_Record_Field_Ref,
            Kit_Field_Impl.Record_Field_To_Storage (Item.Kit_Record,
            Item.Name)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T15_Display_Field_Ref,
            Kit_Field_Impl.Display_Field_To_Storage (Item.Kit_Record,
            Item.Display)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Database_Mutex.Shared_Unlock;
         Kit.Notifier.Record_Created (1, Marlowe.Database_Index
            (Item.T15_Data.Kit_Root_Record));
         Kit.Notifier.Record_Created (15, Marlowe.Database_Index
            (Item.M_Index));
      end if;
      if not Item.Read_Only then
         declare
            T1_Access : constant Kit_Root_Record_Cache.Cache_Access :=
               Kit_Root_Record_Cache.Get (Marlowe.Database_Index
               (Item.T15_Data.Kit_Root_Record));
         begin
            T1_Access.Db := Item.T1_Data;
         end;
         declare
            T15_Access : constant Kit_Field_Cache.Cache_Access :=
               Kit_Field_Cache.Get (Marlowe.Database_Index (Item.M_Index));
         begin
            T15_Access.Db := Item.T15_Data;
         end;
      end if;
      if not Item.Read_Only then
         Kit_Root_Record_Cache.Unlock (Marlowe.Database_Index
            (Item.T15_Data.Kit_Root_Record));
         Kit_Field_Cache.Unlock (Marlowe.Database_Index (Item.M_Index));
      end if;
      Item.M_Index := 0;
      Item.Created := False;
   end Finalize;

   -----------
   -- First --
   -----------

   overriding function First (Object : Iterator) return Cursor is
   begin
      return (Current => Object.Container.Elements.First);
   end First;

   -----------
   -- First --
   -----------

   function First (Container : Selection) return Cursor is
   begin
      return (Current => Container.Elements.First);
   end First;

   ----------------------------
   -- First_By_Display_Field --
   ----------------------------

   function First_By_Display_Field
     (Kit_Record : Kit_Record_Reference;
      Display    : Boolean)
   return Kit_Field_Reference
   is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (15);
      Kit_Field_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Display_Field_Ref,
            Kit_Field_Impl.Display_Field_To_Storage (Kit_Record, Display)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First),
            Kit_Field_Impl.Display_Field_To_Storage (Kit_Record, Display)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last), Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         if M.Valid then
            Db_Index := Marlowe.Key_Storage.To_Database_Index (M.Get_Key);
         end if;
      end;
      Kit_Field_Impl.File_Mutex.Shared_Unlock;
      return Kit_Field_Reference (Db_Index);
   end First_By_Display_Field;

   -------------------------
   -- First_By_Kit_Record --
   -------------------------

   function First_By_Kit_Record (Kit_Record : Kit_Record_Reference)
      return Kit_Field_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (15);
      Kit_Field_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Kit_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Record)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First),
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Record)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last), Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         if M.Valid then
            Db_Index := Marlowe.Key_Storage.To_Database_Index (M.Get_Key);
         end if;
      end;
      Kit_Field_Impl.File_Mutex.Shared_Unlock;
      return Kit_Field_Reference (Db_Index);
   end First_By_Kit_Record;

   -------------------------
   -- First_By_Top_Record --
   -------------------------

   function First_By_Top_Record (Top_Record : Record_Type)
      return Kit_Field_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Field_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Top_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Top_Record), 4) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First),
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Top_Record), 4) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last), Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         if M.Valid then
            Db_Index := Marlowe.Key_Storage.To_Database_Index (M.Get_Key);
         end if;
      end;
      Kit_Field_Impl.File_Mutex.Shared_Unlock;
      return Kit_Field_Reference (Db_Index);
   end First_By_Top_Record;

   ---------
   -- Get --
   ---------

   function Get (Ref : Kit_Field_Reference) return Kit_Field_Type is
   begin
      return Result : Kit_Field_Implementation do
         Kit_Field_Impl.File_Mutex.Shared_Lock;
         Result.M_Index := Ref;
         if Result.M_Index /= Null_Kit_Field_Reference then
            Kit_Field_Cache.S_Lock (Marlowe.Database_Index (Result.M_Index));
            Result.T15_Data := Kit_Field_Cache.Get (Marlowe.Database_Index
               (Result.M_Index), False).Db;
            Kit_Root_Record_Cache.S_Lock (Marlowe.Database_Index
               (Result.T15_Data.Kit_Root_Record));
            Result.T1_Data := Kit_Root_Record_Cache.Get
               (Marlowe.Database_Index (Result.T15_Data.Kit_Root_Record),
               False).Db;
            Kit_Field_Cache.Unlock (Marlowe.Database_Index (Result.M_Index));
            Kit_Root_Record_Cache.Unlock (Marlowe.Database_Index
               (Result.T15_Data.Kit_Root_Record));
         end if;
         Result.Finished := False;
         Result.Using_Key_Value := False;
         Result.Scanning := False;
         Kit_Field_Impl.File_Mutex.Shared_Unlock;
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Item  : Kit_Field_Implementation;
      Field : String)
   return String
   is
   begin
      if Field = "" then
         raise Constraint_Error with
           "missing field name";
      elsif Field = "top_record" then
         return Record_Type'Image (Item.Top_Record);
      elsif Field = "kit_root_record" then
         raise Constraint_Error with
           "field Kit_Root_Record is not readable";
      elsif Field = "name" then
         return Item.Name;
      elsif Field = "kit_record" then
         return Kit_Record_Reference'Image (Item.Kit_Record);
      elsif Field = "field_type" then
         return Kit_Type_Reference'Image (Item.Field_Type);
      elsif Field = "field_offset" then
         return Natural'Image (Item.Field_Offset);
      elsif Field = "field_length" then
         return Natural'Image (Item.Field_Length);
      elsif Field = "created" then
         return Boolean'Image (Item.Created);
      elsif Field = "readable" then
         return Boolean'Image (Item.Readable);
      elsif Field = "writeable" then
         return Boolean'Image (Item.Writeable);
      elsif Field = "display" then
         return Boolean'Image (Item.Display);
      elsif Field = "base_ref" then
         return Boolean'Image (Item.Base_Ref);
      else
         raise Constraint_Error with
           "no such field";
      end if;
   end Get;

   -------------------------
   -- Get_By_Record_Field --
   -------------------------

   function Get_By_Record_Field
     (Kit_Record : Kit_Record_Reference;
      Name       : String)
   return Kit_Field_Reference
   is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (15);
      Kit_Field_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Record_Field_Ref,
            Kit_Field_Impl.Record_Field_To_Storage (Kit_Record, Name)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First),
            Kit_Field_Impl.Record_Field_To_Storage (Kit_Record, Name)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last), Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         if M.Valid then
            Db_Index := Marlowe.Key_Storage.To_Database_Index (M.Get_Key);
         end if;
      end;
      Kit_Field_Impl.File_Mutex.Shared_Unlock;
      return Kit_Field_Reference (Db_Index);
   end Get_By_Record_Field;

   ------------------------------
   -- Get_From_Kit_Root_Record --
   ------------------------------

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit_Root_Record_Reference) return Kit_Field_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (15);
      Kit_Field_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_T1_Idx_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Root_Record)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First),
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Root_Record)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last), Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         if M.Valid then
            Db_Index := Marlowe.Key_Storage.To_Database_Index (M.Get_Key);
         end if;
      end;
      Kit_Field_Impl.File_Mutex.Shared_Unlock;
      return Kit_Field_Reference (Db_Index);
   end Get_From_Kit_Root_Record;

   -----------------------------
   -- Get_Kit_Field_Reference --
   -----------------------------

   overriding function Get_Kit_Field_Reference (Item :
      Kit_Field_Implementation) return Kit_Field_Reference
   is (Item.M_Index);

   -----------------------------------
   -- Get_Kit_Root_Record_Reference --
   -----------------------------------

   overriding function Get_Kit_Root_Record_Reference (Item :
      Kit_Field_Implementation) return Kit_Root_Record_Reference
   is (Item.T15_Data.Kit_Root_Record);

   ----------------
   -- Get_Update --
   ----------------

   function Get_Update (Ref : Kit_Field_Reference) return Kit_Field_Update is
   begin
      return Result : Kit_Field_Update_Implementation do
         Kit_Field_Impl.File_Mutex.Shared_Lock;
         Result.M_Index := Ref;
         if Result.M_Index /= Null_Kit_Field_Reference then
            Kit_Field_Cache.U_Lock (Marlowe.Database_Index (Result.M_Index));
            Result.T15_Data := Kit_Field_Cache.Get (Marlowe.Database_Index
               (Result.M_Index), False).Db;
            Kit_Root_Record_Cache.U_Lock (Marlowe.Database_Index
               (Result.T15_Data.Kit_Root_Record));
            Result.T1_Data := Kit_Root_Record_Cache.Get
               (Marlowe.Database_Index (Result.T15_Data.Kit_Root_Record),
               False).Db;
         end if;
         Result.Finished := False;
         Result.Using_Key_Value := False;
         Result.Scanning := False;
         Result.Read_Only := False;
         Kit_Field_Impl.File_Mutex.Shared_Unlock;
      end return;
   end Get_Update;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element (Item : Kit_Field_Implementation)
      return Boolean is
   begin
      return Item.M_Index /= 0;
   end Has_Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Item : Cursor) return Boolean is
   begin
      return List_Of_References.Has_Element (Item.Current);
   end Has_Element;

   --------------
   -- Identity --
   --------------

   overriding function Identity (Item : Kit_Field_Implementation)
      return String is
   begin
      return "kit_field" & Kit_Field_Reference'Image
         (Item.Get_Kit_Field_Reference);
   end Identity;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Item :
      in out Kit_Field_Update_Implementation) is
   begin
      Item.Finished := True;
      Item.Forward := True;
      Item.Read_Only := True;
      Item.Created := False;
      Item.Deleted := False;
      Item.Scanning := False;
      Item.Has_Finish := False;
      Item.Using_Key := False;
      Item.M_Index := 0;
      Item.Read_Only := True;
   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Selection) return Boolean is
   begin
      return Has_Element (Container.First);
   end Is_Empty;

   ---------------------
   -- Is_Record_Field --
   ---------------------

   function Is_Record_Field
     (Kit_Record : Kit_Record_Reference;
      Name       : String)
   return Boolean
   is
   begin
      return Get_By_Record_Field (Kit_Record, Name)
         /= Null_Kit_Field_Reference;
   end Is_Record_Field;

   -------------
   -- Iterate --
   -------------

   function Iterate (Container : Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Result : Iterator do
         Result.Container := Container'Unrestricted_Access;
      end return;
   end Iterate;

   ----------------
   -- Kit_Record --
   ----------------

   overriding function Kit_Record (Item : Kit_Field_Implementation)
      return Kit_Record_Reference is
      Result : Kit_Record_Reference
        renames Item.T15_Data.Kit_Record;
   begin
      return Result;
   end Kit_Record;

   ----------
   -- Last --
   ----------

   overriding function Last (Object : Iterator) return Cursor is
   begin
      return (Current => Object.Container.Elements.Last);
   end Last;

   ----------
   -- Last --
   ----------

   function Last (Container : Selection) return Cursor is
   begin
      return (Current => Container.Elements.Last);
   end Last;

   ---------------------------
   -- Last_By_Display_Field --
   ---------------------------

   function Last_By_Display_Field
     (Kit_Record : Kit_Record_Reference;
      Display    : Boolean)
   return Kit_Field_Reference
   is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (15);
      Kit_Field_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Display_Field_Ref,
            Kit_Field_Impl.Display_Field_To_Storage (Kit_Record, Display)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First),
            Kit_Field_Impl.Display_Field_To_Storage (Kit_Record, Display)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last), Marlowe.Closed, Marlowe.Closed,
            Marlowe.Backward);
      begin
         if M.Valid then
            Db_Index := Marlowe.Key_Storage.To_Database_Index (M.Get_Key);
         end if;
      end;
      Kit_Field_Impl.File_Mutex.Shared_Unlock;
      return Kit_Field_Reference (Db_Index);
   end Last_By_Display_Field;

   ------------------------
   -- Last_By_Kit_Record --
   ------------------------

   function Last_By_Kit_Record (Kit_Record : Kit_Record_Reference)
      return Kit_Field_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (15);
      Kit_Field_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Kit_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Record)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First),
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Record)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last), Marlowe.Closed, Marlowe.Closed,
            Marlowe.Backward);
      begin
         if M.Valid then
            Db_Index := Marlowe.Key_Storage.To_Database_Index (M.Get_Key);
         end if;
      end;
      Kit_Field_Impl.File_Mutex.Shared_Unlock;
      return Kit_Field_Reference (Db_Index);
   end Last_By_Kit_Record;

   ------------------------
   -- Last_By_Top_Record --
   ------------------------

   function Last_By_Top_Record (Top_Record : Record_Type)
      return Kit_Field_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Field_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Top_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Top_Record), 4) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First),
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Top_Record), 4) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last), Marlowe.Closed, Marlowe.Closed,
            Marlowe.Backward);
      begin
         if M.Valid then
            Db_Index := Marlowe.Key_Storage.To_Database_Index (M.Get_Key);
         end if;
      end;
      Kit_Field_Impl.File_Mutex.Shared_Unlock;
      return Kit_Field_Reference (Db_Index);
   end Last_By_Top_Record;

   ------------
   -- Length --
   ------------

   function Length (Container : Selection) return Natural is
      Result : Natural := 0;
      It : Cursor := Container.First;
   begin
      while Has_Element (It) loop
         Result := Result + 1;
         Next (It);
      end loop;
      return Result;
   end Length;

   ----------
   -- Name --
   ----------

   overriding function Name (Item : Kit_Field_Implementation)
      return String is
      Result : Kit.Strings.String_Type
        renames Item.T15_Data.Name;
   begin
      return Result.Text (1 .. Result.Length);
   end Name;

   ----------
   -- Next --
   ----------

   procedure Next (Position : in out Cursor) is
   begin
      List_Of_References.Next (Position.Current);
   end Next;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Object   : Iterator;
      Position : Cursor)
   return Cursor
   is
   begin
      return (Current => List_Of_References.Next (Position.Current));
   end Next;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
   begin
      return (Current => List_Of_References.Next (Position.Current));
   end Next;

   --------------------------
   -- Notify_Record_Change --
   --------------------------

   overriding procedure Notify_Record_Change
     (Handle         : Kit_Field_Notify_Record;
      Changed_Record : Marlowe.Database_Index)
   is
   begin
      Handle.Handler (Kit_Field_Reference (Changed_Record));
   end Notify_Record_Change;

   -------------------------
   -- Notify_Table_Change --
   -------------------------

   overriding procedure Notify_Table_Change (Handle :
      Kit_Field_Table_Notify_Record) is
   begin
      Handle.Handler.all;
   end Notify_Table_Change;

   -------------------------
   -- On_Kit_Field_Change --
   -------------------------

   procedure On_Kit_Field_Change
     (Reference : Kit_Field_Reference;
      Handler   : Kit_Field_Notify_Handler)
   is
      Rec : Kit_Field_Notify_Record;
   begin
      Rec.Handler := Handler;
      Kit.Notifier.Add_Record_Change_Handler
        (15,
         Marlowe.Database_Index (Reference),
         Rec);
   end On_Kit_Field_Change;

   --------------------------
   -- On_Kit_Field_Created --
   --------------------------

   procedure On_Kit_Field_Created (Handler : Kit_Field_Notify_Handler) is
      Rec : Kit_Field_Notify_Record;
   begin
      Rec.Handler := Handler;
      Kit.Notifier.Add_Record_Create_Handler (15, Rec);
   end On_Kit_Field_Created;

   --------------------------
   -- On_Kit_Field_Deleted --
   --------------------------

   procedure On_Kit_Field_Deleted (Handler : Kit_Field_Notify_Handler) is
      Rec : Kit_Field_Notify_Record;
   begin
      Rec.Handler := Handler;
      Kit.Notifier.Add_Record_Delete_Handler (15, Rec);
   end On_Kit_Field_Deleted;

   -------------------------------
   -- On_Kit_Field_Table_Change --
   -------------------------------

   procedure On_Kit_Field_Table_Change (Handler :
      Kit_Field_Table_Notify_Handler) is
      Rec : Kit_Field_Table_Notify_Record;
   begin
      Rec.Handler := Handler;
      Kit.Notifier.Add_Table_Change_Handler (15, Rec);
   end On_Kit_Field_Table_Change;

   --------------
   -- Previous --
   --------------

   procedure Previous (Position : in out Cursor) is
   begin
      List_Of_References.Previous (Position.Current);
   end Previous;

   --------------
   -- Previous --
   --------------

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor)
   return Cursor
   is
   begin
      return (Current => List_Of_References.Previous (Position.Current));
   end Previous;

   --------------
   -- Previous --
   --------------

   function Previous (Position : Cursor) return Cursor is
   begin
      return (Current => List_Of_References.Previous (Position.Current));
   end Previous;

   --------------
   -- Readable --
   --------------

   overriding function Readable (Item : Kit_Field_Implementation)
      return Boolean is
      Result : Boolean
        renames Item.T15_Data.Readable;
   begin
      return Result;
   end Readable;

   ---------------------------
   -- Scan_By_Display_Field --
   ---------------------------

   function Scan_By_Display_Field return Selection is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (15);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array (1 .. 17) := (others => 0);
         Last_Key : constant Storage_Array (1 .. 17) :=
            (others => System.Storage_Elements.Storage_Element'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Display_Field_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Scan_By_Display_Field;

   ------------------------
   -- Scan_By_Kit_Record --
   ------------------------

   function Scan_By_Kit_Record return Selection is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (15);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array (1 .. 16) := (others => 0);
         Last_Key : constant Storage_Array (1 .. 16) :=
            (others => System.Storage_Elements.Storage_Element'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Kit_Record_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Scan_By_Kit_Record;

   --------------------------
   -- Scan_By_Record_Field --
   --------------------------

   function Scan_By_Record_Field return Selection is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (15);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array (1 .. 80) := (others => 0);
         Last_Key : constant Storage_Array (1 .. 80) :=
            (others => System.Storage_Elements.Storage_Element'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Record_Field_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Scan_By_Record_Field;

   ------------------------
   -- Scan_By_Top_Record --
   ------------------------

   function Scan_By_Top_Record return Selection is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array (1 .. 12) := (others => 0);
         Last_Key : constant Storage_Array (1 .. 12) :=
            (others => System.Storage_Elements.Storage_Element'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Top_Record_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Scan_By_Top_Record;

   ----------------------------------
   -- Select_Bounded_By_Top_Record --
   ----------------------------------

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Record_Type;
      Finish_Top_Record : Record_Type)
   return Selection
   is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array :=
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Start_Top_Record), 4) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First);
         Last_Key : constant Storage_Array :=
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Finish_Top_Record), 4) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Top_Record_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_Bounded_By_Top_Record;

   -----------------------------
   -- Select_By_Display_Field --
   -----------------------------

   function Select_By_Display_Field
     (Kit_Record : Kit_Record_Reference;
      Display    : Boolean)
   return Selection
   is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (15);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array :=
            Kit_Field_Impl.Display_Field_To_Storage (Kit_Record, Display)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First);
         Last_Key : constant Storage_Array :=
            Kit_Field_Impl.Display_Field_To_Storage (Kit_Record, Display)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Display_Field_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_By_Display_Field;

   --------------------------
   -- Select_By_Kit_Record --
   --------------------------

   function Select_By_Kit_Record (Kit_Record : Kit_Record_Reference)
      return Selection is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (15);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array :=
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Record)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First);
         Last_Key : constant Storage_Array :=
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Record)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Kit_Record_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_By_Kit_Record;

   ----------------------------
   -- Select_By_Record_Field --
   ----------------------------

   function Select_By_Record_Field
     (Kit_Record : Kit_Record_Reference;
      Name       : String)
   return Selection
   is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (15);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array :=
            Kit_Field_Impl.Record_Field_To_Storage (Kit_Record, Name)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First);
         Last_Key : constant Storage_Array :=
            Kit_Field_Impl.Record_Field_To_Storage (Kit_Record, Name)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Record_Field_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_By_Record_Field;

   --------------------------
   -- Select_By_Top_Record --
   --------------------------

   function Select_By_Top_Record (Top_Record : Record_Type)
      return Selection is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array :=
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Top_Record), 4) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First);
         Last_Key : constant Storage_Array :=
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Top_Record), 4) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Top_Record_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_By_Top_Record;

   ---------------------------------------------
   -- Select_Display_Field_Bounded_By_Display --
   ---------------------------------------------

   function Select_Display_Field_Bounded_By_Display
     (Kit_Record     : Kit_Record_Reference;
      Start_Display  : Boolean;
      Finish_Display : Boolean)
   return Selection
   is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (15);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array :=
            Kit_Field_Impl.Display_Field_To_Storage (Kit_Record,
            Start_Display) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First);
         Last_Key : constant Storage_Array :=
            Kit_Field_Impl.Display_Field_To_Storage (Kit_Record,
            Finish_Display) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Display_Field_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_Display_Field_Bounded_By_Display;

   -----------------------------------------
   -- Select_Record_Field_Bounded_By_Name --
   -----------------------------------------

   function Select_Record_Field_Bounded_By_Name
     (Kit_Record  : Kit_Record_Reference;
      Start_Name  : String;
      Finish_Name : String)
   return Selection
   is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (15);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array :=
            Kit_Field_Impl.Record_Field_To_Storage (Kit_Record, Start_Name)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First);
         Last_Key : constant Storage_Array :=
            Kit_Field_Impl.Record_Field_To_Storage (Kit_Record, Finish_Name)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T15_Record_Field_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_Record_Field_Bounded_By_Name;

   ---------
   -- Set --
   ---------

   overriding procedure Set
     (Item  : in out Kit_Field_Update_Implementation;
      Field : String;
      Value : String)
   is
   begin
      if Field = "" then
         raise Constraint_Error with
           "missing field name";
      elsif Field = "top_record" then
         raise Constraint_Error with
           "field Top_Record is not writable";
      elsif Field = "kit_root_record" then
         raise Constraint_Error with
           "field Kit_Root_Record is not writable";
      elsif Field = "name" then
         Item.Set_Name (Value);
      elsif Field = "kit_record" then
         Item.Set_Kit_Record (Kit_Record_Reference'Value (Value));
      elsif Field = "field_type" then
         Item.Set_Field_Type (Kit_Type_Reference'Value (Value));
      elsif Field = "field_offset" then
         Item.Set_Field_Offset (Natural'Value (Value));
      elsif Field = "field_length" then
         Item.Set_Field_Length (Natural'Value (Value));
      elsif Field = "created" then
         Item.Set_Created (Boolean'Value (Value));
      elsif Field = "readable" then
         Item.Set_Readable (Boolean'Value (Value));
      elsif Field = "writeable" then
         Item.Set_Writeable (Boolean'Value (Value));
      elsif Field = "display" then
         Item.Set_Display (Boolean'Value (Value));
      elsif Field = "base_ref" then
         Item.Set_Base_Ref (Boolean'Value (Value));
      else
         raise Constraint_Error with
           "no such field";
      end if;
   end Set;

   ------------------
   -- Set_Base_Ref --
   ------------------

   overriding procedure Set_Base_Ref
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Boolean)
   is
      Target : Boolean
        renames Item.T15_Data.Base_Ref;
   begin
      Database_Mutex.Shared_Lock;
      Item.X_Lock;
      Target := Value;
      Database_Mutex.Shared_Unlock;
   end Set_Base_Ref;

   -----------------
   -- Set_Created --
   -----------------

   overriding procedure Set_Created
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Boolean)
   is
      Target : Boolean
        renames Item.T15_Data.Created;
   begin
      Database_Mutex.Shared_Lock;
      Item.X_Lock;
      Target := Value;
      Database_Mutex.Shared_Unlock;
   end Set_Created;

   -----------------
   -- Set_Display --
   -----------------

   overriding procedure Set_Display
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Boolean)
   is
      Target : Boolean
        renames Item.T15_Data.Display;
   begin
      Database_Mutex.Shared_Lock;
      Item.X_Lock;
      if not Item.Created then
         Kit_Deferred_Keys.Key_Changed
           (15,
            Marlowe_Keys.T15_Display_Field_Ref,
            Marlowe.Database_Index (Item.M_Index),
            Kit_Field_Impl.Display_Field_To_Storage (Item.Kit_Record,
               Item.Display),
            Kit_Field_Impl.Display_Field_To_Storage (Item.Kit_Record,
            Value));
      end if;
      Target := Value;
      Database_Mutex.Shared_Unlock;
   end Set_Display;

   ----------------------
   -- Set_Field_Length --
   ----------------------

   overriding procedure Set_Field_Length
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Integer)
   is
      Target : Integer
        renames Item.T15_Data.Field_Length;
   begin
      Database_Mutex.Shared_Lock;
      Item.X_Lock;
      Target := Value;
      Database_Mutex.Shared_Unlock;
   end Set_Field_Length;

   ----------------------
   -- Set_Field_Offset --
   ----------------------

   overriding procedure Set_Field_Offset
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Integer)
   is
      Target : Integer
        renames Item.T15_Data.Field_Offset;
   begin
      Database_Mutex.Shared_Lock;
      Item.X_Lock;
      Target := Value;
      Database_Mutex.Shared_Unlock;
   end Set_Field_Offset;

   --------------------
   -- Set_Field_Type --
   --------------------

   overriding procedure Set_Field_Type
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Kit.Db.Kit_Type.Kit_Type_Type)
   is
   begin
      Item.Set_Field_Type (Value.Get_Kit_Type_Reference);
   end Set_Field_Type;

   --------------------
   -- Set_Field_Type --
   --------------------

   overriding procedure Set_Field_Type
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Kit_Type_Reference)
   is
      Target : Kit_Type_Reference
        renames Item.T15_Data.Field_Type;
   begin
      Database_Mutex.Shared_Lock;
      Item.X_Lock;
      Target := Value;
      Database_Mutex.Shared_Unlock;
   end Set_Field_Type;

   --------------------
   -- Set_Kit_Record --
   --------------------

   overriding procedure Set_Kit_Record
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Kit.Db.Kit_Record.Kit_Record_Type)
   is
   begin
      Item.Set_Kit_Record (Value.Get_Kit_Record_Reference);
   end Set_Kit_Record;

   --------------------
   -- Set_Kit_Record --
   --------------------

   overriding procedure Set_Kit_Record
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Kit_Record_Reference)
   is
      Target : Kit_Record_Reference
        renames Item.T15_Data.Kit_Record;
   begin
      Database_Mutex.Shared_Lock;
      Item.X_Lock;
      if not Item.Created then
         Kit_Deferred_Keys.Key_Changed
           (15,
            Marlowe_Keys.T15_Kit_Record_Ref,
            Marlowe.Database_Index (Item.M_Index),
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
               (Item.Kit_Record)),
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
               (Value)));
      end if;
      if not Item.Created then
         Kit_Deferred_Keys.Key_Changed
           (15,
            Marlowe_Keys.T15_Record_Field_Ref,
            Marlowe.Database_Index (Item.M_Index),
            Kit_Field_Impl.Record_Field_To_Storage (Item.Kit_Record,
               Item.Name),
            Kit_Field_Impl.Record_Field_To_Storage (Value, Item.Name));
      end if;
      if not Item.Created then
         Kit_Deferred_Keys.Key_Changed
           (15,
            Marlowe_Keys.T15_Display_Field_Ref,
            Marlowe.Database_Index (Item.M_Index),
            Kit_Field_Impl.Display_Field_To_Storage (Item.Kit_Record,
               Item.Display),
            Kit_Field_Impl.Display_Field_To_Storage (Value, Item.Display));
      end if;
      Target := Value;
      Database_Mutex.Shared_Unlock;
   end Set_Kit_Record;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Item  : in out Kit_Field_Update_Implementation;
      Value : String)
   is
      Target : Kit.Strings.String_Type
        renames Item.T15_Data.Name;
   begin
      Database_Mutex.Shared_Lock;
      Item.X_Lock;
      if not Item.Created then
         Kit_Deferred_Keys.Key_Changed
           (15,
            Marlowe_Keys.T15_Record_Field_Ref,
            Marlowe.Database_Index (Item.M_Index),
            Kit_Field_Impl.Record_Field_To_Storage (Item.Kit_Record,
               Item.Name),
            Kit_Field_Impl.Record_Field_To_Storage (Item.Kit_Record, Value));
      end if;
      Target.Length := Value'Length;
      Target.Text (1 .. Value'Length) := Value;
      Database_Mutex.Shared_Unlock;
   end Set_Name;

   ------------------
   -- Set_Readable --
   ------------------

   overriding procedure Set_Readable
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Boolean)
   is
      Target : Boolean
        renames Item.T15_Data.Readable;
   begin
      Database_Mutex.Shared_Lock;
      Item.X_Lock;
      Target := Value;
      Database_Mutex.Shared_Unlock;
   end Set_Readable;

   -------------------
   -- Set_Writeable --
   -------------------

   overriding procedure Set_Writeable
     (Item  : in out Kit_Field_Update_Implementation;
      Value : Boolean)
   is
      Target : Boolean
        renames Item.T15_Data.Writeable;
   begin
      Database_Mutex.Shared_Lock;
      Item.X_Lock;
      Target := Value;
      Database_Mutex.Shared_Unlock;
   end Set_Writeable;

   ----------------
   -- Top_Record --
   ----------------

   overriding function Top_Record (Item : Kit_Field_Implementation)
      return Record_Type is
      Result : Record_Type
        renames Item.T1_Data.Top_Record;
   begin
      return Result;
   end Top_Record;

   ---------------
   -- Writeable --
   ---------------

   overriding function Writeable (Item : Kit_Field_Implementation)
      return Boolean is
      Result : Boolean
        renames Item.T15_Data.Writeable;
   begin
      return Result;
   end Writeable;

   ------------
   -- X_Lock --
   ------------

   overriding procedure X_Lock (Item : Kit_Field_Implementation) is
   begin
      Kit_Root_Record_Cache.X_Lock (Marlowe.Database_Index
         (Item.T15_Data.Kit_Root_Record));
      Kit_Field_Cache.X_Lock (Marlowe.Database_Index (Item.M_Index));
   end X_Lock;

end Kit.Db.Kit_Field;
