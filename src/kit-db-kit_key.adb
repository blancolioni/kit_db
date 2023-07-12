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
with Kit.Db.Kit_Key_Impl;
with Kit.Db.Kit_Key_Cache;

package body Kit.Db.Kit_Key is

   type Kit_Key_Implementation is
        limited new Ada.Finalization.Limited_Controlled
     and Kit_Key_Interface with
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
         M_Index         : Kit_Key_Reference;
         T1_Data         :
            Kit_Root_Record_Impl.Kit_Root_Record_Database_Record;
         T17_Data        : Kit_Key_Impl.Kit_Key_Database_Record;
      end record;
   overriding function Has_Element (Item : Kit_Key_Implementation)
      return Boolean;
   overriding procedure X_Lock (Item : Kit_Key_Implementation);
   overriding function Get_Kit_Root_Record_Reference (Item :
      Kit_Key_Implementation) return Kit_Root_Record_Reference;
   overriding function Get_Kit_Key_Reference (Item : Kit_Key_Implementation)
      return Kit_Key_Reference;
   overriding function Top_Record (Item : Kit_Key_Implementation)
      return Record_Type;
   overriding function Name (Item : Kit_Key_Implementation) return String;
   overriding function Kit_Record (Item : Kit_Key_Implementation)
      return Kit_Record_Reference;
   overriding function Is_Unique (Item : Kit_Key_Implementation)
      return Boolean;
   overriding function Length (Item : Kit_Key_Implementation) return Integer;
   overriding function Identity (Item : Kit_Key_Implementation)
      return String;
   overriding function Get
     (Item  : Kit_Key_Implementation;
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
   type Kit_Key_Table_Notify_Record is
        new Kit.Notifier.Table_Notify_Interface with
      record
         Handler : Kit_Key_Table_Notify_Handler;
      end record;
   overriding procedure Notify_Table_Change (Handle :
      Kit_Key_Table_Notify_Record);
   type Kit_Key_Notify_Record is
        new Kit.Notifier.Record_Notify_Interface with
      record
         Handler : Kit_Key_Notify_Handler;
      end record;
   overriding procedure Notify_Record_Change
     (Handle         : Kit_Key_Notify_Record;
      Changed_Record : Marlowe.Database_Index);
   type Kit_Key_Update_Implementation is limited new Kit_Key_Implementation
     and Kit_Key_Update_Interface
     and Record_Update_Interface with null record;
   overriding procedure Initialize (Item :
      in out Kit_Key_Update_Implementation);
   overriding procedure Finalize (Item :
      in out Kit_Key_Update_Implementation);
   overriding procedure Delete (Item : in out Kit_Key_Update_Implementation);
   overriding procedure Set
     (Item  : in out Kit_Key_Update_Implementation;
      Field : String;
      Value : String);
   overriding procedure Set_Name
     (Item  : in out Kit_Key_Update_Implementation;
      Value : String);
   overriding procedure Set_Kit_Record
     (Item  : in out Kit_Key_Update_Implementation;
      Value : Kit_Record_Reference);
   overriding procedure Set_Kit_Record
     (Item  : in out Kit_Key_Update_Implementation;
      Value : Kit.Db.Kit_Record.Kit_Record_Type);
   overriding procedure Set_Is_Unique
     (Item  : in out Kit_Key_Update_Implementation;
      Value : Boolean);
   overriding procedure Set_Length
     (Item  : in out Kit_Key_Update_Implementation;
      Value : Integer);

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
     (Name       : String;
      Kit_Record : Kit_Record_Reference;
      Is_Unique  : Boolean;
      Length     : Integer)
   is
      Result : Kit_Key_Update := Create;
   begin
      Result.Set_Name (Name);
      Result.Set_Kit_Record (Kit_Record);
      Result.Set_Is_Unique (Is_Unique);
      Result.Set_Length (Length);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Name       : String;
      Kit_Record : Kit_Record_Reference;
      Is_Unique  : Boolean;
      Length     : Integer)
   return Kit_Key_Reference
   is
      Result : Kit_Key_Update := Create;
   begin
      Result.Set_Name (Name);
      Result.Set_Kit_Record (Kit_Record);
      Result.Set_Is_Unique (Is_Unique);
      Result.Set_Length (Length);
      return Result.Get_Kit_Key_Reference;
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Kit_Key_Update is
      T1_Access : Kit_Root_Record_Cache.Cache_Access;
      T17_Access : Kit_Key_Cache.Cache_Access;
   begin
      return Result : Kit_Key_Update_Implementation do
         Result.Finished := False;
         Result.Created := True;
         Result.Deleted := False;
         Result.Scanning := False;
         Result.Read_Only := False;
         Result.Using_Key := False;
         Result.M_Index := 0;
         Memory_Mutex.Lock;
         T1_Access := new Kit_Root_Record_Cache.Cache_Record;
         T1_Access.Db.Top_Record := R_Kit_Key;
         T17_Access := new Kit_Key_Cache.Cache_Record;
         Memory_Mutex.Unlock;
         Kit_Root_Record_Impl.File_Mutex.Lock;
         T17_Access.Db.Kit_Root_Record := Kit_Root_Record_Reference
            (Marlowe_Keys.Handle.Insert_Record (1));
         T1_Access.Initialise (1, Marlowe.Database_Index
            (T17_Access.Db.Kit_Root_Record));
         Kit.Cache.Insert (Kit.Cache.Cache_Entry (T1_Access));
         Kit_Root_Record_Impl.Write (Marlowe.Database_Index
            (T17_Access.Db.Kit_Root_Record), T1_Access.Db);
         T1_Access.X_Lock;
         Kit_Root_Record_Impl.File_Mutex.Unlock;
         Kit_Key_Impl.File_Mutex.Lock;
         Result.M_Index := Kit_Key_Reference
            (Marlowe_Keys.Handle.Insert_Record (17));
         T17_Access.Initialise (17, Marlowe.Database_Index (Result.M_Index));
         Kit.Cache.Insert (Kit.Cache.Cache_Entry (T17_Access));
         Kit_Key_Impl.Write (Marlowe.Database_Index (Result.M_Index),
            T17_Access.Db);
         T17_Access.X_Lock;
         Kit_Key_Impl.File_Mutex.Unlock;
         Result.T1_Data := T1_Access.Db;
         Result.T17_Data := T17_Access.Db;
         Result.Read_Only := False;
      end return;
   end Create;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete (Item : in out Kit_Key_Update_Implementation)
      is
   begin
      Database_Mutex.Shared_Lock;
      Item.Deleted := True;
      Database_Mutex.Shared_Unlock;
   end Delete;

   -------------
   -- Element --
   -------------

   function Element (Item : Cursor) return Kit_Key_Reference is
   begin
      return List_Of_References.Element (Item.Current);
   end Element;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Item :
      in out Kit_Key_Update_Implementation) is
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
            (Item.T17_Data.Kit_Root_Record)));
         Marlowe_Keys.Handle.Delete (Marlowe_Keys.T17_Top_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Item.Top_Record), 4)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Delete (Marlowe_Keys.T17_T1_Idx_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.T17_Data.Kit_Root_Record))
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Delete (Marlowe_Keys.T17_Kit_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.Kit_Record))
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Delete (Marlowe_Keys.T17_Record_Key_Ref,
            Kit_Key_Impl.Record_Key_To_Storage (Item.Kit_Record, Item.Name)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Item.T1_Data.Deleted := True;
         Item.T17_Data.Deleted := True;
         Kit.Notifier.Record_Deleted (1, Marlowe.Database_Index
            (Item.T17_Data.Kit_Root_Record));
         Kit.Notifier.Record_Deleted (17, Marlowe.Database_Index
            (Item.M_Index));
      end if;
      if not (Item.Read_Only or else Item.Created) or else Item.Deleted then
         Kit.Notifier.Record_Changed (1, Marlowe.Database_Index
            (Item.T17_Data.Kit_Root_Record));
         Kit.Notifier.Record_Changed (17, Marlowe.Database_Index
            (Item.M_Index));
      end if;
      if (not Item.Read_Only) and then Item.Created then
         Database_Mutex.Shared_Lock;
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T1_Top_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Item.Top_Record), 4)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.T17_Data.Kit_Root_Record)));
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T17_Top_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Item.Top_Record), 4)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T17_T1_Idx_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.T17_Data.Kit_Root_Record))
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T17_Kit_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.Kit_Record))
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T17_Record_Key_Ref,
            Kit_Key_Impl.Record_Key_To_Storage (Item.Kit_Record, Item.Name)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Database_Mutex.Shared_Unlock;
         Kit.Notifier.Record_Created (1, Marlowe.Database_Index
            (Item.T17_Data.Kit_Root_Record));
         Kit.Notifier.Record_Created (17, Marlowe.Database_Index
            (Item.M_Index));
      end if;
      if not Item.Read_Only then
         declare
            T1_Access : constant Kit_Root_Record_Cache.Cache_Access :=
               Kit_Root_Record_Cache.Get (Marlowe.Database_Index
               (Item.T17_Data.Kit_Root_Record));
         begin
            T1_Access.Db := Item.T1_Data;
         end;
         declare
            T17_Access : constant Kit_Key_Cache.Cache_Access :=
               Kit_Key_Cache.Get (Marlowe.Database_Index (Item.M_Index));
         begin
            T17_Access.Db := Item.T17_Data;
         end;
      end if;
      if not Item.Read_Only then
         Kit_Root_Record_Cache.Unlock (Marlowe.Database_Index
            (Item.T17_Data.Kit_Root_Record));
         Kit_Key_Cache.Unlock (Marlowe.Database_Index (Item.M_Index));
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

   -------------------------
   -- First_By_Kit_Record --
   -------------------------

   function First_By_Kit_Record (Kit_Record : Kit_Record_Reference)
      return Kit_Key_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (17);
      Kit_Key_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T17_Kit_Record_Ref,
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
      Kit_Key_Impl.File_Mutex.Shared_Unlock;
      return Kit_Key_Reference (Db_Index);
   end First_By_Kit_Record;

   -------------------------
   -- First_By_Top_Record --
   -------------------------

   function First_By_Top_Record (Top_Record : Record_Type)
      return Kit_Key_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Key_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T17_Top_Record_Ref,
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
      Kit_Key_Impl.File_Mutex.Shared_Unlock;
      return Kit_Key_Reference (Db_Index);
   end First_By_Top_Record;

   ---------
   -- Get --
   ---------

   function Get (Ref : Kit_Key_Reference) return Kit_Key_Type is
   begin
      return Result : Kit_Key_Implementation do
         Kit_Key_Impl.File_Mutex.Shared_Lock;
         Result.M_Index := Ref;
         if Result.M_Index /= Null_Kit_Key_Reference then
            Kit_Key_Cache.S_Lock (Marlowe.Database_Index (Result.M_Index));
            Result.T17_Data := Kit_Key_Cache.Get (Marlowe.Database_Index
               (Result.M_Index), False).Db;
            Kit_Root_Record_Cache.S_Lock (Marlowe.Database_Index
               (Result.T17_Data.Kit_Root_Record));
            Result.T1_Data := Kit_Root_Record_Cache.Get
               (Marlowe.Database_Index (Result.T17_Data.Kit_Root_Record),
               False).Db;
            Kit_Key_Cache.Unlock (Marlowe.Database_Index (Result.M_Index));
            Kit_Root_Record_Cache.Unlock (Marlowe.Database_Index
               (Result.T17_Data.Kit_Root_Record));
         end if;
         Result.Finished := False;
         Result.Using_Key_Value := False;
         Result.Scanning := False;
         Kit_Key_Impl.File_Mutex.Shared_Unlock;
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Item  : Kit_Key_Implementation;
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
      elsif Field = "is_unique" then
         return Boolean'Image (Item.Is_Unique);
      elsif Field = "length" then
         return Natural'Image (Item.Length);
      else
         raise Constraint_Error with
           "no such field";
      end if;
   end Get;

   -----------------------
   -- Get_By_Record_Key --
   -----------------------

   function Get_By_Record_Key
     (Kit_Record : Kit_Record_Reference;
      Name       : String)
   return Kit_Key_Reference
   is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (17);
      Kit_Key_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T17_Record_Key_Ref,
            Kit_Key_Impl.Record_Key_To_Storage (Kit_Record, Name)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First),
            Kit_Key_Impl.Record_Key_To_Storage (Kit_Record, Name)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last), Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         if M.Valid then
            Db_Index := Marlowe.Key_Storage.To_Database_Index (M.Get_Key);
         end if;
      end;
      Kit_Key_Impl.File_Mutex.Shared_Unlock;
      return Kit_Key_Reference (Db_Index);
   end Get_By_Record_Key;

   ------------------------------
   -- Get_From_Kit_Root_Record --
   ------------------------------

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit_Root_Record_Reference) return Kit_Key_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (17);
      Kit_Key_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T17_T1_Idx_Ref,
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
      Kit_Key_Impl.File_Mutex.Shared_Unlock;
      return Kit_Key_Reference (Db_Index);
   end Get_From_Kit_Root_Record;

   ---------------------------
   -- Get_Kit_Key_Reference --
   ---------------------------

   overriding function Get_Kit_Key_Reference (Item : Kit_Key_Implementation)
      return Kit_Key_Reference
   is (Item.M_Index);

   -----------------------------------
   -- Get_Kit_Root_Record_Reference --
   -----------------------------------

   overriding function Get_Kit_Root_Record_Reference (Item :
      Kit_Key_Implementation) return Kit_Root_Record_Reference
   is (Item.T17_Data.Kit_Root_Record);

   ----------------
   -- Get_Update --
   ----------------

   function Get_Update (Ref : Kit_Key_Reference) return Kit_Key_Update is
   begin
      return Result : Kit_Key_Update_Implementation do
         Kit_Key_Impl.File_Mutex.Shared_Lock;
         Result.M_Index := Ref;
         if Result.M_Index /= Null_Kit_Key_Reference then
            Kit_Key_Cache.U_Lock (Marlowe.Database_Index (Result.M_Index));
            Result.T17_Data := Kit_Key_Cache.Get (Marlowe.Database_Index
               (Result.M_Index), False).Db;
            Kit_Root_Record_Cache.U_Lock (Marlowe.Database_Index
               (Result.T17_Data.Kit_Root_Record));
            Result.T1_Data := Kit_Root_Record_Cache.Get
               (Marlowe.Database_Index (Result.T17_Data.Kit_Root_Record),
               False).Db;
         end if;
         Result.Finished := False;
         Result.Using_Key_Value := False;
         Result.Scanning := False;
         Result.Read_Only := False;
         Kit_Key_Impl.File_Mutex.Shared_Unlock;
      end return;
   end Get_Update;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Item : Cursor) return Boolean is
   begin
      return List_Of_References.Has_Element (Item.Current);
   end Has_Element;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element (Item : Kit_Key_Implementation)
      return Boolean is
   begin
      return Item.M_Index /= 0;
   end Has_Element;

   --------------
   -- Identity --
   --------------

   overriding function Identity (Item : Kit_Key_Implementation)
      return String is
   begin
      return "kit_key" & Kit_Key_Reference'Image
         (Item.Get_Kit_Key_Reference);
   end Identity;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Item :
      in out Kit_Key_Update_Implementation) is
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

   -------------------
   -- Is_Record_Key --
   -------------------

   function Is_Record_Key
     (Kit_Record : Kit_Record_Reference;
      Name       : String)
   return Boolean
   is
   begin
      return Get_By_Record_Key (Kit_Record, Name) /= Null_Kit_Key_Reference;
   end Is_Record_Key;

   ---------------
   -- Is_Unique --
   ---------------

   overriding function Is_Unique (Item : Kit_Key_Implementation)
      return Boolean is
      Result : Boolean
        renames Item.T17_Data.Is_Unique;
   begin
      return Result;
   end Is_Unique;

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

   overriding function Kit_Record (Item : Kit_Key_Implementation)
      return Kit_Record_Reference is
      Result : Kit_Record_Reference
        renames Item.T17_Data.Kit_Record;
   begin
      return Result;
   end Kit_Record;

   ----------
   -- Last --
   ----------

   function Last (Container : Selection) return Cursor is
   begin
      return (Current => Container.Elements.Last);
   end Last;

   ----------
   -- Last --
   ----------

   overriding function Last (Object : Iterator) return Cursor is
   begin
      return (Current => Object.Container.Elements.Last);
   end Last;

   ------------------------
   -- Last_By_Kit_Record --
   ------------------------

   function Last_By_Kit_Record (Kit_Record : Kit_Record_Reference)
      return Kit_Key_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (17);
      Kit_Key_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T17_Kit_Record_Ref,
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
      Kit_Key_Impl.File_Mutex.Shared_Unlock;
      return Kit_Key_Reference (Db_Index);
   end Last_By_Kit_Record;

   ------------------------
   -- Last_By_Top_Record --
   ------------------------

   function Last_By_Top_Record (Top_Record : Record_Type)
      return Kit_Key_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Key_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T17_Top_Record_Ref,
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
      Kit_Key_Impl.File_Mutex.Shared_Unlock;
      return Kit_Key_Reference (Db_Index);
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

   ------------
   -- Length --
   ------------

   overriding function Length (Item : Kit_Key_Implementation)
      return Integer is
      Result : Integer
        renames Item.T17_Data.Length;
   begin
      return Result;
   end Length;

   ----------
   -- Name --
   ----------

   overriding function Name (Item : Kit_Key_Implementation) return String is
      Result : Kit.Strings.String_Type
        renames Item.T17_Data.Name;
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
     (Handle         : Kit_Key_Notify_Record;
      Changed_Record : Marlowe.Database_Index)
   is
   begin
      Handle.Handler (Kit_Key_Reference (Changed_Record));
   end Notify_Record_Change;

   -------------------------
   -- Notify_Table_Change --
   -------------------------

   overriding procedure Notify_Table_Change (Handle :
      Kit_Key_Table_Notify_Record) is
   begin
      Handle.Handler.all;
   end Notify_Table_Change;

   -----------------------
   -- On_Kit_Key_Change --
   -----------------------

   procedure On_Kit_Key_Change
     (Reference : Kit_Key_Reference;
      Handler   : Kit_Key_Notify_Handler)
   is
      Rec : Kit_Key_Notify_Record;
   begin
      Rec.Handler := Handler;
      Kit.Notifier.Add_Record_Change_Handler
        (17,
         Marlowe.Database_Index (Reference),
         Rec);
   end On_Kit_Key_Change;

   ------------------------
   -- On_Kit_Key_Created --
   ------------------------

   procedure On_Kit_Key_Created (Handler : Kit_Key_Notify_Handler) is
      Rec : Kit_Key_Notify_Record;
   begin
      Rec.Handler := Handler;
      Kit.Notifier.Add_Record_Create_Handler (17, Rec);
   end On_Kit_Key_Created;

   ------------------------
   -- On_Kit_Key_Deleted --
   ------------------------

   procedure On_Kit_Key_Deleted (Handler : Kit_Key_Notify_Handler) is
      Rec : Kit_Key_Notify_Record;
   begin
      Rec.Handler := Handler;
      Kit.Notifier.Add_Record_Delete_Handler (17, Rec);
   end On_Kit_Key_Deleted;

   -----------------------------
   -- On_Kit_Key_Table_Change --
   -----------------------------

   procedure On_Kit_Key_Table_Change (Handler : Kit_Key_Table_Notify_Handler)
      is
      Rec : Kit_Key_Table_Notify_Record;
   begin
      Rec.Handler := Handler;
      Kit.Notifier.Add_Table_Change_Handler (17, Rec);
   end On_Kit_Key_Table_Change;

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

   ------------------------
   -- Scan_By_Kit_Record --
   ------------------------

   function Scan_By_Kit_Record return Selection is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (17);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array (1 .. 16) := (others => 0);
         Last_Key : constant Storage_Array (1 .. 16) :=
            (others => System.Storage_Elements.Storage_Element'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T17_Kit_Record_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Scan_By_Kit_Record;

   ------------------------
   -- Scan_By_Record_Key --
   ------------------------

   function Scan_By_Record_Key return Selection is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (17);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array (1 .. 80) := (others => 0);
         Last_Key : constant Storage_Array (1 .. 80) :=
            (others => System.Storage_Elements.Storage_Element'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T17_Record_Key_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Scan_By_Record_Key;

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
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T17_Top_Record_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Reference
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
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T17_Top_Record_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_Bounded_By_Top_Record;

   --------------------------
   -- Select_By_Kit_Record --
   --------------------------

   function Select_By_Kit_Record (Kit_Record : Kit_Record_Reference)
      return Selection is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (17);
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
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T17_Kit_Record_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_By_Kit_Record;

   --------------------------
   -- Select_By_Record_Key --
   --------------------------

   function Select_By_Record_Key
     (Kit_Record : Kit_Record_Reference;
      Name       : String)
   return Selection
   is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (17);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array :=
            Kit_Key_Impl.Record_Key_To_Storage (Kit_Record, Name)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First);
         Last_Key : constant Storage_Array :=
            Kit_Key_Impl.Record_Key_To_Storage (Kit_Record, Name)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T17_Record_Key_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_By_Record_Key;

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
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T17_Top_Record_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_By_Top_Record;

   ---------------------------------------
   -- Select_Record_Key_Bounded_By_Name --
   ---------------------------------------

   function Select_Record_Key_Bounded_By_Name
     (Kit_Record  : Kit_Record_Reference;
      Start_Name  : String;
      Finish_Name : String)
   return Selection
   is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (17);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array :=
            Kit_Key_Impl.Record_Key_To_Storage (Kit_Record, Start_Name)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First);
         Last_Key : constant Storage_Array :=
            Kit_Key_Impl.Record_Key_To_Storage (Kit_Record, Finish_Name)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T17_Record_Key_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_Record_Key_Bounded_By_Name;

   ---------
   -- Set --
   ---------

   overriding procedure Set
     (Item  : in out Kit_Key_Update_Implementation;
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
      elsif Field = "is_unique" then
         Item.Set_Is_Unique (Boolean'Value (Value));
      elsif Field = "length" then
         Item.Set_Length (Natural'Value (Value));
      else
         raise Constraint_Error with
           "no such field";
      end if;
   end Set;

   -------------------
   -- Set_Is_Unique --
   -------------------

   overriding procedure Set_Is_Unique
     (Item  : in out Kit_Key_Update_Implementation;
      Value : Boolean)
   is
      Target : Boolean
        renames Item.T17_Data.Is_Unique;
   begin
      Database_Mutex.Shared_Lock;
      Item.X_Lock;
      Target := Value;
      Database_Mutex.Shared_Unlock;
   end Set_Is_Unique;

   --------------------
   -- Set_Kit_Record --
   --------------------

   overriding procedure Set_Kit_Record
     (Item  : in out Kit_Key_Update_Implementation;
      Value : Kit.Db.Kit_Record.Kit_Record_Type)
   is
   begin
      Item.Set_Kit_Record (Value.Get_Kit_Record_Reference);
   end Set_Kit_Record;

   --------------------
   -- Set_Kit_Record --
   --------------------

   overriding procedure Set_Kit_Record
     (Item  : in out Kit_Key_Update_Implementation;
      Value : Kit_Record_Reference)
   is
      Target : Kit_Record_Reference
        renames Item.T17_Data.Kit_Record;
   begin
      Database_Mutex.Shared_Lock;
      Item.X_Lock;
      if not Item.Created then
         Kit_Deferred_Keys.Key_Changed
           (17,
            Marlowe_Keys.T17_Kit_Record_Ref,
            Marlowe.Database_Index (Item.M_Index),
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
               (Item.Kit_Record)),
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
               (Value)));
      end if;
      if not Item.Created then
         Kit_Deferred_Keys.Key_Changed
           (17,
            Marlowe_Keys.T17_Record_Key_Ref,
            Marlowe.Database_Index (Item.M_Index),
            Kit_Key_Impl.Record_Key_To_Storage (Item.Kit_Record, Item.Name),
            Kit_Key_Impl.Record_Key_To_Storage (Value, Item.Name));
      end if;
      Target := Value;
      Database_Mutex.Shared_Unlock;
   end Set_Kit_Record;

   ----------------
   -- Set_Length --
   ----------------

   overriding procedure Set_Length
     (Item  : in out Kit_Key_Update_Implementation;
      Value : Integer)
   is
      Target : Integer
        renames Item.T17_Data.Length;
   begin
      Database_Mutex.Shared_Lock;
      Item.X_Lock;
      Target := Value;
      Database_Mutex.Shared_Unlock;
   end Set_Length;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Item  : in out Kit_Key_Update_Implementation;
      Value : String)
   is
      Target : Kit.Strings.String_Type
        renames Item.T17_Data.Name;
   begin
      Database_Mutex.Shared_Lock;
      Item.X_Lock;
      if not Item.Created then
         Kit_Deferred_Keys.Key_Changed
           (17,
            Marlowe_Keys.T17_Record_Key_Ref,
            Marlowe.Database_Index (Item.M_Index),
            Kit_Key_Impl.Record_Key_To_Storage (Item.Kit_Record, Item.Name),
            Kit_Key_Impl.Record_Key_To_Storage (Item.Kit_Record, Value));
      end if;
      Target.Length := Value'Length;
      Target.Text (1 .. Value'Length) := Value;
      Database_Mutex.Shared_Unlock;
   end Set_Name;

   ----------------
   -- Top_Record --
   ----------------

   overriding function Top_Record (Item : Kit_Key_Implementation)
      return Record_Type is
      Result : Record_Type
        renames Item.T1_Data.Top_Record;
   begin
      return Result;
   end Top_Record;

   ------------
   -- X_Lock --
   ------------

   overriding procedure X_Lock (Item : Kit_Key_Implementation) is
   begin
      Kit_Root_Record_Cache.X_Lock (Marlowe.Database_Index
         (Item.T17_Data.Kit_Root_Record));
      Kit_Key_Cache.X_Lock (Marlowe.Database_Index (Item.M_Index));
   end X_Lock;

end Kit.Db.Kit_Key;
