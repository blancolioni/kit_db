with Ada.Finalization;
with System.Storage_Elements;
with Marlowe.Data_Stores;
with Marlowe.Key_Storage;
with Kit.Cache;
with Kit.Notifier;
with Kit.Db.Kit_Deferred_Keys;
with Kit.Db.Marlowe_Keys;
with Kit.Db.Kit_Root_Record_Cache;
with Kit.Db.Kit_Root_Record_Impl;
with Kit.Db.Kit_Key_Field_Impl;
with Kit.Db.Kit_Key_Field_Cache;

package body Kit.Db.Kit_Key_Field is

   type Kit_Key_Field_Implementation is
        limited new Ada.Finalization.Limited_Controlled
     and Kit_Key_Field_Interface with
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
         M_Index         : Kit_Key_Field_Reference;
         T1_Data         :
            Kit_Root_Record_Impl.Kit_Root_Record_Database_Record;
         T18_Data        : Kit_Key_Field_Impl.Kit_Key_Field_Database_Record;
      end record;
   overriding function Has_Element (Item : Kit_Key_Field_Implementation)
      return Boolean;
   overriding procedure X_Lock (Item : Kit_Key_Field_Implementation);
   overriding function Get_Kit_Root_Record_Reference (Item :
      Kit_Key_Field_Implementation) return Kit_Root_Record_Reference;
   overriding function Get_Kit_Key_Field_Reference (Item :
      Kit_Key_Field_Implementation) return Kit_Key_Field_Reference;
   overriding function Top_Record (Item : Kit_Key_Field_Implementation)
      return Record_Type;
   overriding function Kit_Key (Item : Kit_Key_Field_Implementation)
      return Kit_Key_Reference;
   overriding function Kit_Field (Item : Kit_Key_Field_Implementation)
      return Kit_Field_Reference;
   overriding function Identity (Item : Kit_Key_Field_Implementation)
      return String;
   overriding function Get
     (Item  : Kit_Key_Field_Implementation;
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
   type Kit_Key_Field_Table_Notify_Record is
        new Kit.Notifier.Table_Notify_Interface with
      record
         Handler : Kit_Key_Field_Table_Notify_Handler;
      end record;
   overriding procedure Notify_Table_Change (Handle :
      Kit_Key_Field_Table_Notify_Record);
   type Kit_Key_Field_Notify_Record is
        new Kit.Notifier.Record_Notify_Interface with
      record
         Handler : Kit_Key_Field_Notify_Handler;
      end record;
   overriding procedure Notify_Record_Change
     (Handle         : Kit_Key_Field_Notify_Record;
      Changed_Record : Marlowe.Database_Index);
   type Kit_Key_Field_Update_Implementation is
        limited new Kit_Key_Field_Implementation
     and Kit_Key_Field_Update_Interface
     and Record_Update_Interface with null record;
   overriding procedure Initialize (Item :
      in out Kit_Key_Field_Update_Implementation);
   overriding procedure Finalize (Item :
      in out Kit_Key_Field_Update_Implementation);
   overriding procedure Delete (Item :
      in out Kit_Key_Field_Update_Implementation);
   overriding procedure Set
     (Item  : in out Kit_Key_Field_Update_Implementation;
      Field : String;
      Value : String);
   overriding procedure Set_Kit_Key
     (Item  : in out Kit_Key_Field_Update_Implementation;
      Value : Kit_Key_Reference);
   overriding procedure Set_Kit_Key
     (Item  : in out Kit_Key_Field_Update_Implementation;
      Value : Kit.Db.Kit_Key.Kit_Key_Type);
   overriding procedure Set_Kit_Field
     (Item  : in out Kit_Key_Field_Update_Implementation;
      Value : Kit_Field_Reference);
   overriding procedure Set_Kit_Field
     (Item  : in out Kit_Key_Field_Update_Implementation;
      Value : Kit.Db.Kit_Field.Kit_Field_Type);

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
     (Kit_Key   : Kit_Key_Reference;
      Kit_Field : Kit_Field_Reference)
   is
      Result : Kit_Key_Field_Update := Create;
   begin
      Result.Set_Kit_Key (Kit_Key);
      Result.Set_Kit_Field (Kit_Field);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Kit_Key   : Kit_Key_Reference;
      Kit_Field : Kit_Field_Reference)
   return Kit_Key_Field_Reference
   is
      Result : Kit_Key_Field_Update := Create;
   begin
      Result.Set_Kit_Key (Kit_Key);
      Result.Set_Kit_Field (Kit_Field);
      return Result.Get_Kit_Key_Field_Reference;
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Kit_Key_Field_Update is
      T1_Access : Kit_Root_Record_Cache.Cache_Access;
      T18_Access : Kit_Key_Field_Cache.Cache_Access;
   begin
      return Result : Kit_Key_Field_Update_Implementation do
         Result.Finished := False;
         Result.Created := True;
         Result.Deleted := False;
         Result.Scanning := False;
         Result.Read_Only := False;
         Result.Using_Key := False;
         Result.M_Index := 0;
         Memory_Mutex.Lock;
         T1_Access := new Kit_Root_Record_Cache.Cache_Record;
         T1_Access.Db.Top_Record := R_Kit_Key_Field;
         T18_Access := new Kit_Key_Field_Cache.Cache_Record;
         Memory_Mutex.Unlock;
         Kit_Root_Record_Impl.File_Mutex.Lock;
         T18_Access.Db.Kit_Root_Record := Kit_Root_Record_Reference
            (Marlowe_Keys.Handle.Insert_Record (1));
         T1_Access.Initialise (1, Marlowe.Database_Index
            (T18_Access.Db.Kit_Root_Record));
         Kit.Cache.Insert (Kit.Cache.Cache_Entry (T1_Access));
         Kit_Root_Record_Impl.Write (Marlowe.Database_Index
            (T18_Access.Db.Kit_Root_Record), T1_Access.Db);
         T1_Access.X_Lock;
         Kit_Root_Record_Impl.File_Mutex.Unlock;
         Kit_Key_Field_Impl.File_Mutex.Lock;
         Result.M_Index := Kit_Key_Field_Reference
            (Marlowe_Keys.Handle.Insert_Record (18));
         T18_Access.Initialise (18, Marlowe.Database_Index (Result.M_Index));
         Kit.Cache.Insert (Kit.Cache.Cache_Entry (T18_Access));
         Kit_Key_Field_Impl.Write (Marlowe.Database_Index (Result.M_Index),
            T18_Access.Db);
         T18_Access.X_Lock;
         Kit_Key_Field_Impl.File_Mutex.Unlock;
         Result.T1_Data := T1_Access.Db;
         Result.T18_Data := T18_Access.Db;
         Result.Read_Only := False;
      end return;
   end Create;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete (Item :
      in out Kit_Key_Field_Update_Implementation) is
   begin
      Database_Mutex.Shared_Lock;
      Item.Deleted := True;
      Database_Mutex.Shared_Unlock;
   end Delete;

   -------------
   -- Element --
   -------------

   function Element (Item : Cursor) return Kit_Key_Field_Reference is
   begin
      return List_Of_References.Element (Item.Current);
   end Element;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Item :
      in out Kit_Key_Field_Update_Implementation) is
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
            (Item.T18_Data.Kit_Root_Record)));
         Marlowe_Keys.Handle.Delete (Marlowe_Keys.T18_Top_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Item.Top_Record), 4)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Delete (Marlowe_Keys.T18_T1_Idx_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.T18_Data.Kit_Root_Record))
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Delete (Marlowe_Keys.T18_Kit_Key_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.Kit_Key))
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Delete (Marlowe_Keys.T18_Kit_Field_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.Kit_Field))
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Delete (Marlowe_Keys.T18_Key_Field_Ref,
            Kit_Key_Field_Impl.Key_Field_To_Storage (Item.Kit_Key,
            Item.Kit_Field)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Item.T1_Data.Deleted := True;
         Item.T18_Data.Deleted := True;
         Kit.Notifier.Record_Deleted (1, Marlowe.Database_Index
            (Item.T18_Data.Kit_Root_Record));
         Kit.Notifier.Record_Deleted (18, Marlowe.Database_Index
            (Item.M_Index));
      end if;
      if not (Item.Read_Only or else Item.Created) or else Item.Deleted then
         Kit.Notifier.Record_Changed (1, Marlowe.Database_Index
            (Item.T18_Data.Kit_Root_Record));
         Kit.Notifier.Record_Changed (18, Marlowe.Database_Index
            (Item.M_Index));
      end if;
      if (not Item.Read_Only) and then Item.Created then
         Database_Mutex.Shared_Lock;
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T1_Top_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Item.Top_Record), 4)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.T18_Data.Kit_Root_Record)));
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T18_Top_Record_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Record_Type'Pos
            (Item.Top_Record), 4)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T18_T1_Idx_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.T18_Data.Kit_Root_Record))
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T18_Kit_Key_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.Kit_Key))
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T18_Kit_Field_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.Kit_Field))
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Marlowe_Keys.Handle.Insert (Marlowe_Keys.T18_Key_Field_Ref,
            Kit_Key_Field_Impl.Key_Field_To_Storage (Item.Kit_Key,
            Item.Kit_Field)
         & Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Item.M_Index)));
         Database_Mutex.Shared_Unlock;
         Kit.Notifier.Record_Created (1, Marlowe.Database_Index
            (Item.T18_Data.Kit_Root_Record));
         Kit.Notifier.Record_Created (18, Marlowe.Database_Index
            (Item.M_Index));
      end if;
      if not Item.Read_Only then
         declare
            T1_Access : constant Kit_Root_Record_Cache.Cache_Access :=
               Kit_Root_Record_Cache.Get (Marlowe.Database_Index
               (Item.T18_Data.Kit_Root_Record));
         begin
            T1_Access.Db := Item.T1_Data;
         end;
         declare
            T18_Access : constant Kit_Key_Field_Cache.Cache_Access :=
               Kit_Key_Field_Cache.Get (Marlowe.Database_Index
               (Item.M_Index));
         begin
            T18_Access.Db := Item.T18_Data;
         end;
      end if;
      if not Item.Read_Only then
         Kit_Root_Record_Cache.Unlock (Marlowe.Database_Index
            (Item.T18_Data.Kit_Root_Record));
         Kit_Key_Field_Cache.Unlock (Marlowe.Database_Index (Item.M_Index));
      end if;
      Item.M_Index := 0;
      Item.Created := False;
   end Finalize;

   -----------
   -- First --
   -----------

   function First (Container : Selection) return Cursor is
   begin
      return (Current => Container.Elements.First);
   end First;

   -----------
   -- First --
   -----------

   overriding function First (Object : Iterator) return Cursor is
   begin
      return (Current => Object.Container.Elements.First);
   end First;

   ------------------------
   -- First_By_Kit_Field --
   ------------------------

   function First_By_Kit_Field (Kit_Field : Kit_Field_Reference)
      return Kit_Key_Field_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (18);
      Kit_Key_Field_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_Kit_Field_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Field)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First),
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Field)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last), Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         if M.Valid then
            Db_Index := Marlowe.Key_Storage.To_Database_Index (M.Get_Key);
         end if;
      end;
      Kit_Key_Field_Impl.File_Mutex.Shared_Unlock;
      return Kit_Key_Field_Reference (Db_Index);
   end First_By_Kit_Field;

   ----------------------
   -- First_By_Kit_Key --
   ----------------------

   function First_By_Kit_Key (Kit_Key : Kit_Key_Reference)
      return Kit_Key_Field_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (18);
      Kit_Key_Field_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_Kit_Key_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Key)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First),
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Key)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last), Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         if M.Valid then
            Db_Index := Marlowe.Key_Storage.To_Database_Index (M.Get_Key);
         end if;
      end;
      Kit_Key_Field_Impl.File_Mutex.Shared_Unlock;
      return Kit_Key_Field_Reference (Db_Index);
   end First_By_Kit_Key;

   -------------------------
   -- First_By_Top_Record --
   -------------------------

   function First_By_Top_Record (Top_Record : Record_Type)
      return Kit_Key_Field_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Key_Field_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_Top_Record_Ref,
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
      Kit_Key_Field_Impl.File_Mutex.Shared_Unlock;
      return Kit_Key_Field_Reference (Db_Index);
   end First_By_Top_Record;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Item  : Kit_Key_Field_Implementation;
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
      elsif Field = "kit_key" then
         return Kit_Key_Reference'Image (Item.Kit_Key);
      elsif Field = "kit_field" then
         return Kit_Field_Reference'Image (Item.Kit_Field);
      else
         raise Constraint_Error with
           "no such field";
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Ref : Kit_Key_Field_Reference) return Kit_Key_Field_Type is
   begin
      return Result : Kit_Key_Field_Implementation do
         Kit_Key_Field_Impl.File_Mutex.Shared_Lock;
         Result.M_Index := Ref;
         if Result.M_Index /= Null_Kit_Key_Field_Reference then
            Kit_Key_Field_Cache.S_Lock (Marlowe.Database_Index
               (Result.M_Index));
            Result.T18_Data := Kit_Key_Field_Cache.Get
               (Marlowe.Database_Index (Result.M_Index), False).Db;
            Kit_Root_Record_Cache.S_Lock (Marlowe.Database_Index
               (Result.T18_Data.Kit_Root_Record));
            Result.T1_Data := Kit_Root_Record_Cache.Get
               (Marlowe.Database_Index (Result.T18_Data.Kit_Root_Record),
               False).Db;
            Kit_Key_Field_Cache.Unlock (Marlowe.Database_Index
               (Result.M_Index));
            Kit_Root_Record_Cache.Unlock (Marlowe.Database_Index
               (Result.T18_Data.Kit_Root_Record));
         end if;
         Result.Finished := False;
         Result.Using_Key_Value := False;
         Result.Scanning := False;
         Kit_Key_Field_Impl.File_Mutex.Shared_Unlock;
      end return;
   end Get;

   ----------------------
   -- Get_By_Key_Field --
   ----------------------

   function Get_By_Key_Field
     (Kit_Key   : Kit_Key_Reference;
      Kit_Field : Kit_Field_Reference)
   return Kit_Key_Field_Reference
   is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (18);
      Kit_Key_Field_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_Key_Field_Ref,
            Kit_Key_Field_Impl.Key_Field_To_Storage (Kit_Key, Kit_Field)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First),
            Kit_Key_Field_Impl.Key_Field_To_Storage (Kit_Key, Kit_Field)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last), Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         if M.Valid then
            Db_Index := Marlowe.Key_Storage.To_Database_Index (M.Get_Key);
         end if;
      end;
      Kit_Key_Field_Impl.File_Mutex.Shared_Unlock;
      return Kit_Key_Field_Reference (Db_Index);
   end Get_By_Key_Field;

   ------------------------------
   -- Get_From_Kit_Root_Record --
   ------------------------------

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit_Root_Record_Reference) return Kit_Key_Field_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (18);
      Kit_Key_Field_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_T1_Idx_Ref,
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
      Kit_Key_Field_Impl.File_Mutex.Shared_Unlock;
      return Kit_Key_Field_Reference (Db_Index);
   end Get_From_Kit_Root_Record;

   ---------------------------------
   -- Get_Kit_Key_Field_Reference --
   ---------------------------------

   overriding function Get_Kit_Key_Field_Reference (Item :
      Kit_Key_Field_Implementation) return Kit_Key_Field_Reference
   is (Item.M_Index);

   -----------------------------------
   -- Get_Kit_Root_Record_Reference --
   -----------------------------------

   overriding function Get_Kit_Root_Record_Reference (Item :
      Kit_Key_Field_Implementation) return Kit_Root_Record_Reference
   is (Item.T18_Data.Kit_Root_Record);

   ----------------
   -- Get_Update --
   ----------------

   function Get_Update (Ref : Kit_Key_Field_Reference)
      return Kit_Key_Field_Update is
   begin
      return Result : Kit_Key_Field_Update_Implementation do
         Kit_Key_Field_Impl.File_Mutex.Shared_Lock;
         Result.M_Index := Ref;
         if Result.M_Index /= Null_Kit_Key_Field_Reference then
            Kit_Key_Field_Cache.U_Lock (Marlowe.Database_Index
               (Result.M_Index));
            Result.T18_Data := Kit_Key_Field_Cache.Get
               (Marlowe.Database_Index (Result.M_Index), False).Db;
            Kit_Root_Record_Cache.U_Lock (Marlowe.Database_Index
               (Result.T18_Data.Kit_Root_Record));
            Result.T1_Data := Kit_Root_Record_Cache.Get
               (Marlowe.Database_Index (Result.T18_Data.Kit_Root_Record),
               False).Db;
         end if;
         Result.Finished := False;
         Result.Using_Key_Value := False;
         Result.Scanning := False;
         Result.Read_Only := False;
         Kit_Key_Field_Impl.File_Mutex.Shared_Unlock;
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

   overriding function Has_Element (Item : Kit_Key_Field_Implementation)
      return Boolean is
   begin
      return Item.M_Index /= 0;
   end Has_Element;

   --------------
   -- Identity --
   --------------

   overriding function Identity (Item : Kit_Key_Field_Implementation)
      return String is
   begin
      return "kit_key_field" & Kit_Key_Field_Reference'Image
         (Item.Get_Kit_Key_Field_Reference);
   end Identity;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Item :
      in out Kit_Key_Field_Update_Implementation) is
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

   ------------------
   -- Is_Key_Field --
   ------------------

   function Is_Key_Field
     (Kit_Key   : Kit_Key_Reference;
      Kit_Field : Kit_Field_Reference)
   return Boolean
   is
   begin
      return Get_By_Key_Field (Kit_Key, Kit_Field)
         /= Null_Kit_Key_Field_Reference;
   end Is_Key_Field;

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

   ---------------
   -- Kit_Field --
   ---------------

   overriding function Kit_Field (Item : Kit_Key_Field_Implementation)
      return Kit_Field_Reference is
      Result : Kit_Field_Reference
        renames Item.T18_Data.Kit_Field;
   begin
      return Result;
   end Kit_Field;

   -------------
   -- Kit_Key --
   -------------

   overriding function Kit_Key (Item : Kit_Key_Field_Implementation)
      return Kit_Key_Reference is
      Result : Kit_Key_Reference
        renames Item.T18_Data.Kit_Key;
   begin
      return Result;
   end Kit_Key;

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

   -----------------------
   -- Last_By_Kit_Field --
   -----------------------

   function Last_By_Kit_Field (Kit_Field : Kit_Field_Reference)
      return Kit_Key_Field_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (18);
      Kit_Key_Field_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_Kit_Field_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Field)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First),
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Field)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last), Marlowe.Closed, Marlowe.Closed,
            Marlowe.Backward);
      begin
         if M.Valid then
            Db_Index := Marlowe.Key_Storage.To_Database_Index (M.Get_Key);
         end if;
      end;
      Kit_Key_Field_Impl.File_Mutex.Shared_Unlock;
      return Kit_Key_Field_Reference (Db_Index);
   end Last_By_Kit_Field;

   ---------------------
   -- Last_By_Kit_Key --
   ---------------------

   function Last_By_Kit_Key (Kit_Key : Kit_Key_Reference)
      return Kit_Key_Field_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (18);
      Kit_Key_Field_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_Kit_Key_Ref,
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Key)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First),
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Key)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last), Marlowe.Closed, Marlowe.Closed,
            Marlowe.Backward);
      begin
         if M.Valid then
            Db_Index := Marlowe.Key_Storage.To_Database_Index (M.Get_Key);
         end if;
      end;
      Kit_Key_Field_Impl.File_Mutex.Shared_Unlock;
      return Kit_Key_Field_Reference (Db_Index);
   end Last_By_Kit_Key;

   ------------------------
   -- Last_By_Top_Record --
   ------------------------

   function Last_By_Top_Record (Top_Record : Record_Type)
      return Kit_Key_Field_Reference is
      use type System.Storage_Elements.Storage_Array;
      Db_Index : Marlowe.Database_Index := 0;
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Key_Field_Impl.File_Mutex.Shared_Lock;
      declare
         M : constant Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_Top_Record_Ref,
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
      Kit_Key_Field_Impl.File_Mutex.Shared_Unlock;
      return Kit_Key_Field_Reference (Db_Index);
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
     (Handle         : Kit_Key_Field_Notify_Record;
      Changed_Record : Marlowe.Database_Index)
   is
   begin
      Handle.Handler (Kit_Key_Field_Reference (Changed_Record));
   end Notify_Record_Change;

   -------------------------
   -- Notify_Table_Change --
   -------------------------

   overriding procedure Notify_Table_Change (Handle :
      Kit_Key_Field_Table_Notify_Record) is
   begin
      Handle.Handler.all;
   end Notify_Table_Change;

   -----------------------------
   -- On_Kit_Key_Field_Change --
   -----------------------------

   procedure On_Kit_Key_Field_Change
     (Reference : Kit_Key_Field_Reference;
      Handler   : Kit_Key_Field_Notify_Handler)
   is
      Rec : Kit_Key_Field_Notify_Record;
   begin
      Rec.Handler := Handler;
      Kit.Notifier.Add_Record_Change_Handler
        (18,
         Marlowe.Database_Index (Reference),
         Rec);
   end On_Kit_Key_Field_Change;

   ------------------------------
   -- On_Kit_Key_Field_Created --
   ------------------------------

   procedure On_Kit_Key_Field_Created (Handler :
      Kit_Key_Field_Notify_Handler) is
      Rec : Kit_Key_Field_Notify_Record;
   begin
      Rec.Handler := Handler;
      Kit.Notifier.Add_Record_Create_Handler (18, Rec);
   end On_Kit_Key_Field_Created;

   ------------------------------
   -- On_Kit_Key_Field_Deleted --
   ------------------------------

   procedure On_Kit_Key_Field_Deleted (Handler :
      Kit_Key_Field_Notify_Handler) is
      Rec : Kit_Key_Field_Notify_Record;
   begin
      Rec.Handler := Handler;
      Kit.Notifier.Add_Record_Delete_Handler (18, Rec);
   end On_Kit_Key_Field_Deleted;

   -----------------------------------
   -- On_Kit_Key_Field_Table_Change --
   -----------------------------------

   procedure On_Kit_Key_Field_Table_Change (Handler :
      Kit_Key_Field_Table_Notify_Handler) is
      Rec : Kit_Key_Field_Table_Notify_Record;
   begin
      Rec.Handler := Handler;
      Kit.Notifier.Add_Table_Change_Handler (18, Rec);
   end On_Kit_Key_Field_Table_Change;

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

   -----------------------
   -- Scan_By_Key_Field --
   -----------------------

   function Scan_By_Key_Field return Selection is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (18);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array (1 .. 24) := (others => 0);
         Last_Key : constant Storage_Array (1 .. 24) :=
            (others => System.Storage_Elements.Storage_Element'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_Key_Field_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Scan_By_Key_Field;

   -----------------------
   -- Scan_By_Kit_Field --
   -----------------------

   function Scan_By_Kit_Field return Selection is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (18);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array (1 .. 16) := (others => 0);
         Last_Key : constant Storage_Array (1 .. 16) :=
            (others => System.Storage_Elements.Storage_Element'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_Kit_Field_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Scan_By_Kit_Field;

   ---------------------
   -- Scan_By_Kit_Key --
   ---------------------

   function Scan_By_Kit_Key return Selection is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (18);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array (1 .. 16) := (others => 0);
         Last_Key : constant Storage_Array (1 .. 16) :=
            (others => System.Storage_Elements.Storage_Element'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_Kit_Key_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Scan_By_Kit_Key;

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
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_Top_Record_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Field_Reference
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
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_Top_Record_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_Bounded_By_Top_Record;

   -------------------------
   -- Select_By_Key_Field --
   -------------------------

   function Select_By_Key_Field
     (Kit_Key   : Kit_Key_Reference;
      Kit_Field : Kit_Field_Reference)
   return Selection
   is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (18);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array :=
            Kit_Key_Field_Impl.Key_Field_To_Storage (Kit_Key, Kit_Field)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First);
         Last_Key : constant Storage_Array :=
            Kit_Key_Field_Impl.Key_Field_To_Storage (Kit_Key, Kit_Field)
            & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_Key_Field_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_By_Key_Field;

   -------------------------
   -- Select_By_Kit_Field --
   -------------------------

   function Select_By_Kit_Field (Kit_Field : Kit_Field_Reference)
      return Selection is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (18);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array :=
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Field)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First);
         Last_Key : constant Storage_Array :=
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Field)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_Kit_Field_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_By_Kit_Field;

   -----------------------
   -- Select_By_Kit_Key --
   -----------------------

   function Select_By_Kit_Key (Kit_Key : Kit_Key_Reference)
      return Selection is
   begin
      Kit_Deferred_Keys.Check_Keys (1);
      Kit_Deferred_Keys.Check_Keys (18);
      declare
         use System.Storage_Elements;
         First_Key : constant Storage_Array :=
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Key)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'First);
         Last_Key : constant Storage_Array :=
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
            (Kit_Key)) & Marlowe.Key_Storage.To_Storage_Array
            (Marlowe.Database_Index'Last);
         Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_Kit_Key_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_By_Kit_Key;

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
            Marlowe_Keys.Handle.Search (Marlowe_Keys.T18_Top_Record_Ref,
            First_Key, Last_Key, Marlowe.Closed, Marlowe.Closed,
            Marlowe.Forward);
      begin
         return Result : Selection do
            while Mark.Valid loop
               Result.Elements.Append (Kit_Key_Field_Reference
                  (Marlowe.Key_Storage.To_Database_Index (Mark.Get_Key)));
               Mark.Next;
            end loop;
         end return;
      end;
   end Select_By_Top_Record;

   ---------
   -- Set --
   ---------

   overriding procedure Set
     (Item  : in out Kit_Key_Field_Update_Implementation;
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
      elsif Field = "kit_key" then
         Item.Set_Kit_Key (Kit_Key_Reference'Value (Value));
      elsif Field = "kit_field" then
         Item.Set_Kit_Field (Kit_Field_Reference'Value (Value));
      else
         raise Constraint_Error with
           "no such field";
      end if;
   end Set;

   -------------------
   -- Set_Kit_Field --
   -------------------

   overriding procedure Set_Kit_Field
     (Item  : in out Kit_Key_Field_Update_Implementation;
      Value : Kit.Db.Kit_Field.Kit_Field_Type)
   is
   begin
      Item.Set_Kit_Field (Value.Get_Kit_Field_Reference);
   end Set_Kit_Field;

   -------------------
   -- Set_Kit_Field --
   -------------------

   overriding procedure Set_Kit_Field
     (Item  : in out Kit_Key_Field_Update_Implementation;
      Value : Kit_Field_Reference)
   is
      Target : Kit_Field_Reference
        renames Item.T18_Data.Kit_Field;
   begin
      Database_Mutex.Shared_Lock;
      Item.X_Lock;
      if not Item.Created then
         Kit_Deferred_Keys.Key_Changed
           (18,
            Marlowe_Keys.T18_Kit_Field_Ref,
            Marlowe.Database_Index (Item.M_Index),
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
               (Item.Kit_Field)),
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
               (Value)));
      end if;
      if not Item.Created then
         Kit_Deferred_Keys.Key_Changed
           (18,
            Marlowe_Keys.T18_Key_Field_Ref,
            Marlowe.Database_Index (Item.M_Index),
            Kit_Key_Field_Impl.Key_Field_To_Storage (Item.Kit_Key,
               Item.Kit_Field),
            Kit_Key_Field_Impl.Key_Field_To_Storage (Item.Kit_Key, Value));
      end if;
      Target := Value;
      Database_Mutex.Shared_Unlock;
   end Set_Kit_Field;

   -----------------
   -- Set_Kit_Key --
   -----------------

   overriding procedure Set_Kit_Key
     (Item  : in out Kit_Key_Field_Update_Implementation;
      Value : Kit.Db.Kit_Key.Kit_Key_Type)
   is
   begin
      Item.Set_Kit_Key (Value.Get_Kit_Key_Reference);
   end Set_Kit_Key;

   -----------------
   -- Set_Kit_Key --
   -----------------

   overriding procedure Set_Kit_Key
     (Item  : in out Kit_Key_Field_Update_Implementation;
      Value : Kit_Key_Reference)
   is
      Target : Kit_Key_Reference
        renames Item.T18_Data.Kit_Key;
   begin
      Database_Mutex.Shared_Lock;
      Item.X_Lock;
      if not Item.Created then
         Kit_Deferred_Keys.Key_Changed
           (18,
            Marlowe_Keys.T18_Kit_Key_Ref,
            Marlowe.Database_Index (Item.M_Index),
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
               (Item.Kit_Key)),
            Marlowe.Key_Storage.To_Storage_Array (Marlowe.Database_Index
               (Value)));
      end if;
      if not Item.Created then
         Kit_Deferred_Keys.Key_Changed
           (18,
            Marlowe_Keys.T18_Key_Field_Ref,
            Marlowe.Database_Index (Item.M_Index),
            Kit_Key_Field_Impl.Key_Field_To_Storage (Item.Kit_Key,
               Item.Kit_Field),
            Kit_Key_Field_Impl.Key_Field_To_Storage (Value, Item.Kit_Field));
      end if;
      Target := Value;
      Database_Mutex.Shared_Unlock;
   end Set_Kit_Key;

   ----------------
   -- Top_Record --
   ----------------

   overriding function Top_Record (Item : Kit_Key_Field_Implementation)
      return Record_Type is
      Result : Record_Type
        renames Item.T1_Data.Top_Record;
   begin
      return Result;
   end Top_Record;

   ------------
   -- X_Lock --
   ------------

   overriding procedure X_Lock (Item : Kit_Key_Field_Implementation) is
   begin
      Kit_Root_Record_Cache.X_Lock (Marlowe.Database_Index
         (Item.T18_Data.Kit_Root_Record));
      Kit_Key_Field_Cache.X_Lock (Marlowe.Database_Index (Item.M_Index));
   end X_Lock;

end Kit.Db.Kit_Key_Field;
