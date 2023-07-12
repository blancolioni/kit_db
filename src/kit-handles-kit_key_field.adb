with Kit.Protected_Maps;
with Kit.Db.Kit_Key_Field_Hashes;

package body Kit.Handles.Kit_Key_Field is

   type Cached_Handle is
      record
         Top_Record               : Record_Type;
         Kit_Base_Kit_Root_Record : Kit_Root_Record_Reference;
         Kit_Key                  : Kit_Key_Reference;
         Kit_Field                : Kit_Field_Reference;
      end record;
   procedure Load
     (Reference : Kit.Db.Kit_Key_Field_Reference;
      Cached    : in out Cached_Handle);
   package Cached_Handle_Maps is
     new Kit.Protected_Maps
          (Kit.Db.Kit_Key_Field_Reference,
         Cached_Handle,
         Load,
         Kit.Db.Kit_Key_Field_Hashes.Hash,
         Db."=");
   subtype Constant_Reference_Type is
      Cached_Handle_Maps.Constant_Reference_Type;
   Cache : Cached_Handle_Maps.Map;
   type Iterator is
        new Selection_Iterator_Interfaces.Reversible_Iterator with
      record
         Container : access Kit_Key_Field_Selection;
      end record;
   overriding function First (It : Iterator) return Cursor;
   overriding function Last (It : Iterator) return Cursor;
   overriding function Next
     (It       : Iterator;
      Position : Cursor)
   return Cursor;
   overriding function Previous
     (It       : Iterator;
      Position : Cursor)
   return Cursor;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Kit_Key_Field_Selection;
      Position  : Cursor)
   return Kit_Key_Field_Class
   is
   begin
      return Get (Kit.Db.Kit_Key_Field.Element (Position.Db));
   end Constant_Reference;

   ------------
   -- Create --
   ------------

   function Create
     (Kit_Key   : Kit.Handles.Kit_Key.Kit_Key_Class;
      Kit_Field : Kit.Handles.Kit_Field.Kit_Field_Class)
   return Kit_Key_Field_Handle
   is
   begin
      return Get (Kit.Db.Kit_Key_Field.Create (Kit_Key.Reference_Kit_Key,
         Kit_Field.Reference_Kit_Field));
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Kit_Key   : Kit.Handles.Kit_Key.Kit_Key_Class;
      Kit_Field : Kit.Handles.Kit_Field.Kit_Field_Class)
   is
      Handle : constant Kit_Key_Field_Handle := Create (Kit_Key, Kit_Field);
      pragma Unreferenced (Handle);
   begin
      null;
   end Create;

   ----------
   -- Done --
   ----------

   procedure Done (Update : Kit_Key_Field_Update_Handle) is
      Rec : Kit.Db.Kit_Key_Field.Kit_Key_Field_Update :=
         Kit.Db.Kit_Key_Field.Get_Update (Update.Reference);
   begin
      for Item of Update.Updates loop
         case Item.Field is
            when Update_Kit_Key =>
                  Rec.Set_Kit_Key (Item.Kit_Key_Value);
            when Update_Kit_Field =>
                  Rec.Set_Kit_Field (Item.Kit_Field_Value);
         end case;
      end loop;
      Cache.Invalidate (Update.Reference);
   end Done;

   -------------
   -- Element --
   -------------

   function Element (Item : Cursor) return Kit_Key_Field_Class is
   begin
      return Get (Kit.Db.Kit_Key_Field.Element (Item.Db));
   end Element;

   ------------------
   -- Empty_Handle --
   ------------------

   function Empty_Handle return Kit_Key_Field_Handle is
   begin
      return Get (Kit.Db.Null_Kit_Key_Field_Reference);
   end Empty_Handle;

   -----------
   -- First --
   -----------

   overriding function First (It : Iterator) return Cursor is
   begin
      return Position : Cursor do
         Position.Db := Kit.Db.Kit_Key_Field.First (It.Container.Db);
      end return;
   end First;

   ------------------------
   -- First_By_Kit_Field --
   ------------------------

   function First_By_Kit_Field (Kit_Field :
      Kit.Handles.Kit_Field.Kit_Field_Class) return Kit_Key_Field_Handle is
      use Kit.Db;
      Ref : constant Kit_Key_Field_Reference :=
         Kit.Db.Kit_Key_Field.First_By_Kit_Field
         (Kit_Field.Reference_Kit_Field);
      Result : Kit_Key_Field_Handle;
   begin
      if Ref /= Null_Kit_Key_Field_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Kit_Field;

   ----------------------
   -- First_By_Kit_Key --
   ----------------------

   function First_By_Kit_Key (Kit_Key : Kit.Handles.Kit_Key.Kit_Key_Class)
      return Kit_Key_Field_Handle is
      use Kit.Db;
      Ref : constant Kit_Key_Field_Reference :=
         Kit.Db.Kit_Key_Field.First_By_Kit_Key (Kit_Key.Reference_Kit_Key);
      Result : Kit_Key_Field_Handle;
   begin
      if Ref /= Null_Kit_Key_Field_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Kit_Key;

   -------------------------
   -- First_By_Top_Record --
   -------------------------

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Key_Field_Handle is
      use Kit.Db;
      Ref : constant Kit_Key_Field_Reference :=
         Kit.Db.Kit_Key_Field.First_By_Top_Record (Top_Record);
      Result : Kit_Key_Field_Handle;
   begin
      if Ref /= Null_Kit_Key_Field_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Top_Record;

   ---------
   -- Get --
   ---------

   function Get (Reference : Kit.Db.Kit_Key_Field_Reference)
      return Kit_Key_Field_Handle is
   begin
      return Handle : Kit_Key_Field_Handle do
         Handle.Reference := Reference;
      end return;
   end Get;

   ----------------------
   -- Get_By_Key_Field --
   ----------------------

   function Get_By_Key_Field
     (Kit_Key   : Kit.Handles.Kit_Key.Kit_Key_Class;
      Kit_Field : Kit.Handles.Kit_Field.Kit_Field_Class)
   return Kit_Key_Field_Handle
   is
      use Kit.Db;
      Ref : constant Kit_Key_Field_Reference :=
         Kit.Db.Kit_Key_Field.Get_By_Key_Field (Kit_Key.Reference_Kit_Key,
         Kit_Field.Reference_Kit_Field);
      Result : Kit_Key_Field_Handle;
   begin
      if Ref /= Null_Kit_Key_Field_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Get_By_Key_Field;

   --------------------------
   -- Get_Cache_Statistics --
   --------------------------

   procedure Get_Cache_Statistics
     (Size   :    out Natural;
      Hits   :    out Natural;
      Misses :    out Natural)
   is
   begin
      Cache.Get_Statistics
        (Size,
         Hits,
         Misses);
   end Get_Cache_Statistics;

   ------------------------------
   -- Get_From_Kit_Root_Record --
   ------------------------------

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class)
      return Kit_Key_Field_Handle is
      use Kit.Db;
      Ref : constant Kit_Key_Field_Reference :=
         Kit.Db.Kit_Key_Field.Get_From_Kit_Root_Record
         (Kit_Root_Record.Reference_Kit_Root_Record);
      Result : Kit_Key_Field_Handle;
   begin
      if Ref /= Null_Kit_Key_Field_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Get_From_Kit_Root_Record;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element (Handle : Kit_Key_Field_Handle)
      return Boolean is
      use type Kit.Db.Kit_Key_Field_Reference;
   begin
      return Handle.Reference /= Kit.Db.Null_Kit_Key_Field_Reference;
   end Has_Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Item : Cursor) return Boolean is
   begin
      return Kit.Db.Kit_Key_Field.Has_Element (Item.Db);
   end Has_Element;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Kit_Key_Field_Selection) return Boolean is
   begin
      return Kit.Db.Kit_Key_Field.Is_Empty (Container.Db);
   end Is_Empty;

   ------------------
   -- Is_Key_Field --
   ------------------

   function Is_Key_Field
     (Kit_Key   : Kit.Handles.Kit_Key.Kit_Key_Class;
      Kit_Field : Kit.Handles.Kit_Field.Kit_Field_Class)
   return Boolean
   is
      Handle : constant Kit_Key_Field_Handle := Get_By_Key_Field (Kit_Key,
         Kit_Field);
   begin
      return Handle.Has_Element;
   end Is_Key_Field;

   -------------
   -- Iterate --
   -------------

   function Iterate (Container : Kit_Key_Field_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Result : Iterator do
         Result.Container := Container'Unrestricted_Access;
      end return;
   end Iterate;

   ---------------
   -- Kit_Field --
   ---------------

   overriding function Kit_Field (Handle : Kit_Key_Field_Handle)
      return Kit.Handles.Kit_Field.Kit_Field_Class is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Kit.Handles.Kit_Field.Get (Rec.Kit_Field);
   end Kit_Field;

   -------------
   -- Kit_Key --
   -------------

   overriding function Kit_Key (Handle : Kit_Key_Field_Handle)
      return Kit.Handles.Kit_Key.Kit_Key_Class is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Kit.Handles.Kit_Key.Get (Rec.Kit_Key);
   end Kit_Key;

   ----------------------------
   -- Kit_Root_Record_Handle --
   ----------------------------

   function Kit_Root_Record_Handle (Handle : Kit_Key_Field_Handle)
      return Kit.Handles.Kit_Root_Record.Kit_Root_Record_Handle is
      Rec : constant Kit.Db.Kit_Key_Field.Kit_Key_Field_Type :=
         Kit.Db.Kit_Key_Field.Get (Handle.Reference);
   begin
      return Kit.Handles.Kit_Root_Record.Get
         (Rec.Get_Kit_Root_Record_Reference);
   end Kit_Root_Record_Handle;

   ----------
   -- Last --
   ----------

   overriding function Last (It : Iterator) return Cursor is
   begin
      return Position : Cursor do
         Position.Db := Kit.Db.Kit_Key_Field.Last (It.Container.Db);
      end return;
   end Last;

   -----------------------
   -- Last_By_Kit_Field --
   -----------------------

   function Last_By_Kit_Field (Kit_Field :
      Kit.Handles.Kit_Field.Kit_Field_Class) return Kit_Key_Field_Handle is
      use Kit.Db;
      Ref : constant Kit_Key_Field_Reference :=
         Kit.Db.Kit_Key_Field.Last_By_Kit_Field
         (Kit_Field.Reference_Kit_Field);
      Result : Kit_Key_Field_Handle;
   begin
      if Ref /= Null_Kit_Key_Field_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Kit_Field;

   ---------------------
   -- Last_By_Kit_Key --
   ---------------------

   function Last_By_Kit_Key (Kit_Key : Kit.Handles.Kit_Key.Kit_Key_Class)
      return Kit_Key_Field_Handle is
      use Kit.Db;
      Ref : constant Kit_Key_Field_Reference :=
         Kit.Db.Kit_Key_Field.Last_By_Kit_Key (Kit_Key.Reference_Kit_Key);
      Result : Kit_Key_Field_Handle;
   begin
      if Ref /= Null_Kit_Key_Field_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Kit_Key;

   ------------------------
   -- Last_By_Top_Record --
   ------------------------

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Key_Field_Handle is
      use Kit.Db;
      Ref : constant Kit_Key_Field_Reference :=
         Kit.Db.Kit_Key_Field.Last_By_Top_Record (Top_Record);
      Result : Kit_Key_Field_Handle;
   begin
      if Ref /= Null_Kit_Key_Field_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Top_Record;

   ------------
   -- Length --
   ------------

   function Length (Container : Kit_Key_Field_Selection) return Natural is
   begin
      return Kit.Db.Kit_Key_Field.Length (Container.Db);
   end Length;

   ----------
   -- Load --
   ----------

   procedure Load
     (Reference : Kit.Db.Kit_Key_Field_Reference;
      Cached    : in out Cached_Handle)
   is
      Rec : constant Kit.Db.Kit_Key_Field.Kit_Key_Field_Type :=
         Kit.Db.Kit_Key_Field.Get (Reference);
   begin
      Cached.Top_Record := Rec.Top_Record;
      Cached.Kit_Base_Kit_Root_Record := Rec.Get_Kit_Root_Record_Reference;
      Cached.Kit_Key := Rec.Kit_Key;
      Cached.Kit_Field := Rec.Kit_Field;
   end Load;

   ----------
   -- Next --
   ----------

   overriding function Next
     (It       : Iterator;
      Position : Cursor)
   return Cursor
   is
      pragma Unreferenced (It);
   begin
      return Result : Cursor do
         Result.Db := Kit.Db.Kit_Key_Field.Next (Position.Db);
      end return;
   end Next;

   --------------
   -- Previous --
   --------------

   overriding function Previous
     (It       : Iterator;
      Position : Cursor)
   return Cursor
   is
      pragma Unreferenced (It);
   begin
      return Result : Cursor do
         Result.Db := Kit.Db.Kit_Key_Field.Previous (Position.Db);
      end return;
   end Previous;

   ---------------
   -- Reference --
   ---------------

   function Reference (Handle : Kit_Key_Field_Handle)
      return Kit.Db.Kit_Key_Field_Reference is
   begin
      return Handle.Reference;
   end Reference;

   -----------------------------
   -- Reference_Kit_Key_Field --
   -----------------------------

   overriding function Reference_Kit_Key_Field (Handle :
      Kit_Key_Field_Handle) return Kit.Db.Kit_Key_Field_Reference is
   begin
      return Handle.Reference;
   end Reference_Kit_Key_Field;

   -------------------------------
   -- Reference_Kit_Root_Record --
   -------------------------------

   overriding function Reference_Kit_Root_Record (Handle :
      Kit_Key_Field_Handle) return Kit.Db.Kit_Root_Record_Reference is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Kit_Base_Kit_Root_Record;
   end Reference_Kit_Root_Record;

   -----------------------
   -- Scan_By_Key_Field --
   -----------------------

   function Scan_By_Key_Field return Kit_Key_Field_Selection is
      Db_Selection : constant Kit.Db.Kit_Key_Field.Selection :=
         Kit.Db.Kit_Key_Field.Scan_By_Key_Field;
   begin
      return (Db => Db_Selection);
   end Scan_By_Key_Field;

   -----------------------
   -- Scan_By_Kit_Field --
   -----------------------

   function Scan_By_Kit_Field return Kit_Key_Field_Selection is
      Db_Selection : constant Kit.Db.Kit_Key_Field.Selection :=
         Kit.Db.Kit_Key_Field.Scan_By_Kit_Field;
   begin
      return (Db => Db_Selection);
   end Scan_By_Kit_Field;

   ---------------------
   -- Scan_By_Kit_Key --
   ---------------------

   function Scan_By_Kit_Key return Kit_Key_Field_Selection is
      Db_Selection : constant Kit.Db.Kit_Key_Field.Selection :=
         Kit.Db.Kit_Key_Field.Scan_By_Kit_Key;
   begin
      return (Db => Db_Selection);
   end Scan_By_Kit_Key;

   ------------------------
   -- Scan_By_Top_Record --
   ------------------------

   function Scan_By_Top_Record return Kit_Key_Field_Selection is
      Db_Selection : constant Kit.Db.Kit_Key_Field.Selection :=
         Kit.Db.Kit_Key_Field.Scan_By_Top_Record;
   begin
      return (Db => Db_Selection);
   end Scan_By_Top_Record;

   ----------------------------------
   -- Select_Bounded_By_Top_Record --
   ----------------------------------

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Key_Field_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Key_Field.Selection :=
         Kit.Db.Kit_Key_Field.Select_Bounded_By_Top_Record (Start_Top_Record,
         Finish_Top_Record);
   begin
      return (Db => Db_Selection);
   end Select_Bounded_By_Top_Record;

   -------------------------
   -- Select_By_Key_Field --
   -------------------------

   function Select_By_Key_Field
     (Kit_Key   : Kit.Handles.Kit_Key.Kit_Key_Class;
      Kit_Field : Kit.Handles.Kit_Field.Kit_Field_Class)
   return Kit_Key_Field_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Key_Field.Selection :=
         Kit.Db.Kit_Key_Field.Select_By_Key_Field (Kit_Key.Reference_Kit_Key,
         Kit_Field.Reference_Kit_Field);
   begin
      return (Db => Db_Selection);
   end Select_By_Key_Field;

   -------------------------
   -- Select_By_Kit_Field --
   -------------------------

   function Select_By_Kit_Field (Kit_Field :
      Kit.Handles.Kit_Field.Kit_Field_Class)
      return Kit_Key_Field_Selection is
      Db_Selection : constant Kit.Db.Kit_Key_Field.Selection :=
         Kit.Db.Kit_Key_Field.Select_By_Kit_Field
         (Kit_Field.Reference_Kit_Field);
   begin
      return (Db => Db_Selection);
   end Select_By_Kit_Field;

   -----------------------
   -- Select_By_Kit_Key --
   -----------------------

   function Select_By_Kit_Key (Kit_Key : Kit.Handles.Kit_Key.Kit_Key_Class)
      return Kit_Key_Field_Selection is
      Db_Selection : constant Kit.Db.Kit_Key_Field.Selection :=
         Kit.Db.Kit_Key_Field.Select_By_Kit_Key (Kit_Key.Reference_Kit_Key);
   begin
      return (Db => Db_Selection);
   end Select_By_Kit_Key;

   --------------------------
   -- Select_By_Top_Record --
   --------------------------

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Key_Field_Selection is
      Db_Selection : constant Kit.Db.Kit_Key_Field.Selection :=
         Kit.Db.Kit_Key_Field.Select_By_Top_Record (Top_Record);
   begin
      return (Db => Db_Selection);
   end Select_By_Top_Record;

   -------------------
   -- Set_Kit_Field --
   -------------------

   function Set_Kit_Field
     (Update : Kit_Key_Field_Update_Handle;
      Value  : Kit.Handles.Kit_Field.Kit_Field_Class)
   return Kit_Key_Field_Update_Handle
   is
      Change : Kit_Key_Field_Update_Value (Update_Kit_Field);
   begin
      Change.Kit_Field_Value := Value.Reference_Kit_Field;
      return Result : Kit_Key_Field_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Kit_Field;

   -----------------
   -- Set_Kit_Key --
   -----------------

   function Set_Kit_Key
     (Update : Kit_Key_Field_Update_Handle;
      Value  : Kit.Handles.Kit_Key.Kit_Key_Class)
   return Kit_Key_Field_Update_Handle
   is
      Change : Kit_Key_Field_Update_Value (Update_Kit_Key);
   begin
      Change.Kit_Key_Value := Value.Reference_Kit_Key;
      return Result : Kit_Key_Field_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Kit_Key;

   -----------------------------
   -- To_Kit_Key_Field_Handle --
   -----------------------------

   function To_Kit_Key_Field_Handle (Class : Kit_Key_Field_Class)
      return Kit_Key_Field_Handle is
   begin
      return Get (Class.Reference_Kit_Key_Field);
   end To_Kit_Key_Field_Handle;

   ----------------
   -- Top_Record --
   ----------------

   overriding function Top_Record (Handle : Kit_Key_Field_Handle)
      return Kit.Db.Record_Type is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Top_Record;
   end Top_Record;

   ------------
   -- Update --
   ------------

   function Update (Handle : Kit_Key_Field_Handle)
      return Kit_Key_Field_Update_Handle'Class is
   begin
      return Update_Kit_Key_Field (Handle.Reference);
   end Update;

   --------------------------
   -- Update_Kit_Key_Field --
   --------------------------

   function Update_Kit_Key_Field (Target : Kit.Db.Kit_Key_Field_Reference)
      return Kit_Key_Field_Update_Handle is
   begin
      return Update : Kit_Key_Field_Update_Handle do
         Update.Reference := Target;
      end return;
   end Update_Kit_Key_Field;

   --------------------------
   -- Update_Kit_Key_Field --
   --------------------------

   overriding function Update_Kit_Key_Field (Handle : Kit_Key_Field_Handle)
      return Kit_Key_Field_Update_Handle'Class is
   begin
      return Update_Kit_Key_Field (Handle.Reference);
   end Update_Kit_Key_Field;

end Kit.Handles.Kit_Key_Field;
