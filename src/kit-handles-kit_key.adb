with Kit.Protected_Maps;
with Kit.Db.Kit_Key_Hashes;

package body Kit.Handles.Kit_Key is

   type Cached_Handle is
      record
         Top_Record               : Record_Type;
         Kit_Base_Kit_Root_Record : Kit_Root_Record_Reference;
         Name                     : Kit.Strings.String_Type (64);
         Kit_Record               : Kit_Record_Reference;
         Is_Unique                : Boolean;
         Length                   : Integer;
      end record;
   procedure Load
     (Reference : Kit.Db.Kit_Key_Reference;
      Cached    : in out Cached_Handle);
   package Cached_Handle_Maps is
     new Kit.Protected_Maps
          (Kit.Db.Kit_Key_Reference,
         Cached_Handle,
         Load,
         Kit.Db.Kit_Key_Hashes.Hash,
         Db."=");
   subtype Constant_Reference_Type is
      Cached_Handle_Maps.Constant_Reference_Type;
   Cache : Cached_Handle_Maps.Map;
   type Iterator is
        new Selection_Iterator_Interfaces.Reversible_Iterator with
      record
         Container : access Kit_Key_Selection;
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
     (Container : aliased Kit_Key_Selection;
      Position  : Cursor)
   return Kit_Key_Class
   is
   begin
      return Get (Kit.Db.Kit_Key.Element (Position.Db));
   end Constant_Reference;

   ------------
   -- Create --
   ------------

   procedure Create
     (Name       : String;
      Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Is_Unique  : Boolean;
      Length     : Integer)
   is
      Handle : constant Kit_Key_Handle := Create (Name, Kit_Record,
         Is_Unique, Length);
      pragma Unreferenced (Handle);
   begin
      null;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Name       : String;
      Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Is_Unique  : Boolean;
      Length     : Integer)
   return Kit_Key_Handle
   is
   begin
      return Get (Kit.Db.Kit_Key.Create (Name,
         Kit_Record.Reference_Kit_Record, Is_Unique, Length));
   end Create;

   ----------
   -- Done --
   ----------

   procedure Done (Update : Kit_Key_Update_Handle) is
      Rec : Kit.Db.Kit_Key.Kit_Key_Update := Kit.Db.Kit_Key.Get_Update
         (Update.Reference);
   begin
      for Item of Update.Updates loop
         case Item.Field is
            when Update_Name =>
                  Rec.Set_Name (Item.Name_Value.Text
                     (1 .. Item.Name_Value.Length));
            when Update_Kit_Record =>
                  Rec.Set_Kit_Record (Item.Kit_Record_Value);
            when Update_Is_Unique =>
                  Rec.Set_Is_Unique (Item.Is_Unique_Value);
            when Update_Length =>
                  Rec.Set_Length (Item.Length_Value);
         end case;
      end loop;
      Cache.Invalidate (Update.Reference);
   end Done;

   -------------
   -- Element --
   -------------

   function Element (Item : Cursor) return Kit_Key_Class is
   begin
      return Get (Kit.Db.Kit_Key.Element (Item.Db));
   end Element;

   ------------------
   -- Empty_Handle --
   ------------------

   function Empty_Handle return Kit_Key_Handle is
   begin
      return Get (Kit.Db.Null_Kit_Key_Reference);
   end Empty_Handle;

   -----------
   -- First --
   -----------

   overriding function First (It : Iterator) return Cursor is
   begin
      return Position : Cursor do
         Position.Db := Kit.Db.Kit_Key.First (It.Container.Db);
      end return;
   end First;

   -------------------------
   -- First_By_Kit_Record --
   -------------------------

   function First_By_Kit_Record (Kit_Record :
      Kit.Handles.Kit_Record.Kit_Record_Class) return Kit_Key_Handle is
      use Kit.Db;
      Ref : constant Kit_Key_Reference := Kit.Db.Kit_Key.First_By_Kit_Record
         (Kit_Record.Reference_Kit_Record);
      Result : Kit_Key_Handle;
   begin
      if Ref /= Null_Kit_Key_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Kit_Record;

   -------------------------
   -- First_By_Top_Record --
   -------------------------

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Key_Handle is
      use Kit.Db;
      Ref : constant Kit_Key_Reference := Kit.Db.Kit_Key.First_By_Top_Record
         (Top_Record);
      Result : Kit_Key_Handle;
   begin
      if Ref /= Null_Kit_Key_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Top_Record;

   ---------
   -- Get --
   ---------

   function Get (Reference : Kit.Db.Kit_Key_Reference)
      return Kit_Key_Handle is
   begin
      return Handle : Kit_Key_Handle do
         Handle.Reference := Reference;
      end return;
   end Get;

   -----------------------
   -- Get_By_Record_Key --
   -----------------------

   function Get_By_Record_Key
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Name       : String)
   return Kit_Key_Handle
   is
      use Kit.Db;
      Ref : constant Kit_Key_Reference := Kit.Db.Kit_Key.Get_By_Record_Key
         (Kit_Record.Reference_Kit_Record, Name);
      Result : Kit_Key_Handle;
   begin
      if Ref /= Null_Kit_Key_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Get_By_Record_Key;

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
      return Kit_Key_Handle is
      use Kit.Db;
      Ref : constant Kit_Key_Reference :=
         Kit.Db.Kit_Key.Get_From_Kit_Root_Record
         (Kit_Root_Record.Reference_Kit_Root_Record);
      Result : Kit_Key_Handle;
   begin
      if Ref /= Null_Kit_Key_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Get_From_Kit_Root_Record;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element (Handle : Kit_Key_Handle)
      return Boolean is
      use type Kit.Db.Kit_Key_Reference;
   begin
      return Handle.Reference /= Kit.Db.Null_Kit_Key_Reference;
   end Has_Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Item : Cursor) return Boolean is
   begin
      return Kit.Db.Kit_Key.Has_Element (Item.Db);
   end Has_Element;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Kit_Key_Selection) return Boolean is
   begin
      return Kit.Db.Kit_Key.Is_Empty (Container.Db);
   end Is_Empty;

   -------------------
   -- Is_Record_Key --
   -------------------

   function Is_Record_Key
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Name       : String)
   return Boolean
   is
      Handle : constant Kit_Key_Handle := Get_By_Record_Key (Kit_Record,
         Name);
   begin
      return Handle.Has_Element;
   end Is_Record_Key;

   ---------------
   -- Is_Unique --
   ---------------

   overriding function Is_Unique (Handle : Kit_Key_Handle) return Boolean is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Is_Unique;
   end Is_Unique;

   -------------
   -- Iterate --
   -------------

   function Iterate (Container : Kit_Key_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Result : Iterator do
         Result.Container := Container'Unrestricted_Access;
      end return;
   end Iterate;

   ----------------
   -- Kit_Record --
   ----------------

   overriding function Kit_Record (Handle : Kit_Key_Handle)
      return Kit.Handles.Kit_Record.Kit_Record_Class is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Kit.Handles.Kit_Record.Get (Rec.Kit_Record);
   end Kit_Record;

   ----------------------------
   -- Kit_Root_Record_Handle --
   ----------------------------

   function Kit_Root_Record_Handle (Handle : Kit_Key_Handle)
      return Kit.Handles.Kit_Root_Record.Kit_Root_Record_Handle is
      Rec : constant Kit.Db.Kit_Key.Kit_Key_Type := Kit.Db.Kit_Key.Get
         (Handle.Reference);
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
         Position.Db := Kit.Db.Kit_Key.Last (It.Container.Db);
      end return;
   end Last;

   ------------------------
   -- Last_By_Kit_Record --
   ------------------------

   function Last_By_Kit_Record (Kit_Record :
      Kit.Handles.Kit_Record.Kit_Record_Class) return Kit_Key_Handle is
      use Kit.Db;
      Ref : constant Kit_Key_Reference := Kit.Db.Kit_Key.Last_By_Kit_Record
         (Kit_Record.Reference_Kit_Record);
      Result : Kit_Key_Handle;
   begin
      if Ref /= Null_Kit_Key_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Kit_Record;

   ------------------------
   -- Last_By_Top_Record --
   ------------------------

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Key_Handle is
      use Kit.Db;
      Ref : constant Kit_Key_Reference := Kit.Db.Kit_Key.Last_By_Top_Record
         (Top_Record);
      Result : Kit_Key_Handle;
   begin
      if Ref /= Null_Kit_Key_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Top_Record;

   ------------
   -- Length --
   ------------

   overriding function Length (Handle : Kit_Key_Handle) return Integer is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Length;
   end Length;

   ------------
   -- Length --
   ------------

   function Length (Container : Kit_Key_Selection) return Natural is
   begin
      return Kit.Db.Kit_Key.Length (Container.Db);
   end Length;

   ----------
   -- Load --
   ----------

   procedure Load
     (Reference : Kit.Db.Kit_Key_Reference;
      Cached    : in out Cached_Handle)
   is
      Rec : constant Kit.Db.Kit_Key.Kit_Key_Type := Kit.Db.Kit_Key.Get
         (Reference);
   begin
      Cached.Top_Record := Rec.Top_Record;
      Cached.Kit_Base_Kit_Root_Record := Rec.Get_Kit_Root_Record_Reference;
      Cached.Name.Length := Rec.Name'Length;
      Cached.Name.Text (1 .. Rec.Name'Length) := Rec.Name;
      Cached.Kit_Record := Rec.Kit_Record;
      Cached.Is_Unique := Rec.Is_Unique;
      Cached.Length := Rec.Length;
   end Load;

   ----------
   -- Name --
   ----------

   overriding function Name (Handle : Kit_Key_Handle) return String is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Name.Text (1 .. Rec.Name.Length);
   end Name;

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
         Result.Db := Kit.Db.Kit_Key.Next (Position.Db);
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
         Result.Db := Kit.Db.Kit_Key.Previous (Position.Db);
      end return;
   end Previous;

   ---------------
   -- Reference --
   ---------------

   function Reference (Handle : Kit_Key_Handle)
      return Kit.Db.Kit_Key_Reference is
   begin
      return Handle.Reference;
   end Reference;

   -----------------------
   -- Reference_Kit_Key --
   -----------------------

   overriding function Reference_Kit_Key (Handle : Kit_Key_Handle)
      return Kit.Db.Kit_Key_Reference is
   begin
      return Handle.Reference;
   end Reference_Kit_Key;

   -------------------------------
   -- Reference_Kit_Root_Record --
   -------------------------------

   overriding function Reference_Kit_Root_Record (Handle : Kit_Key_Handle)
      return Kit.Db.Kit_Root_Record_Reference is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Kit_Base_Kit_Root_Record;
   end Reference_Kit_Root_Record;

   ------------------------
   -- Scan_By_Kit_Record --
   ------------------------

   function Scan_By_Kit_Record return Kit_Key_Selection is
      Db_Selection : constant Kit.Db.Kit_Key.Selection :=
         Kit.Db.Kit_Key.Scan_By_Kit_Record;
   begin
      return (Db => Db_Selection);
   end Scan_By_Kit_Record;

   ------------------------
   -- Scan_By_Record_Key --
   ------------------------

   function Scan_By_Record_Key return Kit_Key_Selection is
      Db_Selection : constant Kit.Db.Kit_Key.Selection :=
         Kit.Db.Kit_Key.Scan_By_Record_Key;
   begin
      return (Db => Db_Selection);
   end Scan_By_Record_Key;

   ------------------------
   -- Scan_By_Top_Record --
   ------------------------

   function Scan_By_Top_Record return Kit_Key_Selection is
      Db_Selection : constant Kit.Db.Kit_Key.Selection :=
         Kit.Db.Kit_Key.Scan_By_Top_Record;
   begin
      return (Db => Db_Selection);
   end Scan_By_Top_Record;

   ----------------------------------
   -- Select_Bounded_By_Top_Record --
   ----------------------------------

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Key_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Key.Selection :=
         Kit.Db.Kit_Key.Select_Bounded_By_Top_Record (Start_Top_Record,
         Finish_Top_Record);
   begin
      return (Db => Db_Selection);
   end Select_Bounded_By_Top_Record;

   --------------------------
   -- Select_By_Kit_Record --
   --------------------------

   function Select_By_Kit_Record (Kit_Record :
      Kit.Handles.Kit_Record.Kit_Record_Class) return Kit_Key_Selection is
      Db_Selection : constant Kit.Db.Kit_Key.Selection :=
         Kit.Db.Kit_Key.Select_By_Kit_Record
         (Kit_Record.Reference_Kit_Record);
   begin
      return (Db => Db_Selection);
   end Select_By_Kit_Record;

   --------------------------
   -- Select_By_Record_Key --
   --------------------------

   function Select_By_Record_Key
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Name       : String)
   return Kit_Key_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Key.Selection :=
         Kit.Db.Kit_Key.Select_By_Record_Key
         (Kit_Record.Reference_Kit_Record, Name);
   begin
      return (Db => Db_Selection);
   end Select_By_Record_Key;

   --------------------------
   -- Select_By_Top_Record --
   --------------------------

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Key_Selection is
      Db_Selection : constant Kit.Db.Kit_Key.Selection :=
         Kit.Db.Kit_Key.Select_By_Top_Record (Top_Record);
   begin
      return (Db => Db_Selection);
   end Select_By_Top_Record;

   ---------------------------------------
   -- Select_Record_Key_Bounded_By_Name --
   ---------------------------------------

   function Select_Record_Key_Bounded_By_Name
     (Kit_Record  : Kit.Handles.Kit_Record.Kit_Record_Class;
      Start_Name  : String;
      Finish_Name : String)
   return Kit_Key_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Key.Selection :=
         Kit.Db.Kit_Key.Select_Record_Key_Bounded_By_Name
         (Kit_Record.Reference_Kit_Record, Start_Name, Finish_Name);
   begin
      return (Db => Db_Selection);
   end Select_Record_Key_Bounded_By_Name;

   -------------------
   -- Set_Is_Unique --
   -------------------

   function Set_Is_Unique
     (Update : Kit_Key_Update_Handle;
      Value  : Boolean)
   return Kit_Key_Update_Handle
   is
      Change : Kit_Key_Update_Value (Update_Is_Unique);
   begin
      Change.Is_Unique_Value := Value;
      return Result : Kit_Key_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Is_Unique;

   --------------------
   -- Set_Kit_Record --
   --------------------

   function Set_Kit_Record
     (Update : Kit_Key_Update_Handle;
      Value  : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Key_Update_Handle
   is
      Change : Kit_Key_Update_Value (Update_Kit_Record);
   begin
      Change.Kit_Record_Value := Value.Reference_Kit_Record;
      return Result : Kit_Key_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Kit_Record;

   ----------------
   -- Set_Length --
   ----------------

   function Set_Length
     (Update : Kit_Key_Update_Handle;
      Value  : Integer)
   return Kit_Key_Update_Handle
   is
      Change : Kit_Key_Update_Value (Update_Length);
   begin
      Change.Length_Value := Value;
      return Result : Kit_Key_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Length;

   --------------
   -- Set_Name --
   --------------

   function Set_Name
     (Update : Kit_Key_Update_Handle;
      Value  : String)
   return Kit_Key_Update_Handle
   is
      Change : Kit_Key_Update_Value (Update_Name);
   begin
      Change.Name_Value.Length := Value'Length;
      Change.Name_Value.Text (1 .. Value'Length) := Value;
      return Result : Kit_Key_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Name;

   -----------------------
   -- To_Kit_Key_Handle --
   -----------------------

   function To_Kit_Key_Handle (Class : Kit_Key_Class)
      return Kit_Key_Handle is
   begin
      return Get (Class.Reference_Kit_Key);
   end To_Kit_Key_Handle;

   ----------------
   -- Top_Record --
   ----------------

   overriding function Top_Record (Handle : Kit_Key_Handle)
      return Kit.Db.Record_Type is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Top_Record;
   end Top_Record;

   ------------
   -- Update --
   ------------

   function Update (Handle : Kit_Key_Handle)
      return Kit_Key_Update_Handle'Class is
   begin
      return Update_Kit_Key (Handle.Reference);
   end Update;

   --------------------
   -- Update_Kit_Key --
   --------------------

   overriding function Update_Kit_Key (Handle : Kit_Key_Handle)
      return Kit_Key_Update_Handle'Class is
   begin
      return Update_Kit_Key (Handle.Reference);
   end Update_Kit_Key;

   --------------------
   -- Update_Kit_Key --
   --------------------

   function Update_Kit_Key (Target : Kit.Db.Kit_Key_Reference)
      return Kit_Key_Update_Handle is
   begin
      return Update : Kit_Key_Update_Handle do
         Update.Reference := Target;
      end return;
   end Update_Kit_Key;

end Kit.Handles.Kit_Key;
