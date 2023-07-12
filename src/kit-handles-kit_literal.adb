with Kit.Protected_Maps;
with Kit.Db.Kit_Literal_Hashes;

package body Kit.Handles.Kit_Literal is

   type Cached_Handle is
      record
         Top_Record               : Record_Type;
         Kit_Base_Kit_Root_Record : Kit_Root_Record_Reference;
         Name                     : Kit.Strings.String_Type (64);
         Kit_Enumeration          : Kit_Enumeration_Reference;
         Value                    : Integer;
      end record;
   procedure Load
     (Reference : Kit.Db.Kit_Literal_Reference;
      Cached    : in out Cached_Handle);
   package Cached_Handle_Maps is
     new Kit.Protected_Maps
          (Kit.Db.Kit_Literal_Reference,
         Cached_Handle,
         Load,
         Kit.Db.Kit_Literal_Hashes.Hash,
         Db."=");
   subtype Constant_Reference_Type is
      Cached_Handle_Maps.Constant_Reference_Type;
   Cache : Cached_Handle_Maps.Map;
   type Iterator is
        new Selection_Iterator_Interfaces.Reversible_Iterator with
      record
         Container : access Kit_Literal_Selection;
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
     (Container : aliased Kit_Literal_Selection;
      Position  : Cursor)
   return Kit_Literal_Class
   is
   begin
      return Get (Kit.Db.Kit_Literal.Element (Position.Db));
   end Constant_Reference;

   ------------
   -- Create --
   ------------

   procedure Create
     (Name            : String;
      Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Value           : Integer)
   is
      Handle : constant Kit_Literal_Handle := Create (Name, Kit_Enumeration,
         Value);
      pragma Unreferenced (Handle);
   begin
      null;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Name            : String;
      Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Value           : Integer)
   return Kit_Literal_Handle
   is
   begin
      return Get (Kit.Db.Kit_Literal.Create (Name,
         Kit_Enumeration.Reference_Kit_Enumeration, Value));
   end Create;

   ----------
   -- Done --
   ----------

   procedure Done (Update : Kit_Literal_Update_Handle) is
      Rec : Kit.Db.Kit_Literal.Kit_Literal_Update :=
         Kit.Db.Kit_Literal.Get_Update (Update.Reference);
   begin
      for Item of Update.Updates loop
         case Item.Field is
            when Update_Name =>
                  Rec.Set_Name (Item.Name_Value.Text
                     (1 .. Item.Name_Value.Length));
            when Update_Kit_Enumeration =>
                  Rec.Set_Kit_Enumeration (Item.Kit_Enumeration_Value);
            when Update_Value =>
                  Rec.Set_Value (Item.Value_Value);
         end case;
      end loop;
      Cache.Invalidate (Update.Reference);
   end Done;

   -------------
   -- Element --
   -------------

   function Element (Item : Cursor) return Kit_Literal_Class is
   begin
      return Get (Kit.Db.Kit_Literal.Element (Item.Db));
   end Element;

   ------------------
   -- Empty_Handle --
   ------------------

   function Empty_Handle return Kit_Literal_Handle is
   begin
      return Get (Kit.Db.Null_Kit_Literal_Reference);
   end Empty_Handle;

   -----------
   -- First --
   -----------

   overriding function First (It : Iterator) return Cursor is
   begin
      return Position : Cursor do
         Position.Db := Kit.Db.Kit_Literal.First (It.Container.Db);
      end return;
   end First;

   ------------------------------
   -- First_By_Kit_Enumeration --
   ------------------------------

   function First_By_Kit_Enumeration (Kit_Enumeration :
      Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class)
      return Kit_Literal_Handle is
      use Kit.Db;
      Ref : constant Kit_Literal_Reference :=
         Kit.Db.Kit_Literal.First_By_Kit_Enumeration
         (Kit_Enumeration.Reference_Kit_Enumeration);
      Result : Kit_Literal_Handle;
   begin
      if Ref /= Null_Kit_Literal_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Kit_Enumeration;

   -------------------------
   -- First_By_Top_Record --
   -------------------------

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Literal_Handle is
      use Kit.Db;
      Ref : constant Kit_Literal_Reference :=
         Kit.Db.Kit_Literal.First_By_Top_Record (Top_Record);
      Result : Kit_Literal_Handle;
   begin
      if Ref /= Null_Kit_Literal_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Top_Record;

   --------------------
   -- First_By_Value --
   --------------------

   function First_By_Value (Value : Integer) return Kit_Literal_Handle is
      use Kit.Db;
      Ref : constant Kit_Literal_Reference :=
         Kit.Db.Kit_Literal.First_By_Value (Value);
      Result : Kit_Literal_Handle;
   begin
      if Ref /= Null_Kit_Literal_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Value;

   ---------
   -- Get --
   ---------

   function Get (Reference : Kit.Db.Kit_Literal_Reference)
      return Kit_Literal_Handle is
   begin
      return Handle : Kit_Literal_Handle do
         Handle.Reference := Reference;
      end return;
   end Get;

   ----------------------
   -- Get_By_Enum_Name --
   ----------------------

   function Get_By_Enum_Name
     (Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Name            : String)
   return Kit_Literal_Handle
   is
      use Kit.Db;
      Ref : constant Kit_Literal_Reference :=
         Kit.Db.Kit_Literal.Get_By_Enum_Name
         (Kit_Enumeration.Reference_Kit_Enumeration, Name);
      Result : Kit_Literal_Handle;
   begin
      if Ref /= Null_Kit_Literal_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Get_By_Enum_Name;

   -----------------------
   -- Get_By_Enum_Value --
   -----------------------

   function Get_By_Enum_Value
     (Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Value           : Integer)
   return Kit_Literal_Handle
   is
      use Kit.Db;
      Ref : constant Kit_Literal_Reference :=
         Kit.Db.Kit_Literal.Get_By_Enum_Value
         (Kit_Enumeration.Reference_Kit_Enumeration, Value);
      Result : Kit_Literal_Handle;
   begin
      if Ref /= Null_Kit_Literal_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Get_By_Enum_Value;

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
      return Kit_Literal_Handle is
      use Kit.Db;
      Ref : constant Kit_Literal_Reference :=
         Kit.Db.Kit_Literal.Get_From_Kit_Root_Record
         (Kit_Root_Record.Reference_Kit_Root_Record);
      Result : Kit_Literal_Handle;
   begin
      if Ref /= Null_Kit_Literal_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Get_From_Kit_Root_Record;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Item : Cursor) return Boolean is
   begin
      return Kit.Db.Kit_Literal.Has_Element (Item.Db);
   end Has_Element;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element (Handle : Kit_Literal_Handle)
      return Boolean is
      use type Kit.Db.Kit_Literal_Reference;
   begin
      return Handle.Reference /= Kit.Db.Null_Kit_Literal_Reference;
   end Has_Element;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Kit_Literal_Selection) return Boolean is
   begin
      return Kit.Db.Kit_Literal.Is_Empty (Container.Db);
   end Is_Empty;

   ------------------
   -- Is_Enum_Name --
   ------------------

   function Is_Enum_Name
     (Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Name            : String)
   return Boolean
   is
      Handle : constant Kit_Literal_Handle := Get_By_Enum_Name
         (Kit_Enumeration, Name);
   begin
      return Handle.Has_Element;
   end Is_Enum_Name;

   -------------------
   -- Is_Enum_Value --
   -------------------

   function Is_Enum_Value
     (Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Value           : Integer)
   return Boolean
   is
      Handle : constant Kit_Literal_Handle := Get_By_Enum_Value
         (Kit_Enumeration, Value);
   begin
      return Handle.Has_Element;
   end Is_Enum_Value;

   -------------
   -- Iterate --
   -------------

   function Iterate (Container : Kit_Literal_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Result : Iterator do
         Result.Container := Container'Unrestricted_Access;
      end return;
   end Iterate;

   ---------------------
   -- Kit_Enumeration --
   ---------------------

   overriding function Kit_Enumeration (Handle : Kit_Literal_Handle)
      return Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Kit.Handles.Kit_Enumeration.Get (Rec.Kit_Enumeration);
   end Kit_Enumeration;

   ----------------------------
   -- Kit_Root_Record_Handle --
   ----------------------------

   function Kit_Root_Record_Handle (Handle : Kit_Literal_Handle)
      return Kit.Handles.Kit_Root_Record.Kit_Root_Record_Handle is
      Rec : constant Kit.Db.Kit_Literal.Kit_Literal_Type :=
         Kit.Db.Kit_Literal.Get (Handle.Reference);
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
         Position.Db := Kit.Db.Kit_Literal.Last (It.Container.Db);
      end return;
   end Last;

   -----------------------------
   -- Last_By_Kit_Enumeration --
   -----------------------------

   function Last_By_Kit_Enumeration (Kit_Enumeration :
      Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class)
      return Kit_Literal_Handle is
      use Kit.Db;
      Ref : constant Kit_Literal_Reference :=
         Kit.Db.Kit_Literal.Last_By_Kit_Enumeration
         (Kit_Enumeration.Reference_Kit_Enumeration);
      Result : Kit_Literal_Handle;
   begin
      if Ref /= Null_Kit_Literal_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Kit_Enumeration;

   ------------------------
   -- Last_By_Top_Record --
   ------------------------

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Literal_Handle is
      use Kit.Db;
      Ref : constant Kit_Literal_Reference :=
         Kit.Db.Kit_Literal.Last_By_Top_Record (Top_Record);
      Result : Kit_Literal_Handle;
   begin
      if Ref /= Null_Kit_Literal_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Top_Record;

   -------------------
   -- Last_By_Value --
   -------------------

   function Last_By_Value (Value : Integer) return Kit_Literal_Handle is
      use Kit.Db;
      Ref : constant Kit_Literal_Reference :=
         Kit.Db.Kit_Literal.Last_By_Value (Value);
      Result : Kit_Literal_Handle;
   begin
      if Ref /= Null_Kit_Literal_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Value;

   ------------
   -- Length --
   ------------

   function Length (Container : Kit_Literal_Selection) return Natural is
   begin
      return Kit.Db.Kit_Literal.Length (Container.Db);
   end Length;

   ----------
   -- Load --
   ----------

   procedure Load
     (Reference : Kit.Db.Kit_Literal_Reference;
      Cached    : in out Cached_Handle)
   is
      Rec : constant Kit.Db.Kit_Literal.Kit_Literal_Type :=
         Kit.Db.Kit_Literal.Get (Reference);
   begin
      Cached.Top_Record := Rec.Top_Record;
      Cached.Kit_Base_Kit_Root_Record := Rec.Get_Kit_Root_Record_Reference;
      Cached.Name.Length := Rec.Name'Length;
      Cached.Name.Text (1 .. Rec.Name'Length) := Rec.Name;
      Cached.Kit_Enumeration := Rec.Kit_Enumeration;
      Cached.Value := Rec.Value;
   end Load;

   ----------
   -- Name --
   ----------

   overriding function Name (Handle : Kit_Literal_Handle) return String is
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
         Result.Db := Kit.Db.Kit_Literal.Next (Position.Db);
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
         Result.Db := Kit.Db.Kit_Literal.Previous (Position.Db);
      end return;
   end Previous;

   ---------------
   -- Reference --
   ---------------

   function Reference (Handle : Kit_Literal_Handle)
      return Kit.Db.Kit_Literal_Reference is
   begin
      return Handle.Reference;
   end Reference;

   ---------------------------
   -- Reference_Kit_Literal --
   ---------------------------

   overriding function Reference_Kit_Literal (Handle : Kit_Literal_Handle)
      return Kit.Db.Kit_Literal_Reference is
   begin
      return Handle.Reference;
   end Reference_Kit_Literal;

   -------------------------------
   -- Reference_Kit_Root_Record --
   -------------------------------

   overriding function Reference_Kit_Root_Record (Handle :
      Kit_Literal_Handle) return Kit.Db.Kit_Root_Record_Reference is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Kit_Base_Kit_Root_Record;
   end Reference_Kit_Root_Record;

   -----------------------
   -- Scan_By_Enum_Name --
   -----------------------

   function Scan_By_Enum_Name return Kit_Literal_Selection is
      Db_Selection : constant Kit.Db.Kit_Literal.Selection :=
         Kit.Db.Kit_Literal.Scan_By_Enum_Name;
   begin
      return (Db => Db_Selection);
   end Scan_By_Enum_Name;

   ------------------------
   -- Scan_By_Enum_Value --
   ------------------------

   function Scan_By_Enum_Value return Kit_Literal_Selection is
      Db_Selection : constant Kit.Db.Kit_Literal.Selection :=
         Kit.Db.Kit_Literal.Scan_By_Enum_Value;
   begin
      return (Db => Db_Selection);
   end Scan_By_Enum_Value;

   -----------------------------
   -- Scan_By_Kit_Enumeration --
   -----------------------------

   function Scan_By_Kit_Enumeration return Kit_Literal_Selection is
      Db_Selection : constant Kit.Db.Kit_Literal.Selection :=
         Kit.Db.Kit_Literal.Scan_By_Kit_Enumeration;
   begin
      return (Db => Db_Selection);
   end Scan_By_Kit_Enumeration;

   ------------------------
   -- Scan_By_Top_Record --
   ------------------------

   function Scan_By_Top_Record return Kit_Literal_Selection is
      Db_Selection : constant Kit.Db.Kit_Literal.Selection :=
         Kit.Db.Kit_Literal.Scan_By_Top_Record;
   begin
      return (Db => Db_Selection);
   end Scan_By_Top_Record;

   -------------------
   -- Scan_By_Value --
   -------------------

   function Scan_By_Value return Kit_Literal_Selection is
      Db_Selection : constant Kit.Db.Kit_Literal.Selection :=
         Kit.Db.Kit_Literal.Scan_By_Value;
   begin
      return (Db => Db_Selection);
   end Scan_By_Value;

   ----------------------------------
   -- Select_Bounded_By_Top_Record --
   ----------------------------------

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Literal_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Literal.Selection :=
         Kit.Db.Kit_Literal.Select_Bounded_By_Top_Record (Start_Top_Record,
         Finish_Top_Record);
   begin
      return (Db => Db_Selection);
   end Select_Bounded_By_Top_Record;

   -----------------------------
   -- Select_Bounded_By_Value --
   -----------------------------

   function Select_Bounded_By_Value
     (Start_Value  : Integer;
      Finish_Value : Integer)
   return Kit_Literal_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Literal.Selection :=
         Kit.Db.Kit_Literal.Select_Bounded_By_Value (Start_Value,
         Finish_Value);
   begin
      return (Db => Db_Selection);
   end Select_Bounded_By_Value;

   -------------------------
   -- Select_By_Enum_Name --
   -------------------------

   function Select_By_Enum_Name
     (Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Name            : String)
   return Kit_Literal_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Literal.Selection :=
         Kit.Db.Kit_Literal.Select_By_Enum_Name
         (Kit_Enumeration.Reference_Kit_Enumeration, Name);
   begin
      return (Db => Db_Selection);
   end Select_By_Enum_Name;

   --------------------------
   -- Select_By_Enum_Value --
   --------------------------

   function Select_By_Enum_Value
     (Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Value           : Integer)
   return Kit_Literal_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Literal.Selection :=
         Kit.Db.Kit_Literal.Select_By_Enum_Value
         (Kit_Enumeration.Reference_Kit_Enumeration, Value);
   begin
      return (Db => Db_Selection);
   end Select_By_Enum_Value;

   -------------------------------
   -- Select_By_Kit_Enumeration --
   -------------------------------

   function Select_By_Kit_Enumeration (Kit_Enumeration :
      Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class)
      return Kit_Literal_Selection is
      Db_Selection : constant Kit.Db.Kit_Literal.Selection :=
         Kit.Db.Kit_Literal.Select_By_Kit_Enumeration
         (Kit_Enumeration.Reference_Kit_Enumeration);
   begin
      return (Db => Db_Selection);
   end Select_By_Kit_Enumeration;

   --------------------------
   -- Select_By_Top_Record --
   --------------------------

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Literal_Selection is
      Db_Selection : constant Kit.Db.Kit_Literal.Selection :=
         Kit.Db.Kit_Literal.Select_By_Top_Record (Top_Record);
   begin
      return (Db => Db_Selection);
   end Select_By_Top_Record;

   ---------------------
   -- Select_By_Value --
   ---------------------

   function Select_By_Value (Value : Integer) return Kit_Literal_Selection is
      Db_Selection : constant Kit.Db.Kit_Literal.Selection :=
         Kit.Db.Kit_Literal.Select_By_Value (Value);
   begin
      return (Db => Db_Selection);
   end Select_By_Value;

   --------------------------------------
   -- Select_Enum_Name_Bounded_By_Name --
   --------------------------------------

   function Select_Enum_Name_Bounded_By_Name
     (Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Start_Name      : String;
      Finish_Name     : String)
   return Kit_Literal_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Literal.Selection :=
         Kit.Db.Kit_Literal.Select_Enum_Name_Bounded_By_Name
         (Kit_Enumeration.Reference_Kit_Enumeration, Start_Name,
         Finish_Name);
   begin
      return (Db => Db_Selection);
   end Select_Enum_Name_Bounded_By_Name;

   ----------------------------------------
   -- Select_Enum_Value_Bounded_By_Value --
   ----------------------------------------

   function Select_Enum_Value_Bounded_By_Value
     (Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Start_Value     : Integer;
      Finish_Value    : Integer)
   return Kit_Literal_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Literal.Selection :=
         Kit.Db.Kit_Literal.Select_Enum_Value_Bounded_By_Value
         (Kit_Enumeration.Reference_Kit_Enumeration, Start_Value,
         Finish_Value);
   begin
      return (Db => Db_Selection);
   end Select_Enum_Value_Bounded_By_Value;

   -------------------------
   -- Set_Kit_Enumeration --
   -------------------------

   function Set_Kit_Enumeration
     (Update : Kit_Literal_Update_Handle;
      Value  : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class)
   return Kit_Literal_Update_Handle
   is
      Change : Kit_Literal_Update_Value (Update_Kit_Enumeration);
   begin
      Change.Kit_Enumeration_Value := Value.Reference_Kit_Enumeration;
      return Result : Kit_Literal_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Kit_Enumeration;

   --------------
   -- Set_Name --
   --------------

   function Set_Name
     (Update : Kit_Literal_Update_Handle;
      Value  : String)
   return Kit_Literal_Update_Handle
   is
      Change : Kit_Literal_Update_Value (Update_Name);
   begin
      Change.Name_Value.Length := Value'Length;
      Change.Name_Value.Text (1 .. Value'Length) := Value;
      return Result : Kit_Literal_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Name;

   ---------------
   -- Set_Value --
   ---------------

   function Set_Value
     (Update : Kit_Literal_Update_Handle;
      Value  : Integer)
   return Kit_Literal_Update_Handle
   is
      Change : Kit_Literal_Update_Value (Update_Value);
   begin
      Change.Value_Value := Value;
      return Result : Kit_Literal_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Value;

   ---------------------------
   -- To_Kit_Literal_Handle --
   ---------------------------

   function To_Kit_Literal_Handle (Class : Kit_Literal_Class)
      return Kit_Literal_Handle is
   begin
      return Get (Class.Reference_Kit_Literal);
   end To_Kit_Literal_Handle;

   ----------------
   -- Top_Record --
   ----------------

   overriding function Top_Record (Handle : Kit_Literal_Handle)
      return Kit.Db.Record_Type is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Top_Record;
   end Top_Record;

   ------------
   -- Update --
   ------------

   function Update (Handle : Kit_Literal_Handle)
      return Kit_Literal_Update_Handle'Class is
   begin
      return Update_Kit_Literal (Handle.Reference);
   end Update;

   ------------------------
   -- Update_Kit_Literal --
   ------------------------

   overriding function Update_Kit_Literal (Handle : Kit_Literal_Handle)
      return Kit_Literal_Update_Handle'Class is
   begin
      return Update_Kit_Literal (Handle.Reference);
   end Update_Kit_Literal;

   ------------------------
   -- Update_Kit_Literal --
   ------------------------

   function Update_Kit_Literal (Target : Kit.Db.Kit_Literal_Reference)
      return Kit_Literal_Update_Handle is
   begin
      return Update : Kit_Literal_Update_Handle do
         Update.Reference := Target;
      end return;
   end Update_Kit_Literal;

   -----------
   -- Value --
   -----------

   overriding function Value (Handle : Kit_Literal_Handle) return Integer is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Value;
   end Value;

end Kit.Handles.Kit_Literal;
