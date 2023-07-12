with Kit.Protected_Maps;
with Kit.Db.Kit_Field_Hashes;

package body Kit.Handles.Kit_Field is

   type Cached_Handle is
      record
         Top_Record               : Record_Type;
         Kit_Base_Kit_Root_Record : Kit_Root_Record_Reference;
         Name                     : Kit.Strings.String_Type (64);
         Kit_Record               : Kit_Record_Reference;
         Field_Type               : Kit_Type_Reference;
         Field_Offset             : Integer;
         Field_Length             : Integer;
         Created                  : Boolean;
         Readable                 : Boolean;
         Writeable                : Boolean;
         Display                  : Boolean;
         Base_Ref                 : Boolean;
      end record;
   procedure Load
     (Reference : Kit.Db.Kit_Field_Reference;
      Cached    : in out Cached_Handle);
   package Cached_Handle_Maps is
     new Kit.Protected_Maps
          (Kit.Db.Kit_Field_Reference,
         Cached_Handle,
         Load,
         Kit.Db.Kit_Field_Hashes.Hash,
         Db."=");
   subtype Constant_Reference_Type is
      Cached_Handle_Maps.Constant_Reference_Type;
   Cache : Cached_Handle_Maps.Map;
   type Iterator is
        new Selection_Iterator_Interfaces.Reversible_Iterator with
      record
         Container : access Kit_Field_Selection;
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

   --------------
   -- Base_Ref --
   --------------

   overriding function Base_Ref (Handle : Kit_Field_Handle) return Boolean is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Base_Ref;
   end Base_Ref;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Kit_Field_Selection;
      Position  : Cursor)
   return Kit_Field_Class
   is
   begin
      return Get (Kit.Db.Kit_Field.Element (Position.Db));
   end Constant_Reference;

   ------------
   -- Create --
   ------------

   procedure Create
     (Name         : String;
      Kit_Record   : Kit.Handles.Kit_Record.Kit_Record_Class;
      Field_Type   : Kit.Handles.Kit_Type.Kit_Type_Class;
      Field_Offset : Integer;
      Field_Length : Integer;
      Created      : Boolean;
      Readable     : Boolean;
      Writeable    : Boolean;
      Display      : Boolean;
      Base_Ref     : Boolean)
   is
      Handle : constant Kit_Field_Handle := Create (Name, Kit_Record,
         Field_Type, Field_Offset, Field_Length, Created, Readable,
         Writeable, Display, Base_Ref);
      pragma Unreferenced (Handle);
   begin
      null;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Name         : String;
      Kit_Record   : Kit.Handles.Kit_Record.Kit_Record_Class;
      Field_Type   : Kit.Handles.Kit_Type.Kit_Type_Class;
      Field_Offset : Integer;
      Field_Length : Integer;
      Created      : Boolean;
      Readable     : Boolean;
      Writeable    : Boolean;
      Display      : Boolean;
      Base_Ref     : Boolean)
   return Kit_Field_Handle
   is
   begin
      return Get (Kit.Db.Kit_Field.Create (Name,
         Kit_Record.Reference_Kit_Record, Field_Type.Reference_Kit_Type,
         Field_Offset, Field_Length, Created, Readable, Writeable, Display,
         Base_Ref));
   end Create;

   -------------
   -- Created --
   -------------

   overriding function Created (Handle : Kit_Field_Handle) return Boolean is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Created;
   end Created;

   -------------
   -- Display --
   -------------

   overriding function Display (Handle : Kit_Field_Handle) return Boolean is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Display;
   end Display;

   ----------
   -- Done --
   ----------

   procedure Done (Update : Kit_Field_Update_Handle) is
      Rec : Kit.Db.Kit_Field.Kit_Field_Update := Kit.Db.Kit_Field.Get_Update
         (Update.Reference);
   begin
      for Item of Update.Updates loop
         case Item.Field is
            when Update_Name =>
                  Rec.Set_Name (Item.Name_Value.Text
                     (1 .. Item.Name_Value.Length));
            when Update_Kit_Record =>
                  Rec.Set_Kit_Record (Item.Kit_Record_Value);
            when Update_Field_Type =>
                  Rec.Set_Field_Type (Item.Field_Type_Value);
            when Update_Field_Offset =>
                  Rec.Set_Field_Offset (Item.Field_Offset_Value);
            when Update_Field_Length =>
                  Rec.Set_Field_Length (Item.Field_Length_Value);
            when Update_Created =>
                  Rec.Set_Created (Item.Created_Value);
            when Update_Readable =>
                  Rec.Set_Readable (Item.Readable_Value);
            when Update_Writeable =>
                  Rec.Set_Writeable (Item.Writeable_Value);
            when Update_Display =>
                  Rec.Set_Display (Item.Display_Value);
            when Update_Base_Ref =>
                  Rec.Set_Base_Ref (Item.Base_Ref_Value);
         end case;
      end loop;
      Cache.Invalidate (Update.Reference);
   end Done;

   -------------
   -- Element --
   -------------

   function Element (Item : Cursor) return Kit_Field_Class is
   begin
      return Get (Kit.Db.Kit_Field.Element (Item.Db));
   end Element;

   ------------------
   -- Empty_Handle --
   ------------------

   function Empty_Handle return Kit_Field_Handle is
   begin
      return Get (Kit.Db.Null_Kit_Field_Reference);
   end Empty_Handle;

   ------------------
   -- Field_Length --
   ------------------

   overriding function Field_Length (Handle : Kit_Field_Handle)
      return Integer is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Field_Length;
   end Field_Length;

   ------------------
   -- Field_Offset --
   ------------------

   overriding function Field_Offset (Handle : Kit_Field_Handle)
      return Integer is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Field_Offset;
   end Field_Offset;

   ----------------
   -- Field_Type --
   ----------------

   overriding function Field_Type (Handle : Kit_Field_Handle)
      return Kit.Handles.Kit_Type.Kit_Type_Class is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Kit.Handles.Kit_Type.Get (Rec.Field_Type);
   end Field_Type;

   -----------
   -- First --
   -----------

   overriding function First (It : Iterator) return Cursor is
   begin
      return Position : Cursor do
         Position.Db := Kit.Db.Kit_Field.First (It.Container.Db);
      end return;
   end First;

   ----------------------------
   -- First_By_Display_Field --
   ----------------------------

   function First_By_Display_Field
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Display    : Boolean)
   return Kit_Field_Handle
   is
      use Kit.Db;
      Ref : constant Kit_Field_Reference :=
         Kit.Db.Kit_Field.First_By_Display_Field
         (Kit_Record.Reference_Kit_Record, Display);
      Result : Kit_Field_Handle;
   begin
      if Ref /= Null_Kit_Field_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Display_Field;

   -------------------------
   -- First_By_Kit_Record --
   -------------------------

   function First_By_Kit_Record (Kit_Record :
      Kit.Handles.Kit_Record.Kit_Record_Class) return Kit_Field_Handle is
      use Kit.Db;
      Ref : constant Kit_Field_Reference :=
         Kit.Db.Kit_Field.First_By_Kit_Record
         (Kit_Record.Reference_Kit_Record);
      Result : Kit_Field_Handle;
   begin
      if Ref /= Null_Kit_Field_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Kit_Record;

   -------------------------
   -- First_By_Top_Record --
   -------------------------

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Field_Handle is
      use Kit.Db;
      Ref : constant Kit_Field_Reference :=
         Kit.Db.Kit_Field.First_By_Top_Record (Top_Record);
      Result : Kit_Field_Handle;
   begin
      if Ref /= Null_Kit_Field_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Top_Record;

   ---------
   -- Get --
   ---------

   function Get (Reference : Kit.Db.Kit_Field_Reference)
      return Kit_Field_Handle is
   begin
      return Handle : Kit_Field_Handle do
         Handle.Reference := Reference;
      end return;
   end Get;

   -------------------------
   -- Get_By_Record_Field --
   -------------------------

   function Get_By_Record_Field
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Name       : String)
   return Kit_Field_Handle
   is
      use Kit.Db;
      Ref : constant Kit_Field_Reference :=
         Kit.Db.Kit_Field.Get_By_Record_Field
         (Kit_Record.Reference_Kit_Record, Name);
      Result : Kit_Field_Handle;
   begin
      if Ref /= Null_Kit_Field_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Get_By_Record_Field;

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
      return Kit_Field_Handle is
      use Kit.Db;
      Ref : constant Kit_Field_Reference :=
         Kit.Db.Kit_Field.Get_From_Kit_Root_Record
         (Kit_Root_Record.Reference_Kit_Root_Record);
      Result : Kit_Field_Handle;
   begin
      if Ref /= Null_Kit_Field_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Get_From_Kit_Root_Record;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Item : Cursor) return Boolean is
   begin
      return Kit.Db.Kit_Field.Has_Element (Item.Db);
   end Has_Element;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element (Handle : Kit_Field_Handle)
      return Boolean is
      use type Kit.Db.Kit_Field_Reference;
   begin
      return Handle.Reference /= Kit.Db.Null_Kit_Field_Reference;
   end Has_Element;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Kit_Field_Selection) return Boolean is
   begin
      return Kit.Db.Kit_Field.Is_Empty (Container.Db);
   end Is_Empty;

   ---------------------
   -- Is_Record_Field --
   ---------------------

   function Is_Record_Field
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Name       : String)
   return Boolean
   is
      Handle : constant Kit_Field_Handle := Get_By_Record_Field (Kit_Record,
         Name);
   begin
      return Handle.Has_Element;
   end Is_Record_Field;

   -------------
   -- Iterate --
   -------------

   function Iterate (Container : Kit_Field_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Result : Iterator do
         Result.Container := Container'Unrestricted_Access;
      end return;
   end Iterate;

   ----------------
   -- Kit_Record --
   ----------------

   overriding function Kit_Record (Handle : Kit_Field_Handle)
      return Kit.Handles.Kit_Record.Kit_Record_Class is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Kit.Handles.Kit_Record.Get (Rec.Kit_Record);
   end Kit_Record;

   ----------------------------
   -- Kit_Root_Record_Handle --
   ----------------------------

   function Kit_Root_Record_Handle (Handle : Kit_Field_Handle)
      return Kit.Handles.Kit_Root_Record.Kit_Root_Record_Handle is
      Rec : constant Kit.Db.Kit_Field.Kit_Field_Type := Kit.Db.Kit_Field.Get
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
         Position.Db := Kit.Db.Kit_Field.Last (It.Container.Db);
      end return;
   end Last;

   ---------------------------
   -- Last_By_Display_Field --
   ---------------------------

   function Last_By_Display_Field
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Display    : Boolean)
   return Kit_Field_Handle
   is
      use Kit.Db;
      Ref : constant Kit_Field_Reference :=
         Kit.Db.Kit_Field.Last_By_Display_Field
         (Kit_Record.Reference_Kit_Record, Display);
      Result : Kit_Field_Handle;
   begin
      if Ref /= Null_Kit_Field_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Display_Field;

   ------------------------
   -- Last_By_Kit_Record --
   ------------------------

   function Last_By_Kit_Record (Kit_Record :
      Kit.Handles.Kit_Record.Kit_Record_Class) return Kit_Field_Handle is
      use Kit.Db;
      Ref : constant Kit_Field_Reference :=
         Kit.Db.Kit_Field.Last_By_Kit_Record
         (Kit_Record.Reference_Kit_Record);
      Result : Kit_Field_Handle;
   begin
      if Ref /= Null_Kit_Field_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Kit_Record;

   ------------------------
   -- Last_By_Top_Record --
   ------------------------

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Field_Handle is
      use Kit.Db;
      Ref : constant Kit_Field_Reference :=
         Kit.Db.Kit_Field.Last_By_Top_Record (Top_Record);
      Result : Kit_Field_Handle;
   begin
      if Ref /= Null_Kit_Field_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Top_Record;

   ------------
   -- Length --
   ------------

   function Length (Container : Kit_Field_Selection) return Natural is
   begin
      return Kit.Db.Kit_Field.Length (Container.Db);
   end Length;

   ----------
   -- Load --
   ----------

   procedure Load
     (Reference : Kit.Db.Kit_Field_Reference;
      Cached    : in out Cached_Handle)
   is
      Rec : constant Kit.Db.Kit_Field.Kit_Field_Type := Kit.Db.Kit_Field.Get
         (Reference);
   begin
      Cached.Top_Record := Rec.Top_Record;
      Cached.Kit_Base_Kit_Root_Record := Rec.Get_Kit_Root_Record_Reference;
      Cached.Name.Length := Rec.Name'Length;
      Cached.Name.Text (1 .. Rec.Name'Length) := Rec.Name;
      Cached.Kit_Record := Rec.Kit_Record;
      Cached.Field_Type := Rec.Field_Type;
      Cached.Field_Offset := Rec.Field_Offset;
      Cached.Field_Length := Rec.Field_Length;
      Cached.Created := Rec.Created;
      Cached.Readable := Rec.Readable;
      Cached.Writeable := Rec.Writeable;
      Cached.Display := Rec.Display;
      Cached.Base_Ref := Rec.Base_Ref;
   end Load;

   ----------
   -- Name --
   ----------

   overriding function Name (Handle : Kit_Field_Handle) return String is
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
         Result.Db := Kit.Db.Kit_Field.Next (Position.Db);
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
         Result.Db := Kit.Db.Kit_Field.Previous (Position.Db);
      end return;
   end Previous;

   --------------
   -- Readable --
   --------------

   overriding function Readable (Handle : Kit_Field_Handle) return Boolean is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Readable;
   end Readable;

   ---------------
   -- Reference --
   ---------------

   function Reference (Handle : Kit_Field_Handle)
      return Kit.Db.Kit_Field_Reference is
   begin
      return Handle.Reference;
   end Reference;

   -------------------------
   -- Reference_Kit_Field --
   -------------------------

   overriding function Reference_Kit_Field (Handle : Kit_Field_Handle)
      return Kit.Db.Kit_Field_Reference is
   begin
      return Handle.Reference;
   end Reference_Kit_Field;

   -------------------------------
   -- Reference_Kit_Root_Record --
   -------------------------------

   overriding function Reference_Kit_Root_Record (Handle : Kit_Field_Handle)
      return Kit.Db.Kit_Root_Record_Reference is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Kit_Base_Kit_Root_Record;
   end Reference_Kit_Root_Record;

   ---------------------------
   -- Scan_By_Display_Field --
   ---------------------------

   function Scan_By_Display_Field return Kit_Field_Selection is
      Db_Selection : constant Kit.Db.Kit_Field.Selection :=
         Kit.Db.Kit_Field.Scan_By_Display_Field;
   begin
      return (Db => Db_Selection);
   end Scan_By_Display_Field;

   ------------------------
   -- Scan_By_Kit_Record --
   ------------------------

   function Scan_By_Kit_Record return Kit_Field_Selection is
      Db_Selection : constant Kit.Db.Kit_Field.Selection :=
         Kit.Db.Kit_Field.Scan_By_Kit_Record;
   begin
      return (Db => Db_Selection);
   end Scan_By_Kit_Record;

   --------------------------
   -- Scan_By_Record_Field --
   --------------------------

   function Scan_By_Record_Field return Kit_Field_Selection is
      Db_Selection : constant Kit.Db.Kit_Field.Selection :=
         Kit.Db.Kit_Field.Scan_By_Record_Field;
   begin
      return (Db => Db_Selection);
   end Scan_By_Record_Field;

   ------------------------
   -- Scan_By_Top_Record --
   ------------------------

   function Scan_By_Top_Record return Kit_Field_Selection is
      Db_Selection : constant Kit.Db.Kit_Field.Selection :=
         Kit.Db.Kit_Field.Scan_By_Top_Record;
   begin
      return (Db => Db_Selection);
   end Scan_By_Top_Record;

   ----------------------------------
   -- Select_Bounded_By_Top_Record --
   ----------------------------------

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Field_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Field.Selection :=
         Kit.Db.Kit_Field.Select_Bounded_By_Top_Record (Start_Top_Record,
         Finish_Top_Record);
   begin
      return (Db => Db_Selection);
   end Select_Bounded_By_Top_Record;

   -----------------------------
   -- Select_By_Display_Field --
   -----------------------------

   function Select_By_Display_Field
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Display    : Boolean)
   return Kit_Field_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Field.Selection :=
         Kit.Db.Kit_Field.Select_By_Display_Field
         (Kit_Record.Reference_Kit_Record, Display);
   begin
      return (Db => Db_Selection);
   end Select_By_Display_Field;

   --------------------------
   -- Select_By_Kit_Record --
   --------------------------

   function Select_By_Kit_Record (Kit_Record :
      Kit.Handles.Kit_Record.Kit_Record_Class) return Kit_Field_Selection is
      Db_Selection : constant Kit.Db.Kit_Field.Selection :=
         Kit.Db.Kit_Field.Select_By_Kit_Record
         (Kit_Record.Reference_Kit_Record);
   begin
      return (Db => Db_Selection);
   end Select_By_Kit_Record;

   ----------------------------
   -- Select_By_Record_Field --
   ----------------------------

   function Select_By_Record_Field
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Name       : String)
   return Kit_Field_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Field.Selection :=
         Kit.Db.Kit_Field.Select_By_Record_Field
         (Kit_Record.Reference_Kit_Record, Name);
   begin
      return (Db => Db_Selection);
   end Select_By_Record_Field;

   --------------------------
   -- Select_By_Top_Record --
   --------------------------

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Field_Selection is
      Db_Selection : constant Kit.Db.Kit_Field.Selection :=
         Kit.Db.Kit_Field.Select_By_Top_Record (Top_Record);
   begin
      return (Db => Db_Selection);
   end Select_By_Top_Record;

   ---------------------------------------------
   -- Select_Display_Field_Bounded_By_Display --
   ---------------------------------------------

   function Select_Display_Field_Bounded_By_Display
     (Kit_Record     : Kit.Handles.Kit_Record.Kit_Record_Class;
      Start_Display  : Boolean;
      Finish_Display : Boolean)
   return Kit_Field_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Field.Selection :=
         Kit.Db.Kit_Field.Select_Display_Field_Bounded_By_Display
         (Kit_Record.Reference_Kit_Record, Start_Display, Finish_Display);
   begin
      return (Db => Db_Selection);
   end Select_Display_Field_Bounded_By_Display;

   -----------------------------------------
   -- Select_Record_Field_Bounded_By_Name --
   -----------------------------------------

   function Select_Record_Field_Bounded_By_Name
     (Kit_Record  : Kit.Handles.Kit_Record.Kit_Record_Class;
      Start_Name  : String;
      Finish_Name : String)
   return Kit_Field_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Field.Selection :=
         Kit.Db.Kit_Field.Select_Record_Field_Bounded_By_Name
         (Kit_Record.Reference_Kit_Record, Start_Name, Finish_Name);
   begin
      return (Db => Db_Selection);
   end Select_Record_Field_Bounded_By_Name;

   ------------------
   -- Set_Base_Ref --
   ------------------

   function Set_Base_Ref
     (Update : Kit_Field_Update_Handle;
      Value  : Boolean)
   return Kit_Field_Update_Handle
   is
      Change : Kit_Field_Update_Value (Update_Base_Ref);
   begin
      Change.Base_Ref_Value := Value;
      return Result : Kit_Field_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Base_Ref;

   -----------------
   -- Set_Created --
   -----------------

   function Set_Created
     (Update : Kit_Field_Update_Handle;
      Value  : Boolean)
   return Kit_Field_Update_Handle
   is
      Change : Kit_Field_Update_Value (Update_Created);
   begin
      Change.Created_Value := Value;
      return Result : Kit_Field_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Created;

   -----------------
   -- Set_Display --
   -----------------

   function Set_Display
     (Update : Kit_Field_Update_Handle;
      Value  : Boolean)
   return Kit_Field_Update_Handle
   is
      Change : Kit_Field_Update_Value (Update_Display);
   begin
      Change.Display_Value := Value;
      return Result : Kit_Field_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Display;

   ----------------------
   -- Set_Field_Length --
   ----------------------

   function Set_Field_Length
     (Update : Kit_Field_Update_Handle;
      Value  : Integer)
   return Kit_Field_Update_Handle
   is
      Change : Kit_Field_Update_Value (Update_Field_Length);
   begin
      Change.Field_Length_Value := Value;
      return Result : Kit_Field_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Field_Length;

   ----------------------
   -- Set_Field_Offset --
   ----------------------

   function Set_Field_Offset
     (Update : Kit_Field_Update_Handle;
      Value  : Integer)
   return Kit_Field_Update_Handle
   is
      Change : Kit_Field_Update_Value (Update_Field_Offset);
   begin
      Change.Field_Offset_Value := Value;
      return Result : Kit_Field_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Field_Offset;

   --------------------
   -- Set_Field_Type --
   --------------------

   function Set_Field_Type
     (Update : Kit_Field_Update_Handle;
      Value  : Kit.Handles.Kit_Type.Kit_Type_Class)
   return Kit_Field_Update_Handle
   is
      Change : Kit_Field_Update_Value (Update_Field_Type);
   begin
      Change.Field_Type_Value := Value.Reference_Kit_Type;
      return Result : Kit_Field_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Field_Type;

   --------------------
   -- Set_Kit_Record --
   --------------------

   function Set_Kit_Record
     (Update : Kit_Field_Update_Handle;
      Value  : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Field_Update_Handle
   is
      Change : Kit_Field_Update_Value (Update_Kit_Record);
   begin
      Change.Kit_Record_Value := Value.Reference_Kit_Record;
      return Result : Kit_Field_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Kit_Record;

   --------------
   -- Set_Name --
   --------------

   function Set_Name
     (Update : Kit_Field_Update_Handle;
      Value  : String)
   return Kit_Field_Update_Handle
   is
      Change : Kit_Field_Update_Value (Update_Name);
   begin
      Change.Name_Value.Length := Value'Length;
      Change.Name_Value.Text (1 .. Value'Length) := Value;
      return Result : Kit_Field_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Name;

   ------------------
   -- Set_Readable --
   ------------------

   function Set_Readable
     (Update : Kit_Field_Update_Handle;
      Value  : Boolean)
   return Kit_Field_Update_Handle
   is
      Change : Kit_Field_Update_Value (Update_Readable);
   begin
      Change.Readable_Value := Value;
      return Result : Kit_Field_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Readable;

   -------------------
   -- Set_Writeable --
   -------------------

   function Set_Writeable
     (Update : Kit_Field_Update_Handle;
      Value  : Boolean)
   return Kit_Field_Update_Handle
   is
      Change : Kit_Field_Update_Value (Update_Writeable);
   begin
      Change.Writeable_Value := Value;
      return Result : Kit_Field_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Writeable;

   -------------------------
   -- To_Kit_Field_Handle --
   -------------------------

   function To_Kit_Field_Handle (Class : Kit_Field_Class)
      return Kit_Field_Handle is
   begin
      return Get (Class.Reference_Kit_Field);
   end To_Kit_Field_Handle;

   ----------------
   -- Top_Record --
   ----------------

   overriding function Top_Record (Handle : Kit_Field_Handle)
      return Kit.Db.Record_Type is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Top_Record;
   end Top_Record;

   ------------
   -- Update --
   ------------

   function Update (Handle : Kit_Field_Handle)
      return Kit_Field_Update_Handle'Class is
   begin
      return Update_Kit_Field (Handle.Reference);
   end Update;

   ----------------------
   -- Update_Kit_Field --
   ----------------------

   function Update_Kit_Field (Target : Kit.Db.Kit_Field_Reference)
      return Kit_Field_Update_Handle is
   begin
      return Update : Kit_Field_Update_Handle do
         Update.Reference := Target;
      end return;
   end Update_Kit_Field;

   ----------------------
   -- Update_Kit_Field --
   ----------------------

   overriding function Update_Kit_Field (Handle : Kit_Field_Handle)
      return Kit_Field_Update_Handle'Class is
   begin
      return Update_Kit_Field (Handle.Reference);
   end Update_Kit_Field;

   ---------------
   -- Writeable --
   ---------------

   overriding function Writeable (Handle : Kit_Field_Handle)
      return Boolean is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Writeable;
   end Writeable;

end Kit.Handles.Kit_Field;
