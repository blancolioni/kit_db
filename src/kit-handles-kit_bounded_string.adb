with Kit.Protected_Maps;
with Kit.Db.Kit_Bounded_String_Hashes;

package body Kit.Handles.Kit_Bounded_String is

   type Cached_Handle is
      record
         Top_Record               : Record_Type;
         Kit_Base_Kit_Root_Record : Kit_Root_Record_Reference;
         Kit_Base_Kit_Type        : Kit_Type_Reference;
         Length                   : Integer;
         Size                     : Integer;
         Name                     : Kit.Strings.String_Type (64);
         Kit_Base_Kit_String      : Kit_String_Reference;
      end record;
   procedure Load
     (Reference : Kit.Db.Kit_Bounded_String_Reference;
      Cached    : in out Cached_Handle);
   package Cached_Handle_Maps is
     new Kit.Protected_Maps
          (Kit.Db.Kit_Bounded_String_Reference,
         Cached_Handle,
         Load,
         Kit.Db.Kit_Bounded_String_Hashes.Hash,
         Db."=");
   subtype Constant_Reference_Type is
      Cached_Handle_Maps.Constant_Reference_Type;
   Cache : Cached_Handle_Maps.Map;
   type Iterator is
        new Selection_Iterator_Interfaces.Reversible_Iterator with
      record
         Container : access Kit_Bounded_String_Selection;
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
     (Container : aliased Kit_Bounded_String_Selection;
      Position  : Cursor)
   return Kit_Bounded_String_Class
   is
   begin
      return Get (Kit.Db.Kit_Bounded_String.Element (Position.Db));
   end Constant_Reference;

   ------------
   -- Create --
   ------------

   function Create
     (Length : Integer;
      Size   : Integer;
      Name   : String)
   return Kit_Bounded_String_Handle
   is
   begin
      return Get (Kit.Db.Kit_Bounded_String.Create (Length, Size, Name));
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Length : Integer;
      Size   : Integer;
      Name   : String)
   is
      Handle : constant Kit_Bounded_String_Handle := Create (Length, Size,
         Name);
      pragma Unreferenced (Handle);
   begin
      null;
   end Create;

   ----------
   -- Done --
   ----------

   procedure Done (Update : Kit_Bounded_String_Update_Handle) is
      Rec : Kit.Db.Kit_Bounded_String.Kit_Bounded_String_Update :=
         Kit.Db.Kit_Bounded_String.Get_Update (Update.Reference);
   begin
      for Item of Update.Updates loop
         case Item.Field is
            when Update_Length =>
                  Rec.Set_Length (Item.Length_Value);
            when Update_Size =>
                  Rec.Set_Size (Item.Size_Value);
            when Update_Name =>
                  Rec.Set_Name (Item.Name_Value.Text
                     (1 .. Item.Name_Value.Length));
         end case;
      end loop;
      Cache.Invalidate (Update.Reference);
   end Done;

   -------------
   -- Element --
   -------------

   function Element (Item : Cursor) return Kit_Bounded_String_Class is
   begin
      return Get (Kit.Db.Kit_Bounded_String.Element (Item.Db));
   end Element;

   ------------------
   -- Empty_Handle --
   ------------------

   function Empty_Handle return Kit_Bounded_String_Handle is
   begin
      return Get (Kit.Db.Null_Kit_Bounded_String_Reference);
   end Empty_Handle;

   -----------
   -- First --
   -----------

   overriding function First (It : Iterator) return Cursor is
   begin
      return Position : Cursor do
         Position.Db := Kit.Db.Kit_Bounded_String.First (It.Container.Db);
      end return;
   end First;

   -------------------------
   -- First_By_Top_Record --
   -------------------------

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Bounded_String_Handle is
      use Kit.Db;
      Ref : constant Kit_Bounded_String_Reference :=
         Kit.Db.Kit_Bounded_String.First_By_Top_Record (Top_Record);
      Result : Kit_Bounded_String_Handle;
   begin
      if Ref /= Null_Kit_Bounded_String_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Top_Record;

   ---------
   -- Get --
   ---------

   function Get (Reference : Kit.Db.Kit_Bounded_String_Reference)
      return Kit_Bounded_String_Handle is
   begin
      return Handle : Kit_Bounded_String_Handle do
         Handle.Reference := Reference;
      end return;
   end Get;

   -----------------
   -- Get_By_Name --
   -----------------

   function Get_By_Name (Name : String) return Kit_Bounded_String_Handle is
      use Kit.Db;
      Ref : constant Kit_Bounded_String_Reference :=
         Kit.Db.Kit_Bounded_String.Get_By_Name (Name);
      Result : Kit_Bounded_String_Handle;
   begin
      if Ref /= Null_Kit_Bounded_String_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Get_By_Name;

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
      return Kit_Bounded_String_Handle is
      use Kit.Db;
      Ref : constant Kit_Bounded_String_Reference :=
         Kit.Db.Kit_Bounded_String.Get_From_Kit_Root_Record
         (Kit_Root_Record.Reference_Kit_Root_Record);
      Result : Kit_Bounded_String_Handle;
   begin
      if Ref /= Null_Kit_Bounded_String_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Get_From_Kit_Root_Record;

   -------------------------
   -- Get_From_Kit_String --
   -------------------------

   function Get_From_Kit_String (Kit_String :
      Kit.Handles.Kit_String.Kit_String_Class)
      return Kit_Bounded_String_Handle is
      use Kit.Db;
      Ref : constant Kit_Bounded_String_Reference :=
         Kit.Db.Kit_Bounded_String.Get_From_Kit_String
         (Kit_String.Reference_Kit_String);
      Result : Kit_Bounded_String_Handle;
   begin
      if Ref /= Null_Kit_Bounded_String_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Get_From_Kit_String;

   -----------------------
   -- Get_From_Kit_Type --
   -----------------------

   function Get_From_Kit_Type (Kit_Type :
      Kit.Handles.Kit_Type.Kit_Type_Class)
      return Kit_Bounded_String_Handle is
      use Kit.Db;
      Ref : constant Kit_Bounded_String_Reference :=
         Kit.Db.Kit_Bounded_String.Get_From_Kit_Type
         (Kit_Type.Reference_Kit_Type);
      Result : Kit_Bounded_String_Handle;
   begin
      if Ref /= Null_Kit_Bounded_String_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Get_From_Kit_Type;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element (Handle : Kit_Bounded_String_Handle)
      return Boolean is
      use type Kit.Db.Kit_Bounded_String_Reference;
   begin
      return Handle.Reference /= Kit.Db.Null_Kit_Bounded_String_Reference;
   end Has_Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Item : Cursor) return Boolean is
   begin
      return Kit.Db.Kit_Bounded_String.Has_Element (Item.Db);
   end Has_Element;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Kit_Bounded_String_Selection)
      return Boolean is
   begin
      return Kit.Db.Kit_Bounded_String.Is_Empty (Container.Db);
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   function Iterate (Container : Kit_Bounded_String_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Result : Iterator do
         Result.Container := Container'Unrestricted_Access;
      end return;
   end Iterate;

   ----------------------------
   -- Kit_Root_Record_Handle --
   ----------------------------

   function Kit_Root_Record_Handle (Handle : Kit_Bounded_String_Handle)
      return Kit.Handles.Kit_Root_Record.Kit_Root_Record_Handle is
      Rec : constant Kit.Db.Kit_Bounded_String.Kit_Bounded_String_Type :=
         Kit.Db.Kit_Bounded_String.Get (Handle.Reference);
   begin
      return Kit.Handles.Kit_Root_Record.Get
         (Rec.Get_Kit_Root_Record_Reference);
   end Kit_Root_Record_Handle;

   -----------------------
   -- Kit_String_Handle --
   -----------------------

   function Kit_String_Handle (Handle : Kit_Bounded_String_Handle)
      return Kit.Handles.Kit_String.Kit_String_Handle is
      Rec : constant Kit.Db.Kit_Bounded_String.Kit_Bounded_String_Type :=
         Kit.Db.Kit_Bounded_String.Get (Handle.Reference);
   begin
      return Kit.Handles.Kit_String.Get (Rec.Get_Kit_String_Reference);
   end Kit_String_Handle;

   ---------------------
   -- Kit_Type_Handle --
   ---------------------

   function Kit_Type_Handle (Handle : Kit_Bounded_String_Handle)
      return Kit.Handles.Kit_Type.Kit_Type_Handle is
      Rec : constant Kit.Db.Kit_Bounded_String.Kit_Bounded_String_Type :=
         Kit.Db.Kit_Bounded_String.Get (Handle.Reference);
   begin
      return Kit.Handles.Kit_Type.Get (Rec.Get_Kit_Type_Reference);
   end Kit_Type_Handle;

   ----------
   -- Last --
   ----------

   overriding function Last (It : Iterator) return Cursor is
   begin
      return Position : Cursor do
         Position.Db := Kit.Db.Kit_Bounded_String.Last (It.Container.Db);
      end return;
   end Last;

   ------------------------
   -- Last_By_Top_Record --
   ------------------------

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Bounded_String_Handle is
      use Kit.Db;
      Ref : constant Kit_Bounded_String_Reference :=
         Kit.Db.Kit_Bounded_String.Last_By_Top_Record (Top_Record);
      Result : Kit_Bounded_String_Handle;
   begin
      if Ref /= Null_Kit_Bounded_String_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Top_Record;

   ------------
   -- Length --
   ------------

   overriding function Length (Handle : Kit_Bounded_String_Handle)
      return Integer is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Length;
   end Length;

   ------------
   -- Length --
   ------------

   function Length (Container : Kit_Bounded_String_Selection)
      return Natural is
   begin
      return Kit.Db.Kit_Bounded_String.Length (Container.Db);
   end Length;

   ----------
   -- Load --
   ----------

   procedure Load
     (Reference : Kit.Db.Kit_Bounded_String_Reference;
      Cached    : in out Cached_Handle)
   is
      Rec : constant Kit.Db.Kit_Bounded_String.Kit_Bounded_String_Type :=
         Kit.Db.Kit_Bounded_String.Get (Reference);
   begin
      Cached.Top_Record := Rec.Top_Record;
      Cached.Kit_Base_Kit_Root_Record := Rec.Get_Kit_Root_Record_Reference;
      Cached.Kit_Base_Kit_Type := Rec.Get_Kit_Type_Reference;
      Cached.Length := Rec.Length;
      Cached.Size := Rec.Size;
      Cached.Name.Length := Rec.Name'Length;
      Cached.Name.Text (1 .. Rec.Name'Length) := Rec.Name;
      Cached.Kit_Base_Kit_String := Rec.Get_Kit_String_Reference;
   end Load;

   ----------
   -- Name --
   ----------

   overriding function Name (Handle : Kit_Bounded_String_Handle)
      return String is
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
         Result.Db := Kit.Db.Kit_Bounded_String.Next (Position.Db);
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
         Result.Db := Kit.Db.Kit_Bounded_String.Previous (Position.Db);
      end return;
   end Previous;

   ---------------
   -- Reference --
   ---------------

   function Reference (Handle : Kit_Bounded_String_Handle)
      return Kit.Db.Kit_Bounded_String_Reference is
   begin
      return Handle.Reference;
   end Reference;

   ----------------------------------
   -- Reference_Kit_Bounded_String --
   ----------------------------------

   overriding function Reference_Kit_Bounded_String (Handle :
      Kit_Bounded_String_Handle)
      return Kit.Db.Kit_Bounded_String_Reference is
   begin
      return Handle.Reference;
   end Reference_Kit_Bounded_String;

   -------------------------------
   -- Reference_Kit_Root_Record --
   -------------------------------

   overriding function Reference_Kit_Root_Record (Handle :
      Kit_Bounded_String_Handle) return Kit.Db.Kit_Root_Record_Reference is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Kit_Base_Kit_Root_Record;
   end Reference_Kit_Root_Record;

   --------------------------
   -- Reference_Kit_String --
   --------------------------

   overriding function Reference_Kit_String (Handle :
      Kit_Bounded_String_Handle) return Kit.Db.Kit_String_Reference is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Kit_Base_Kit_String;
   end Reference_Kit_String;

   ------------------------
   -- Reference_Kit_Type --
   ------------------------

   overriding function Reference_Kit_Type (Handle :
      Kit_Bounded_String_Handle) return Kit.Db.Kit_Type_Reference is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Kit_Base_Kit_Type;
   end Reference_Kit_Type;

   ------------------
   -- Scan_By_Name --
   ------------------

   function Scan_By_Name return Kit_Bounded_String_Selection is
      Db_Selection : constant Kit.Db.Kit_Bounded_String.Selection :=
         Kit.Db.Kit_Bounded_String.Scan_By_Name;
   begin
      return (Db => Db_Selection);
   end Scan_By_Name;

   ------------------------
   -- Scan_By_Top_Record --
   ------------------------

   function Scan_By_Top_Record return Kit_Bounded_String_Selection is
      Db_Selection : constant Kit.Db.Kit_Bounded_String.Selection :=
         Kit.Db.Kit_Bounded_String.Scan_By_Top_Record;
   begin
      return (Db => Db_Selection);
   end Scan_By_Top_Record;

   ----------------------------
   -- Select_Bounded_By_Name --
   ----------------------------

   function Select_Bounded_By_Name
     (Start_Name  : String;
      Finish_Name : String)
   return Kit_Bounded_String_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Bounded_String.Selection :=
         Kit.Db.Kit_Bounded_String.Select_Bounded_By_Name (Start_Name,
         Finish_Name);
   begin
      return (Db => Db_Selection);
   end Select_Bounded_By_Name;

   ----------------------------------
   -- Select_Bounded_By_Top_Record --
   ----------------------------------

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Bounded_String_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Bounded_String.Selection :=
         Kit.Db.Kit_Bounded_String.Select_Bounded_By_Top_Record
         (Start_Top_Record, Finish_Top_Record);
   begin
      return (Db => Db_Selection);
   end Select_Bounded_By_Top_Record;

   --------------------
   -- Select_By_Name --
   --------------------

   function Select_By_Name (Name : String)
      return Kit_Bounded_String_Selection is
      Db_Selection : constant Kit.Db.Kit_Bounded_String.Selection :=
         Kit.Db.Kit_Bounded_String.Select_By_Name (Name);
   begin
      return (Db => Db_Selection);
   end Select_By_Name;

   --------------------------
   -- Select_By_Top_Record --
   --------------------------

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Bounded_String_Selection is
      Db_Selection : constant Kit.Db.Kit_Bounded_String.Selection :=
         Kit.Db.Kit_Bounded_String.Select_By_Top_Record (Top_Record);
   begin
      return (Db => Db_Selection);
   end Select_By_Top_Record;

   ----------------
   -- Set_Length --
   ----------------

   function Set_Length
     (Update : Kit_Bounded_String_Update_Handle;
      Value  : Integer)
   return Kit_Bounded_String_Update_Handle
   is
      Change : Kit_Bounded_String_Update_Value (Update_Length);
   begin
      Change.Length_Value := Value;
      return Result : Kit_Bounded_String_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Length;

   --------------
   -- Set_Name --
   --------------

   function Set_Name
     (Update : Kit_Bounded_String_Update_Handle;
      Value  : String)
   return Kit_Bounded_String_Update_Handle
   is
      Change : Kit_Bounded_String_Update_Value (Update_Name);
   begin
      Change.Name_Value.Length := Value'Length;
      Change.Name_Value.Text (1 .. Value'Length) := Value;
      return Result : Kit_Bounded_String_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Name;

   --------------
   -- Set_Size --
   --------------

   function Set_Size
     (Update : Kit_Bounded_String_Update_Handle;
      Value  : Integer)
   return Kit_Bounded_String_Update_Handle
   is
      Change : Kit_Bounded_String_Update_Value (Update_Size);
   begin
      Change.Size_Value := Value;
      return Result : Kit_Bounded_String_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Size;

   ----------
   -- Size --
   ----------

   overriding function Size (Handle : Kit_Bounded_String_Handle)
      return Integer is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Size;
   end Size;

   ----------------------------------
   -- To_Kit_Bounded_String_Handle --
   ----------------------------------

   function To_Kit_Bounded_String_Handle (Class : Kit_Bounded_String_Class)
      return Kit_Bounded_String_Handle is
   begin
      return Get (Class.Reference_Kit_Bounded_String);
   end To_Kit_Bounded_String_Handle;

   ----------------
   -- Top_Record --
   ----------------

   overriding function Top_Record (Handle : Kit_Bounded_String_Handle)
      return Kit.Db.Record_Type is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Top_Record;
   end Top_Record;

   ------------
   -- Update --
   ------------

   function Update (Handle : Kit_Bounded_String_Handle)
      return Kit_Bounded_String_Update_Handle'Class is
   begin
      return Update_Kit_Bounded_String (Handle.Reference);
   end Update;

   -------------------------------
   -- Update_Kit_Bounded_String --
   -------------------------------

   function Update_Kit_Bounded_String (Target :
      Kit.Db.Kit_Bounded_String_Reference)
      return Kit_Bounded_String_Update_Handle is
   begin
      return Update : Kit_Bounded_String_Update_Handle do
         Update.Reference := Target;
      end return;
   end Update_Kit_Bounded_String;

   -------------------------------
   -- Update_Kit_Bounded_String --
   -------------------------------

   overriding function Update_Kit_Bounded_String (Handle :
      Kit_Bounded_String_Handle)
      return Kit_Bounded_String_Update_Handle'Class is
   begin
      return Update_Kit_Bounded_String (Handle.Reference);
   end Update_Kit_Bounded_String;

   -----------------------
   -- Update_Kit_String --
   -----------------------

   overriding function Update_Kit_String (Handle : Kit_Bounded_String_Handle)
      return Kit_String.Kit_String_Update_Handle'Class is
   begin
      Cache.Invalidate (Handle.Reference);
      return Kit_String.Update_Kit_String (Handle.Reference_Kit_String);
   end Update_Kit_String;

   ---------------------
   -- Update_Kit_Type --
   ---------------------

   overriding function Update_Kit_Type (Handle : Kit_Bounded_String_Handle)
      return Kit_Type.Kit_Type_Update_Handle'Class is
   begin
      Cache.Invalidate (Handle.Reference);
      return Kit_Type.Update_Kit_Type (Handle.Reference_Kit_Type);
   end Update_Kit_Type;

end Kit.Handles.Kit_Bounded_String;
