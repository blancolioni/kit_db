with Kit.Protected_Maps;
with Kit.Db.Kit_Record_Base_Hashes;

package body Kit.Handles.Kit_Record_Base is

   type Cached_Handle is
      record
         Top_Record               : Record_Type;
         Kit_Base_Kit_Root_Record : Kit_Root_Record_Reference;
         Offset                   : Integer;
         Base                     : Kit_Record_Reference;
         Derived                  : Kit_Record_Reference;
      end record;
   procedure Load
     (Reference : Kit.Db.Kit_Record_Base_Reference;
      Cached    : in out Cached_Handle);
   package Cached_Handle_Maps is
     new Kit.Protected_Maps
          (Kit.Db.Kit_Record_Base_Reference,
         Cached_Handle,
         Load,
         Kit.Db.Kit_Record_Base_Hashes.Hash,
         Db."=");
   subtype Constant_Reference_Type is
      Cached_Handle_Maps.Constant_Reference_Type;
   Cache : Cached_Handle_Maps.Map;
   type Iterator is
        new Selection_Iterator_Interfaces.Reversible_Iterator with
      record
         Container : access Kit_Record_Base_Selection;
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

   ----------
   -- Base --
   ----------

   overriding function Base (Handle : Kit_Record_Base_Handle)
      return Kit.Handles.Kit_Record.Kit_Record_Class is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Kit.Handles.Kit_Record.Get (Rec.Base);
   end Base;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Kit_Record_Base_Selection;
      Position  : Cursor)
   return Kit_Record_Base_Class
   is
   begin
      return Get (Kit.Db.Kit_Record_Base.Element (Position.Db));
   end Constant_Reference;

   ------------
   -- Create --
   ------------

   procedure Create
     (Offset  : Integer;
      Base    : Kit.Handles.Kit_Record.Kit_Record_Class;
      Derived : Kit.Handles.Kit_Record.Kit_Record_Class)
   is
      Handle : constant Kit_Record_Base_Handle := Create (Offset, Base,
         Derived);
      pragma Unreferenced (Handle);
   begin
      null;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Offset  : Integer;
      Base    : Kit.Handles.Kit_Record.Kit_Record_Class;
      Derived : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Record_Base_Handle
   is
   begin
      return Get (Kit.Db.Kit_Record_Base.Create (Offset,
         Base.Reference_Kit_Record, Derived.Reference_Kit_Record));
   end Create;

   -------------
   -- Derived --
   -------------

   overriding function Derived (Handle : Kit_Record_Base_Handle)
      return Kit.Handles.Kit_Record.Kit_Record_Class is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Kit.Handles.Kit_Record.Get (Rec.Derived);
   end Derived;

   ----------
   -- Done --
   ----------

   procedure Done (Update : Kit_Record_Base_Update_Handle) is
      Rec : Kit.Db.Kit_Record_Base.Kit_Record_Base_Update :=
         Kit.Db.Kit_Record_Base.Get_Update (Update.Reference);
   begin
      for Item of Update.Updates loop
         case Item.Field is
            when Update_Offset =>
                  Rec.Set_Offset (Item.Offset_Value);
            when Update_Base =>
                  Rec.Set_Base (Item.Base_Value);
            when Update_Derived =>
                  Rec.Set_Derived (Item.Derived_Value);
         end case;
      end loop;
      Cache.Invalidate (Update.Reference);
   end Done;

   -------------
   -- Element --
   -------------

   function Element (Item : Cursor) return Kit_Record_Base_Class is
   begin
      return Get (Kit.Db.Kit_Record_Base.Element (Item.Db));
   end Element;

   ------------------
   -- Empty_Handle --
   ------------------

   function Empty_Handle return Kit_Record_Base_Handle is
   begin
      return Get (Kit.Db.Null_Kit_Record_Base_Reference);
   end Empty_Handle;

   -----------
   -- First --
   -----------

   overriding function First (It : Iterator) return Cursor is
   begin
      return Position : Cursor do
         Position.Db := Kit.Db.Kit_Record_Base.First (It.Container.Db);
      end return;
   end First;

   -------------------
   -- First_By_Base --
   -------------------

   function First_By_Base (Base : Kit.Handles.Kit_Record.Kit_Record_Class)
      return Kit_Record_Base_Handle is
      use Kit.Db;
      Ref : constant Kit_Record_Base_Reference :=
         Kit.Db.Kit_Record_Base.First_By_Base (Base.Reference_Kit_Record);
      Result : Kit_Record_Base_Handle;
   begin
      if Ref /= Null_Kit_Record_Base_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Base;

   --------------------------
   -- First_By_Base_Record --
   --------------------------

   function First_By_Base_Record
     (Base    : Kit.Handles.Kit_Record.Kit_Record_Class;
      Derived : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Record_Base_Handle
   is
      use Kit.Db;
      Ref : constant Kit_Record_Base_Reference :=
         Kit.Db.Kit_Record_Base.First_By_Base_Record
         (Base.Reference_Kit_Record, Derived.Reference_Kit_Record);
      Result : Kit_Record_Base_Handle;
   begin
      if Ref /= Null_Kit_Record_Base_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Base_Record;

   ----------------------
   -- First_By_Derived --
   ----------------------

   function First_By_Derived (Derived :
      Kit.Handles.Kit_Record.Kit_Record_Class)
      return Kit_Record_Base_Handle is
      use Kit.Db;
      Ref : constant Kit_Record_Base_Reference :=
         Kit.Db.Kit_Record_Base.First_By_Derived
         (Derived.Reference_Kit_Record);
      Result : Kit_Record_Base_Handle;
   begin
      if Ref /= Null_Kit_Record_Base_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Derived;

   -------------------------
   -- First_By_Top_Record --
   -------------------------

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Record_Base_Handle is
      use Kit.Db;
      Ref : constant Kit_Record_Base_Reference :=
         Kit.Db.Kit_Record_Base.First_By_Top_Record (Top_Record);
      Result : Kit_Record_Base_Handle;
   begin
      if Ref /= Null_Kit_Record_Base_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Top_Record;

   ---------
   -- Get --
   ---------

   function Get (Reference : Kit.Db.Kit_Record_Base_Reference)
      return Kit_Record_Base_Handle is
   begin
      return Handle : Kit_Record_Base_Handle do
         Handle.Reference := Reference;
      end return;
   end Get;

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
      return Kit_Record_Base_Handle is
      use Kit.Db;
      Ref : constant Kit_Record_Base_Reference :=
         Kit.Db.Kit_Record_Base.Get_From_Kit_Root_Record
         (Kit_Root_Record.Reference_Kit_Root_Record);
      Result : Kit_Record_Base_Handle;
   begin
      if Ref /= Null_Kit_Record_Base_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Get_From_Kit_Root_Record;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Item : Cursor) return Boolean is
   begin
      return Kit.Db.Kit_Record_Base.Has_Element (Item.Db);
   end Has_Element;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element (Handle : Kit_Record_Base_Handle)
      return Boolean is
      use type Kit.Db.Kit_Record_Base_Reference;
   begin
      return Handle.Reference /= Kit.Db.Null_Kit_Record_Base_Reference;
   end Has_Element;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Kit_Record_Base_Selection)
      return Boolean is
   begin
      return Kit.Db.Kit_Record_Base.Is_Empty (Container.Db);
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   function Iterate (Container : Kit_Record_Base_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Result : Iterator do
         Result.Container := Container'Unrestricted_Access;
      end return;
   end Iterate;

   ----------------------------
   -- Kit_Root_Record_Handle --
   ----------------------------

   function Kit_Root_Record_Handle (Handle : Kit_Record_Base_Handle)
      return Kit.Handles.Kit_Root_Record.Kit_Root_Record_Handle is
      Rec : constant Kit.Db.Kit_Record_Base.Kit_Record_Base_Type :=
         Kit.Db.Kit_Record_Base.Get (Handle.Reference);
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
         Position.Db := Kit.Db.Kit_Record_Base.Last (It.Container.Db);
      end return;
   end Last;

   ------------------
   -- Last_By_Base --
   ------------------

   function Last_By_Base (Base : Kit.Handles.Kit_Record.Kit_Record_Class)
      return Kit_Record_Base_Handle is
      use Kit.Db;
      Ref : constant Kit_Record_Base_Reference :=
         Kit.Db.Kit_Record_Base.Last_By_Base (Base.Reference_Kit_Record);
      Result : Kit_Record_Base_Handle;
   begin
      if Ref /= Null_Kit_Record_Base_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Base;

   -------------------------
   -- Last_By_Base_Record --
   -------------------------

   function Last_By_Base_Record
     (Base    : Kit.Handles.Kit_Record.Kit_Record_Class;
      Derived : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Record_Base_Handle
   is
      use Kit.Db;
      Ref : constant Kit_Record_Base_Reference :=
         Kit.Db.Kit_Record_Base.Last_By_Base_Record
         (Base.Reference_Kit_Record, Derived.Reference_Kit_Record);
      Result : Kit_Record_Base_Handle;
   begin
      if Ref /= Null_Kit_Record_Base_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Base_Record;

   ---------------------
   -- Last_By_Derived --
   ---------------------

   function Last_By_Derived (Derived :
      Kit.Handles.Kit_Record.Kit_Record_Class)
      return Kit_Record_Base_Handle is
      use Kit.Db;
      Ref : constant Kit_Record_Base_Reference :=
         Kit.Db.Kit_Record_Base.Last_By_Derived
         (Derived.Reference_Kit_Record);
      Result : Kit_Record_Base_Handle;
   begin
      if Ref /= Null_Kit_Record_Base_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Derived;

   ------------------------
   -- Last_By_Top_Record --
   ------------------------

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Record_Base_Handle is
      use Kit.Db;
      Ref : constant Kit_Record_Base_Reference :=
         Kit.Db.Kit_Record_Base.Last_By_Top_Record (Top_Record);
      Result : Kit_Record_Base_Handle;
   begin
      if Ref /= Null_Kit_Record_Base_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Top_Record;

   ------------
   -- Length --
   ------------

   function Length (Container : Kit_Record_Base_Selection) return Natural is
   begin
      return Kit.Db.Kit_Record_Base.Length (Container.Db);
   end Length;

   ----------
   -- Load --
   ----------

   procedure Load
     (Reference : Kit.Db.Kit_Record_Base_Reference;
      Cached    : in out Cached_Handle)
   is
      Rec : constant Kit.Db.Kit_Record_Base.Kit_Record_Base_Type :=
         Kit.Db.Kit_Record_Base.Get (Reference);
   begin
      Cached.Top_Record := Rec.Top_Record;
      Cached.Kit_Base_Kit_Root_Record := Rec.Get_Kit_Root_Record_Reference;
      Cached.Offset := Rec.Offset;
      Cached.Base := Rec.Base;
      Cached.Derived := Rec.Derived;
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
         Result.Db := Kit.Db.Kit_Record_Base.Next (Position.Db);
      end return;
   end Next;

   ------------
   -- Offset --
   ------------

   overriding function Offset (Handle : Kit_Record_Base_Handle)
      return Integer is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Offset;
   end Offset;

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
         Result.Db := Kit.Db.Kit_Record_Base.Previous (Position.Db);
      end return;
   end Previous;

   ---------------
   -- Reference --
   ---------------

   function Reference (Handle : Kit_Record_Base_Handle)
      return Kit.Db.Kit_Record_Base_Reference is
   begin
      return Handle.Reference;
   end Reference;

   -------------------------------
   -- Reference_Kit_Record_Base --
   -------------------------------

   overriding function Reference_Kit_Record_Base (Handle :
      Kit_Record_Base_Handle) return Kit.Db.Kit_Record_Base_Reference is
   begin
      return Handle.Reference;
   end Reference_Kit_Record_Base;

   -------------------------------
   -- Reference_Kit_Root_Record --
   -------------------------------

   overriding function Reference_Kit_Root_Record (Handle :
      Kit_Record_Base_Handle) return Kit.Db.Kit_Root_Record_Reference is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Kit_Base_Kit_Root_Record;
   end Reference_Kit_Root_Record;

   ------------------
   -- Scan_By_Base --
   ------------------

   function Scan_By_Base return Kit_Record_Base_Selection is
      Db_Selection : constant Kit.Db.Kit_Record_Base.Selection :=
         Kit.Db.Kit_Record_Base.Scan_By_Base;
   begin
      return (Db => Db_Selection);
   end Scan_By_Base;

   -------------------------
   -- Scan_By_Base_Record --
   -------------------------

   function Scan_By_Base_Record return Kit_Record_Base_Selection is
      Db_Selection : constant Kit.Db.Kit_Record_Base.Selection :=
         Kit.Db.Kit_Record_Base.Scan_By_Base_Record;
   begin
      return (Db => Db_Selection);
   end Scan_By_Base_Record;

   ---------------------
   -- Scan_By_Derived --
   ---------------------

   function Scan_By_Derived return Kit_Record_Base_Selection is
      Db_Selection : constant Kit.Db.Kit_Record_Base.Selection :=
         Kit.Db.Kit_Record_Base.Scan_By_Derived;
   begin
      return (Db => Db_Selection);
   end Scan_By_Derived;

   ------------------------
   -- Scan_By_Top_Record --
   ------------------------

   function Scan_By_Top_Record return Kit_Record_Base_Selection is
      Db_Selection : constant Kit.Db.Kit_Record_Base.Selection :=
         Kit.Db.Kit_Record_Base.Scan_By_Top_Record;
   begin
      return (Db => Db_Selection);
   end Scan_By_Top_Record;

   ----------------------------------
   -- Select_Bounded_By_Top_Record --
   ----------------------------------

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Record_Base_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Record_Base.Selection :=
         Kit.Db.Kit_Record_Base.Select_Bounded_By_Top_Record
         (Start_Top_Record, Finish_Top_Record);
   begin
      return (Db => Db_Selection);
   end Select_Bounded_By_Top_Record;

   --------------------
   -- Select_By_Base --
   --------------------

   function Select_By_Base (Base : Kit.Handles.Kit_Record.Kit_Record_Class)
      return Kit_Record_Base_Selection is
      Db_Selection : constant Kit.Db.Kit_Record_Base.Selection :=
         Kit.Db.Kit_Record_Base.Select_By_Base (Base.Reference_Kit_Record);
   begin
      return (Db => Db_Selection);
   end Select_By_Base;

   ---------------------------
   -- Select_By_Base_Record --
   ---------------------------

   function Select_By_Base_Record
     (Base    : Kit.Handles.Kit_Record.Kit_Record_Class;
      Derived : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Record_Base_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Record_Base.Selection :=
         Kit.Db.Kit_Record_Base.Select_By_Base_Record
         (Base.Reference_Kit_Record, Derived.Reference_Kit_Record);
   begin
      return (Db => Db_Selection);
   end Select_By_Base_Record;

   -----------------------
   -- Select_By_Derived --
   -----------------------

   function Select_By_Derived (Derived :
      Kit.Handles.Kit_Record.Kit_Record_Class)
      return Kit_Record_Base_Selection is
      Db_Selection : constant Kit.Db.Kit_Record_Base.Selection :=
         Kit.Db.Kit_Record_Base.Select_By_Derived
         (Derived.Reference_Kit_Record);
   begin
      return (Db => Db_Selection);
   end Select_By_Derived;

   --------------------------
   -- Select_By_Top_Record --
   --------------------------

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Record_Base_Selection is
      Db_Selection : constant Kit.Db.Kit_Record_Base.Selection :=
         Kit.Db.Kit_Record_Base.Select_By_Top_Record (Top_Record);
   begin
      return (Db => Db_Selection);
   end Select_By_Top_Record;

   --------------
   -- Set_Base --
   --------------

   function Set_Base
     (Update : Kit_Record_Base_Update_Handle;
      Value  : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Record_Base_Update_Handle
   is
      Change : Kit_Record_Base_Update_Value (Update_Base);
   begin
      Change.Base_Value := Value.Reference_Kit_Record;
      return Result : Kit_Record_Base_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Base;

   -----------------
   -- Set_Derived --
   -----------------

   function Set_Derived
     (Update : Kit_Record_Base_Update_Handle;
      Value  : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Record_Base_Update_Handle
   is
      Change : Kit_Record_Base_Update_Value (Update_Derived);
   begin
      Change.Derived_Value := Value.Reference_Kit_Record;
      return Result : Kit_Record_Base_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Derived;

   ----------------
   -- Set_Offset --
   ----------------

   function Set_Offset
     (Update : Kit_Record_Base_Update_Handle;
      Value  : Integer)
   return Kit_Record_Base_Update_Handle
   is
      Change : Kit_Record_Base_Update_Value (Update_Offset);
   begin
      Change.Offset_Value := Value;
      return Result : Kit_Record_Base_Update_Handle do
         Result.Reference := Update.Reference;
         Result.Updates := Update.Updates;
         Result.Updates.Append (Change);
      end return;
   end Set_Offset;

   -------------------------------
   -- To_Kit_Record_Base_Handle --
   -------------------------------

   function To_Kit_Record_Base_Handle (Class : Kit_Record_Base_Class)
      return Kit_Record_Base_Handle is
   begin
      return Get (Class.Reference_Kit_Record_Base);
   end To_Kit_Record_Base_Handle;

   ----------------
   -- Top_Record --
   ----------------

   overriding function Top_Record (Handle : Kit_Record_Base_Handle)
      return Kit.Db.Record_Type is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Top_Record;
   end Top_Record;

   ------------
   -- Update --
   ------------

   function Update (Handle : Kit_Record_Base_Handle)
      return Kit_Record_Base_Update_Handle'Class is
   begin
      return Update_Kit_Record_Base (Handle.Reference);
   end Update;

   ----------------------------
   -- Update_Kit_Record_Base --
   ----------------------------

   overriding function Update_Kit_Record_Base (Handle :
      Kit_Record_Base_Handle) return Kit_Record_Base_Update_Handle'Class is
   begin
      return Update_Kit_Record_Base (Handle.Reference);
   end Update_Kit_Record_Base;

   ----------------------------
   -- Update_Kit_Record_Base --
   ----------------------------

   function Update_Kit_Record_Base (Target :
      Kit.Db.Kit_Record_Base_Reference)
      return Kit_Record_Base_Update_Handle is
   begin
      return Update : Kit_Record_Base_Update_Handle do
         Update.Reference := Target;
      end return;
   end Update_Kit_Record_Base;

end Kit.Handles.Kit_Record_Base;
