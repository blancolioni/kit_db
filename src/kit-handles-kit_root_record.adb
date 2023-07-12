with Kit.Protected_Maps;
with Kit.Db.Kit_Root_Record_Hashes;

package body Kit.Handles.Kit_Root_Record is

   type Cached_Handle is
      record
         Top_Record : Record_Type;
      end record;
   procedure Load
     (Reference : Kit.Db.Kit_Root_Record_Reference;
      Cached    : in out Cached_Handle);
   package Cached_Handle_Maps is
     new Kit.Protected_Maps
          (Kit.Db.Kit_Root_Record_Reference,
         Cached_Handle,
         Load,
         Kit.Db.Kit_Root_Record_Hashes.Hash,
         Db."=");
   subtype Constant_Reference_Type is
      Cached_Handle_Maps.Constant_Reference_Type;
   Cache : Cached_Handle_Maps.Map;
   type Iterator is
        new Selection_Iterator_Interfaces.Reversible_Iterator with
      record
         Container : access Kit_Root_Record_Selection;
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
     (Container : aliased Kit_Root_Record_Selection;
      Position  : Cursor)
   return Kit_Root_Record_Class
   is
   begin
      return Get (Kit.Db.Kit_Root_Record.Element (Position.Db));
   end Constant_Reference;

   -------------
   -- Element --
   -------------

   function Element (Item : Cursor) return Kit_Root_Record_Class is
   begin
      return Get (Kit.Db.Kit_Root_Record.Element (Item.Db));
   end Element;

   ------------------
   -- Empty_Handle --
   ------------------

   function Empty_Handle return Kit_Root_Record_Handle is
   begin
      return Get (Kit.Db.Null_Kit_Root_Record_Reference);
   end Empty_Handle;

   -----------
   -- First --
   -----------

   overriding function First (It : Iterator) return Cursor is
   begin
      return Position : Cursor do
         Position.Db := Kit.Db.Kit_Root_Record.First (It.Container.Db);
      end return;
   end First;

   -------------------------
   -- First_By_Top_Record --
   -------------------------

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Root_Record_Handle is
      use Kit.Db;
      Ref : constant Kit_Root_Record_Reference :=
         Kit.Db.Kit_Root_Record.First_By_Top_Record (Top_Record);
      Result : Kit_Root_Record_Handle;
   begin
      if Ref /= Null_Kit_Root_Record_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end First_By_Top_Record;

   ---------
   -- Get --
   ---------

   function Get (Reference : Kit.Db.Kit_Root_Record_Reference)
      return Kit_Root_Record_Handle is
   begin
      return Handle : Kit_Root_Record_Handle do
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

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element (Handle : Kit_Root_Record_Handle)
      return Boolean is
      use type Kit.Db.Kit_Root_Record_Reference;
   begin
      return Handle.Reference /= Kit.Db.Null_Kit_Root_Record_Reference;
   end Has_Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Item : Cursor) return Boolean is
   begin
      return Kit.Db.Kit_Root_Record.Has_Element (Item.Db);
   end Has_Element;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Kit_Root_Record_Selection)
      return Boolean is
   begin
      return Kit.Db.Kit_Root_Record.Is_Empty (Container.Db);
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   function Iterate (Container : Kit_Root_Record_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Result : Iterator do
         Result.Container := Container'Unrestricted_Access;
      end return;
   end Iterate;

   ----------
   -- Last --
   ----------

   overriding function Last (It : Iterator) return Cursor is
   begin
      return Position : Cursor do
         Position.Db := Kit.Db.Kit_Root_Record.Last (It.Container.Db);
      end return;
   end Last;

   ------------------------
   -- Last_By_Top_Record --
   ------------------------

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Root_Record_Handle is
      use Kit.Db;
      Ref : constant Kit_Root_Record_Reference :=
         Kit.Db.Kit_Root_Record.Last_By_Top_Record (Top_Record);
      Result : Kit_Root_Record_Handle;
   begin
      if Ref /= Null_Kit_Root_Record_Reference then
         Result := Get (Ref);
      end if;
      return Result;
   end Last_By_Top_Record;

   ------------
   -- Length --
   ------------

   function Length (Container : Kit_Root_Record_Selection) return Natural is
   begin
      return Kit.Db.Kit_Root_Record.Length (Container.Db);
   end Length;

   ----------
   -- Load --
   ----------

   procedure Load
     (Reference : Kit.Db.Kit_Root_Record_Reference;
      Cached    : in out Cached_Handle)
   is
      Rec : constant Kit.Db.Kit_Root_Record.Kit_Root_Record_Type :=
         Kit.Db.Kit_Root_Record.Get (Reference);
   begin
      Cached.Top_Record := Rec.Top_Record;
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
         Result.Db := Kit.Db.Kit_Root_Record.Next (Position.Db);
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
         Result.Db := Kit.Db.Kit_Root_Record.Previous (Position.Db);
      end return;
   end Previous;

   ---------------
   -- Reference --
   ---------------

   function Reference (Handle : Kit_Root_Record_Handle)
      return Kit.Db.Kit_Root_Record_Reference is
   begin
      return Handle.Reference;
   end Reference;

   -------------------------------
   -- Reference_Kit_Root_Record --
   -------------------------------

   overriding function Reference_Kit_Root_Record (Handle :
      Kit_Root_Record_Handle) return Kit.Db.Kit_Root_Record_Reference is
   begin
      return Handle.Reference;
   end Reference_Kit_Root_Record;

   ------------------------
   -- Scan_By_Top_Record --
   ------------------------

   function Scan_By_Top_Record return Kit_Root_Record_Selection is
      Db_Selection : constant Kit.Db.Kit_Root_Record.Selection :=
         Kit.Db.Kit_Root_Record.Scan_By_Top_Record;
   begin
      return (Db => Db_Selection);
   end Scan_By_Top_Record;

   ----------------------------------
   -- Select_Bounded_By_Top_Record --
   ----------------------------------

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Root_Record_Selection
   is
      Db_Selection : constant Kit.Db.Kit_Root_Record.Selection :=
         Kit.Db.Kit_Root_Record.Select_Bounded_By_Top_Record
         (Start_Top_Record, Finish_Top_Record);
   begin
      return (Db => Db_Selection);
   end Select_Bounded_By_Top_Record;

   --------------------------
   -- Select_By_Top_Record --
   --------------------------

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Root_Record_Selection is
      Db_Selection : constant Kit.Db.Kit_Root_Record.Selection :=
         Kit.Db.Kit_Root_Record.Select_By_Top_Record (Top_Record);
   begin
      return (Db => Db_Selection);
   end Select_By_Top_Record;

   -------------------------------
   -- To_Kit_Root_Record_Handle --
   -------------------------------

   function To_Kit_Root_Record_Handle (Class : Kit_Root_Record_Class)
      return Kit_Root_Record_Handle is
   begin
      return Get (Class.Reference_Kit_Root_Record);
   end To_Kit_Root_Record_Handle;

   ----------------
   -- Top_Record --
   ----------------

   overriding function Top_Record (Handle : Kit_Root_Record_Handle)
      return Kit.Db.Record_Type is
      Rec : constant Constant_Reference_Type := Cache.Constant_Reference
         (Handle.Reference);
   begin
      return Rec.Top_Record;
   end Top_Record;

end Kit.Handles.Kit_Root_Record;
