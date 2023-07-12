with Kit.Db;
with Ada.Iterator_Interfaces;
private with Kit.Db.Kit_Root_Record;

package Kit.Handles.Kit_Root_Record is

   procedure Get_Cache_Statistics
     (Size   :    out Natural;
      Hits   :    out Natural;
      Misses :    out Natural);
   type Kit_Root_Record_Interface is interface
     and Handle_Interface;

   subtype Kit_Root_Record_Class is Kit_Root_Record_Interface'Class;

   function Reference_Kit_Root_Record (Handle : Kit_Root_Record_Interface)
      return Kit.Db.Kit_Root_Record_Reference
      is abstract;
   function Top_Record (Handle : Kit_Root_Record_Interface)
      return Kit.Db.Record_Type
      is abstract;

   type Kit_Root_Record_Handle is new Kit_Root_Record_Interface with private;

   function Get (Reference : Kit.Db.Kit_Root_Record_Reference)
      return Kit_Root_Record_Handle;

   function Reference (Handle : Kit_Root_Record_Handle)
      return Kit.Db.Kit_Root_Record_Reference;
   overriding function Reference_Kit_Root_Record (Handle :
      Kit_Root_Record_Handle) return Kit.Db.Kit_Root_Record_Reference;
   overriding function Has_Element (Handle : Kit_Root_Record_Handle)
      return Boolean;

   function To_Kit_Root_Record_Handle (Class : Kit_Root_Record_Class)
      return Kit_Root_Record_Handle;

   overriding function Top_Record (Handle : Kit_Root_Record_Handle)
      return Kit.Db.Record_Type;
   function Empty_Handle return Kit_Root_Record_Handle;

   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Kit_Root_Record_Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Root_Record_Class;
   function Iterate (Container : Kit_Root_Record_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Kit_Root_Record_Selection;
      Position  : Cursor)
   return Kit_Root_Record_Class;
   function Is_Empty (Container : Kit_Root_Record_Selection) return Boolean;
   function Element (Item : Cursor) return Kit_Root_Record_Class;
   function Length (Container : Kit_Root_Record_Selection) return Natural;
   function Scan_By_Top_Record return Kit_Root_Record_Selection;

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Root_Record_Handle;

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Root_Record_Handle;

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Root_Record_Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Root_Record_Selection;

private

   subtype Kit_Root_Record_Reference is Kit.Db.Kit_Root_Record_Reference;
   subtype Record_Type is Kit.Db.Record_Type;

   type Kit_Root_Record_Handle is new Kit_Root_Record_Interface with
      record
         Reference : Kit.Db.Kit_Root_Record_Reference :=
            Kit.Db.Null_Kit_Root_Record_Reference;
      end record;

   type Cursor is
      record
         Db : Kit.Db.Kit_Root_Record.Cursor;
      end record;
   type Kit_Root_Record_Selection is tagged
      record
         Db : Kit.Db.Kit_Root_Record.Selection;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Handles.Kit_Root_Record;
