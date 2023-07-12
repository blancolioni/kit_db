with Kit.Db;
with Kit.Handles.Kit_Root_Record;
with Kit.Handles.Kit_Record;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;
private with Kit.Db.Kit_Record_Base;

package Kit.Handles.Kit_Record_Base is

   subtype Kit_Root_Record_Class is
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class;
   subtype Kit_Record_Class is Kit.Handles.Kit_Record.Kit_Record_Class;

   procedure Get_Cache_Statistics
     (Size   :    out Natural;
      Hits   :    out Natural;
      Misses :    out Natural);

   type Kit_Record_Base_Update_Handle is tagged limited private;
   type Kit_Record_Base_Interface is interface
     and Kit_Root_Record.Kit_Root_Record_Interface
     and Handle_Interface;

   subtype Kit_Record_Base_Class is Kit_Record_Base_Interface'Class;

   function Reference_Kit_Record_Base (Handle : Kit_Record_Base_Interface)
      return Kit.Db.Kit_Record_Base_Reference
      is abstract;
   function Update_Kit_Record_Base (Handle : Kit_Record_Base_Interface)
      return Kit_Record_Base_Update_Handle'Class
      is abstract;
   function Offset (Handle : Kit_Record_Base_Interface) return Integer
      is abstract;
   function Base (Handle : Kit_Record_Base_Interface)
      return Kit.Handles.Kit_Record.Kit_Record_Class
      is abstract;
   function Derived (Handle : Kit_Record_Base_Interface)
      return Kit.Handles.Kit_Record.Kit_Record_Class
      is abstract;

   type Kit_Record_Base_Handle is new Kit_Record_Base_Interface with private;

   function Get (Reference : Kit.Db.Kit_Record_Base_Reference)
      return Kit_Record_Base_Handle;

   function Reference (Handle : Kit_Record_Base_Handle)
      return Kit.Db.Kit_Record_Base_Reference;
   overriding function Reference_Kit_Record_Base (Handle :
      Kit_Record_Base_Handle) return Kit.Db.Kit_Record_Base_Reference;
   overriding function Has_Element (Handle : Kit_Record_Base_Handle)
      return Boolean;

   function Kit_Root_Record_Handle (Handle : Kit_Record_Base_Handle)
      return Kit.Handles.Kit_Root_Record.Kit_Root_Record_Handle;
   function To_Kit_Record_Base_Handle (Class : Kit_Record_Base_Class)
      return Kit_Record_Base_Handle;

   function Update_Kit_Record_Base (Target :
      Kit.Db.Kit_Record_Base_Reference) return Kit_Record_Base_Update_Handle;
   procedure Done (Update : Kit_Record_Base_Update_Handle);
   function Set_Offset
     (Update : Kit_Record_Base_Update_Handle;
      Value  : Integer)
   return Kit_Record_Base_Update_Handle;
   function Set_Base
     (Update : Kit_Record_Base_Update_Handle;
      Value  : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Record_Base_Update_Handle;
   function Set_Derived
     (Update : Kit_Record_Base_Update_Handle;
      Value  : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Record_Base_Update_Handle;
   function Update (Handle : Kit_Record_Base_Handle)
      return Kit_Record_Base_Update_Handle'Class;
   overriding function Update_Kit_Record_Base (Handle :
      Kit_Record_Base_Handle) return Kit_Record_Base_Update_Handle'Class;

   overriding function Top_Record (Handle : Kit_Record_Base_Handle)
      return Kit.Db.Record_Type;
   overriding function Reference_Kit_Root_Record (Handle :
      Kit_Record_Base_Handle) return Kit.Db.Kit_Root_Record_Reference;
   overriding function Offset (Handle : Kit_Record_Base_Handle)
      return Integer;
   overriding function Base (Handle : Kit_Record_Base_Handle)
      return Kit.Handles.Kit_Record.Kit_Record_Class;
   overriding function Derived (Handle : Kit_Record_Base_Handle)
      return Kit.Handles.Kit_Record.Kit_Record_Class;
   function Empty_Handle return Kit_Record_Base_Handle;

   function Create
     (Offset  : Integer;
      Base    : Kit.Handles.Kit_Record.Kit_Record_Class;
      Derived : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Record_Base_Handle;
   procedure Create
     (Offset  : Integer;
      Base    : Kit.Handles.Kit_Record.Kit_Record_Class;
      Derived : Kit.Handles.Kit_Record.Kit_Record_Class);
   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Kit_Record_Base_Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Record_Base_Class;
   function Iterate (Container : Kit_Record_Base_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Kit_Record_Base_Selection;
      Position  : Cursor)
   return Kit_Record_Base_Class;
   function Is_Empty (Container : Kit_Record_Base_Selection) return Boolean;
   function Element (Item : Cursor) return Kit_Record_Base_Class;
   function Length (Container : Kit_Record_Base_Selection) return Natural;
   function Scan_By_Top_Record return Kit_Record_Base_Selection;

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Record_Base_Handle;

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Record_Base_Handle;

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Record_Base_Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Record_Base_Selection;

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class)
      return Kit_Record_Base_Handle;

   function Scan_By_Base return Kit_Record_Base_Selection;

   function First_By_Base (Base : Kit.Handles.Kit_Record.Kit_Record_Class)
      return Kit_Record_Base_Handle;

   function Last_By_Base (Base : Kit.Handles.Kit_Record.Kit_Record_Class)
      return Kit_Record_Base_Handle;

   function Select_By_Base (Base : Kit.Handles.Kit_Record.Kit_Record_Class)
      return Kit_Record_Base_Selection;

   function Scan_By_Derived return Kit_Record_Base_Selection;

   function First_By_Derived (Derived :
      Kit.Handles.Kit_Record.Kit_Record_Class) return Kit_Record_Base_Handle;

   function Last_By_Derived (Derived :
      Kit.Handles.Kit_Record.Kit_Record_Class) return Kit_Record_Base_Handle;

   function Select_By_Derived (Derived :
      Kit.Handles.Kit_Record.Kit_Record_Class)
      return Kit_Record_Base_Selection;

   function Scan_By_Base_Record return Kit_Record_Base_Selection;

   function First_By_Base_Record
     (Base    : Kit.Handles.Kit_Record.Kit_Record_Class;
      Derived : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Record_Base_Handle;

   function Last_By_Base_Record
     (Base    : Kit.Handles.Kit_Record.Kit_Record_Class;
      Derived : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Record_Base_Handle;

   function Select_By_Base_Record
     (Base    : Kit.Handles.Kit_Record.Kit_Record_Class;
      Derived : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Record_Base_Selection;

private

   subtype Kit_Root_Record_Reference is Kit.Db.Kit_Root_Record_Reference;
   subtype Kit_Record_Reference is Kit.Db.Kit_Record_Reference;
   subtype Kit_Record_Base_Reference is Kit.Db.Kit_Record_Base_Reference;
   subtype Record_Type is Kit.Db.Record_Type;

   type Kit_Record_Base_Fields is (Update_Offset, Update_Base,
                                   Update_Derived);
   type Kit_Record_Base_Update_Value (Field : Kit_Record_Base_Fields) is
      record
         case Field is
            when Update_Offset =>
               Offset_Value  : Integer;
            when Update_Base =>
               Base_Value    : Kit_Record_Reference;
            when Update_Derived =>
               Derived_Value : Kit_Record_Reference;
         end case;
      end record;
   package Update_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
          (Kit_Record_Base_Update_Value);
   type Kit_Record_Base_Update_Handle is tagged limited
      record
         Reference : Kit_Record_Base_Reference;
         Updates   : Update_Lists.List;
      end record;

   type Kit_Record_Base_Handle is new Kit_Record_Base_Interface with
      record
         Reference : Kit.Db.Kit_Record_Base_Reference :=
            Kit.Db.Null_Kit_Record_Base_Reference;
      end record;

   type Cursor is
      record
         Db : Kit.Db.Kit_Record_Base.Cursor;
      end record;
   type Kit_Record_Base_Selection is tagged
      record
         Db : Kit.Db.Kit_Record_Base.Selection;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Handles.Kit_Record_Base;
