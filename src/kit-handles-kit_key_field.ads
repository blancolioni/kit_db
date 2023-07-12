with Kit.Db;
with Kit.Handles.Kit_Root_Record;
with Kit.Handles.Kit_Key;
with Kit.Handles.Kit_Field;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;
private with Kit.Db.Kit_Key_Field;

package Kit.Handles.Kit_Key_Field is

   subtype Kit_Root_Record_Class is
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class;
   subtype Kit_Key_Class is Kit.Handles.Kit_Key.Kit_Key_Class;
   subtype Kit_Field_Class is Kit.Handles.Kit_Field.Kit_Field_Class;

   procedure Get_Cache_Statistics
     (Size   :    out Natural;
      Hits   :    out Natural;
      Misses :    out Natural);

   type Kit_Key_Field_Update_Handle is tagged limited private;
   type Kit_Key_Field_Interface is interface
     and Kit_Root_Record.Kit_Root_Record_Interface
     and Handle_Interface;

   subtype Kit_Key_Field_Class is Kit_Key_Field_Interface'Class;

   function Reference_Kit_Key_Field (Handle : Kit_Key_Field_Interface)
      return Kit.Db.Kit_Key_Field_Reference
      is abstract;
   function Update_Kit_Key_Field (Handle : Kit_Key_Field_Interface)
      return Kit_Key_Field_Update_Handle'Class
      is abstract;
   function Kit_Key (Handle : Kit_Key_Field_Interface)
      return Kit.Handles.Kit_Key.Kit_Key_Class
      is abstract;
   function Kit_Field (Handle : Kit_Key_Field_Interface)
      return Kit.Handles.Kit_Field.Kit_Field_Class
      is abstract;

   type Kit_Key_Field_Handle is new Kit_Key_Field_Interface with private;

   function Get (Reference : Kit.Db.Kit_Key_Field_Reference)
      return Kit_Key_Field_Handle;

   function Reference (Handle : Kit_Key_Field_Handle)
      return Kit.Db.Kit_Key_Field_Reference;
   overriding function Reference_Kit_Key_Field (Handle :
      Kit_Key_Field_Handle) return Kit.Db.Kit_Key_Field_Reference;
   overriding function Has_Element (Handle : Kit_Key_Field_Handle)
      return Boolean;

   function Kit_Root_Record_Handle (Handle : Kit_Key_Field_Handle)
      return Kit.Handles.Kit_Root_Record.Kit_Root_Record_Handle;
   function To_Kit_Key_Field_Handle (Class : Kit_Key_Field_Class)
      return Kit_Key_Field_Handle;

   function Update_Kit_Key_Field (Target : Kit.Db.Kit_Key_Field_Reference)
      return Kit_Key_Field_Update_Handle;
   procedure Done (Update : Kit_Key_Field_Update_Handle);
   function Set_Kit_Key
     (Update : Kit_Key_Field_Update_Handle;
      Value  : Kit.Handles.Kit_Key.Kit_Key_Class)
   return Kit_Key_Field_Update_Handle;
   function Set_Kit_Field
     (Update : Kit_Key_Field_Update_Handle;
      Value  : Kit.Handles.Kit_Field.Kit_Field_Class)
   return Kit_Key_Field_Update_Handle;
   function Update (Handle : Kit_Key_Field_Handle)
      return Kit_Key_Field_Update_Handle'Class;
   overriding function Update_Kit_Key_Field (Handle : Kit_Key_Field_Handle)
      return Kit_Key_Field_Update_Handle'Class;

   overriding function Top_Record (Handle : Kit_Key_Field_Handle)
      return Kit.Db.Record_Type;
   overriding function Reference_Kit_Root_Record (Handle :
      Kit_Key_Field_Handle) return Kit.Db.Kit_Root_Record_Reference;
   overriding function Kit_Key (Handle : Kit_Key_Field_Handle)
      return Kit.Handles.Kit_Key.Kit_Key_Class;
   overriding function Kit_Field (Handle : Kit_Key_Field_Handle)
      return Kit.Handles.Kit_Field.Kit_Field_Class;
   function Empty_Handle return Kit_Key_Field_Handle;

   function Create
     (Kit_Key   : Kit.Handles.Kit_Key.Kit_Key_Class;
      Kit_Field : Kit.Handles.Kit_Field.Kit_Field_Class)
   return Kit_Key_Field_Handle;
   procedure Create
     (Kit_Key   : Kit.Handles.Kit_Key.Kit_Key_Class;
      Kit_Field : Kit.Handles.Kit_Field.Kit_Field_Class);
   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Kit_Key_Field_Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Key_Field_Class;
   function Iterate (Container : Kit_Key_Field_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Kit_Key_Field_Selection;
      Position  : Cursor)
   return Kit_Key_Field_Class;
   function Is_Empty (Container : Kit_Key_Field_Selection) return Boolean;
   function Element (Item : Cursor) return Kit_Key_Field_Class;
   function Length (Container : Kit_Key_Field_Selection) return Natural;
   function Scan_By_Top_Record return Kit_Key_Field_Selection;

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Key_Field_Handle;

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Key_Field_Handle;

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Key_Field_Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Key_Field_Selection;

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class)
      return Kit_Key_Field_Handle;

   function Scan_By_Kit_Key return Kit_Key_Field_Selection;

   function First_By_Kit_Key (Kit_Key : Kit.Handles.Kit_Key.Kit_Key_Class)
      return Kit_Key_Field_Handle;

   function Last_By_Kit_Key (Kit_Key : Kit.Handles.Kit_Key.Kit_Key_Class)
      return Kit_Key_Field_Handle;

   function Select_By_Kit_Key (Kit_Key : Kit.Handles.Kit_Key.Kit_Key_Class)
      return Kit_Key_Field_Selection;

   function Scan_By_Kit_Field return Kit_Key_Field_Selection;

   function First_By_Kit_Field (Kit_Field :
      Kit.Handles.Kit_Field.Kit_Field_Class) return Kit_Key_Field_Handle;

   function Last_By_Kit_Field (Kit_Field :
      Kit.Handles.Kit_Field.Kit_Field_Class) return Kit_Key_Field_Handle;

   function Select_By_Kit_Field (Kit_Field :
      Kit.Handles.Kit_Field.Kit_Field_Class) return Kit_Key_Field_Selection;

   function Scan_By_Key_Field return Kit_Key_Field_Selection;

   function Get_By_Key_Field
     (Kit_Key   : Kit.Handles.Kit_Key.Kit_Key_Class;
      Kit_Field : Kit.Handles.Kit_Field.Kit_Field_Class)
   return Kit_Key_Field_Handle;

   function Select_By_Key_Field
     (Kit_Key   : Kit.Handles.Kit_Key.Kit_Key_Class;
      Kit_Field : Kit.Handles.Kit_Field.Kit_Field_Class)
   return Kit_Key_Field_Selection;

   function Is_Key_Field
     (Kit_Key   : Kit.Handles.Kit_Key.Kit_Key_Class;
      Kit_Field : Kit.Handles.Kit_Field.Kit_Field_Class)
   return Boolean;

private

   subtype Kit_Root_Record_Reference is Kit.Db.Kit_Root_Record_Reference;
   subtype Kit_Key_Reference is Kit.Db.Kit_Key_Reference;
   subtype Kit_Field_Reference is Kit.Db.Kit_Field_Reference;
   subtype Kit_Key_Field_Reference is Kit.Db.Kit_Key_Field_Reference;
   subtype Record_Type is Kit.Db.Record_Type;

   type Kit_Key_Field_Fields is (Update_Kit_Key, Update_Kit_Field);
   type Kit_Key_Field_Update_Value (Field : Kit_Key_Field_Fields) is
      record
         case Field is
            when Update_Kit_Key =>
               Kit_Key_Value   : Kit_Key_Reference;
            when Update_Kit_Field =>
               Kit_Field_Value : Kit_Field_Reference;
         end case;
      end record;
   package Update_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
          (Kit_Key_Field_Update_Value);
   type Kit_Key_Field_Update_Handle is tagged limited
      record
         Reference : Kit_Key_Field_Reference;
         Updates   : Update_Lists.List;
      end record;

   type Kit_Key_Field_Handle is new Kit_Key_Field_Interface with
      record
         Reference : Kit.Db.Kit_Key_Field_Reference :=
            Kit.Db.Null_Kit_Key_Field_Reference;
      end record;

   type Cursor is
      record
         Db : Kit.Db.Kit_Key_Field.Cursor;
      end record;
   type Kit_Key_Field_Selection is tagged
      record
         Db : Kit.Db.Kit_Key_Field.Selection;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Handles.Kit_Key_Field;
