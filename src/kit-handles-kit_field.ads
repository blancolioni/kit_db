with Kit.Db;
with Kit.Handles.Kit_Root_Record;
with Kit.Handles.Kit_Record;
with Kit.Handles.Kit_Type;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Kit.Strings;
with Ada.Iterator_Interfaces;
private with Kit.Db.Kit_Field;

package Kit.Handles.Kit_Field is

   subtype Kit_Root_Record_Class is
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class;
   subtype Kit_Record_Class is Kit.Handles.Kit_Record.Kit_Record_Class;
   subtype Kit_Type_Class is Kit.Handles.Kit_Type.Kit_Type_Class;

   procedure Get_Cache_Statistics
     (Size   :    out Natural;
      Hits   :    out Natural;
      Misses :    out Natural);

   type Kit_Field_Update_Handle is tagged limited private;
   type Kit_Field_Interface is interface
     and Kit_Root_Record.Kit_Root_Record_Interface
     and Handle_Interface;

   subtype Kit_Field_Class is Kit_Field_Interface'Class;

   function Reference_Kit_Field (Handle : Kit_Field_Interface)
      return Kit.Db.Kit_Field_Reference
      is abstract;
   function Update_Kit_Field (Handle : Kit_Field_Interface)
      return Kit_Field_Update_Handle'Class
      is abstract;
   function Name (Handle : Kit_Field_Interface) return String
      is abstract;
   function Kit_Record (Handle : Kit_Field_Interface)
      return Kit.Handles.Kit_Record.Kit_Record_Class
      is abstract;
   function Field_Type (Handle : Kit_Field_Interface)
      return Kit.Handles.Kit_Type.Kit_Type_Class
      is abstract;
   function Field_Offset (Handle : Kit_Field_Interface) return Integer
      is abstract;
   function Field_Length (Handle : Kit_Field_Interface) return Integer
      is abstract;
   function Created (Handle : Kit_Field_Interface) return Boolean
      is abstract;
   function Readable (Handle : Kit_Field_Interface) return Boolean
      is abstract;
   function Writeable (Handle : Kit_Field_Interface) return Boolean
      is abstract;
   function Display (Handle : Kit_Field_Interface) return Boolean
      is abstract;
   function Base_Ref (Handle : Kit_Field_Interface) return Boolean
      is abstract;

   type Kit_Field_Handle is new Kit_Field_Interface with private;

   function Get (Reference : Kit.Db.Kit_Field_Reference)
      return Kit_Field_Handle;

   function Reference (Handle : Kit_Field_Handle)
      return Kit.Db.Kit_Field_Reference;
   overriding function Reference_Kit_Field (Handle : Kit_Field_Handle)
      return Kit.Db.Kit_Field_Reference;
   overriding function Has_Element (Handle : Kit_Field_Handle)
      return Boolean;

   function Kit_Root_Record_Handle (Handle : Kit_Field_Handle)
      return Kit.Handles.Kit_Root_Record.Kit_Root_Record_Handle;
   function To_Kit_Field_Handle (Class : Kit_Field_Class)
      return Kit_Field_Handle;

   function Update_Kit_Field (Target : Kit.Db.Kit_Field_Reference)
      return Kit_Field_Update_Handle;
   procedure Done (Update : Kit_Field_Update_Handle);
   function Set_Name
     (Update : Kit_Field_Update_Handle;
      Value  : String)
   return Kit_Field_Update_Handle;
   function Set_Kit_Record
     (Update : Kit_Field_Update_Handle;
      Value  : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Field_Update_Handle;
   function Set_Field_Type
     (Update : Kit_Field_Update_Handle;
      Value  : Kit.Handles.Kit_Type.Kit_Type_Class)
   return Kit_Field_Update_Handle;
   function Set_Field_Offset
     (Update : Kit_Field_Update_Handle;
      Value  : Integer)
   return Kit_Field_Update_Handle;
   function Set_Field_Length
     (Update : Kit_Field_Update_Handle;
      Value  : Integer)
   return Kit_Field_Update_Handle;
   function Set_Created
     (Update : Kit_Field_Update_Handle;
      Value  : Boolean)
   return Kit_Field_Update_Handle;
   function Set_Readable
     (Update : Kit_Field_Update_Handle;
      Value  : Boolean)
   return Kit_Field_Update_Handle;
   function Set_Writeable
     (Update : Kit_Field_Update_Handle;
      Value  : Boolean)
   return Kit_Field_Update_Handle;
   function Set_Display
     (Update : Kit_Field_Update_Handle;
      Value  : Boolean)
   return Kit_Field_Update_Handle;
   function Set_Base_Ref
     (Update : Kit_Field_Update_Handle;
      Value  : Boolean)
   return Kit_Field_Update_Handle;
   function Update (Handle : Kit_Field_Handle)
      return Kit_Field_Update_Handle'Class;
   overriding function Update_Kit_Field (Handle : Kit_Field_Handle)
      return Kit_Field_Update_Handle'Class;

   overriding function Top_Record (Handle : Kit_Field_Handle)
      return Kit.Db.Record_Type;
   overriding function Reference_Kit_Root_Record (Handle : Kit_Field_Handle)
      return Kit.Db.Kit_Root_Record_Reference;
   overriding function Name (Handle : Kit_Field_Handle) return String;
   overriding function Kit_Record (Handle : Kit_Field_Handle)
      return Kit.Handles.Kit_Record.Kit_Record_Class;
   overriding function Field_Type (Handle : Kit_Field_Handle)
      return Kit.Handles.Kit_Type.Kit_Type_Class;
   overriding function Field_Offset (Handle : Kit_Field_Handle)
      return Integer;
   overriding function Field_Length (Handle : Kit_Field_Handle)
      return Integer;
   overriding function Created (Handle : Kit_Field_Handle) return Boolean;
   overriding function Readable (Handle : Kit_Field_Handle) return Boolean;
   overriding function Writeable (Handle : Kit_Field_Handle) return Boolean;
   overriding function Display (Handle : Kit_Field_Handle) return Boolean;
   overriding function Base_Ref (Handle : Kit_Field_Handle) return Boolean;
   function Empty_Handle return Kit_Field_Handle;

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
   return Kit_Field_Handle;
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
      Base_Ref     : Boolean);
   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Kit_Field_Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Field_Class;
   function Iterate (Container : Kit_Field_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Kit_Field_Selection;
      Position  : Cursor)
   return Kit_Field_Class;
   function Is_Empty (Container : Kit_Field_Selection) return Boolean;
   function Element (Item : Cursor) return Kit_Field_Class;
   function Length (Container : Kit_Field_Selection) return Natural;
   function Scan_By_Top_Record return Kit_Field_Selection;

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Field_Handle;

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Field_Handle;

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Field_Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Field_Selection;

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class)
      return Kit_Field_Handle;

   function Scan_By_Kit_Record return Kit_Field_Selection;

   function First_By_Kit_Record (Kit_Record :
      Kit.Handles.Kit_Record.Kit_Record_Class) return Kit_Field_Handle;

   function Last_By_Kit_Record (Kit_Record :
      Kit.Handles.Kit_Record.Kit_Record_Class) return Kit_Field_Handle;

   function Select_By_Kit_Record (Kit_Record :
      Kit.Handles.Kit_Record.Kit_Record_Class) return Kit_Field_Selection;

   function Scan_By_Record_Field return Kit_Field_Selection;

   function Get_By_Record_Field
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Name       : String)
   return Kit_Field_Handle;

   function Select_By_Record_Field
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Name       : String)
   return Kit_Field_Selection;

   function Select_Record_Field_Bounded_By_Name
     (Kit_Record  : Kit.Handles.Kit_Record.Kit_Record_Class;
      Start_Name  : String;
      Finish_Name : String)
   return Kit_Field_Selection;

   function Is_Record_Field
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Name       : String)
   return Boolean;

   function Scan_By_Display_Field return Kit_Field_Selection;

   function First_By_Display_Field
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Display    : Boolean)
   return Kit_Field_Handle;

   function Last_By_Display_Field
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Display    : Boolean)
   return Kit_Field_Handle;

   function Select_By_Display_Field
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Display    : Boolean)
   return Kit_Field_Selection;

   function Select_Display_Field_Bounded_By_Display
     (Kit_Record     : Kit.Handles.Kit_Record.Kit_Record_Class;
      Start_Display  : Boolean;
      Finish_Display : Boolean)
   return Kit_Field_Selection;

private

   subtype Kit_Root_Record_Reference is Kit.Db.Kit_Root_Record_Reference;
   subtype Kit_Record_Reference is Kit.Db.Kit_Record_Reference;
   subtype Kit_Type_Reference is Kit.Db.Kit_Type_Reference;
   subtype Kit_Field_Reference is Kit.Db.Kit_Field_Reference;
   subtype Record_Type is Kit.Db.Record_Type;

   type Kit_Field_Fields is (Update_Name, Update_Kit_Record,
                             Update_Field_Type, Update_Field_Offset,
                             Update_Field_Length, Update_Created,
                             Update_Readable, Update_Writeable,
                             Update_Display, Update_Base_Ref);
   type Kit_Field_Update_Value (Field : Kit_Field_Fields) is
      record
         case Field is
            when Update_Name =>
               Name_Value         : Kit.Strings.String_Type (64);
            when Update_Kit_Record =>
               Kit_Record_Value   : Kit_Record_Reference;
            when Update_Field_Type =>
               Field_Type_Value   : Kit_Type_Reference;
            when Update_Field_Offset =>
               Field_Offset_Value : Integer;
            when Update_Field_Length =>
               Field_Length_Value : Integer;
            when Update_Created =>
               Created_Value      : Boolean;
            when Update_Readable =>
               Readable_Value     : Boolean;
            when Update_Writeable =>
               Writeable_Value    : Boolean;
            when Update_Display =>
               Display_Value      : Boolean;
            when Update_Base_Ref =>
               Base_Ref_Value     : Boolean;
         end case;
      end record;
   package Update_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
          (Kit_Field_Update_Value);
   type Kit_Field_Update_Handle is tagged limited
      record
         Reference : Kit_Field_Reference;
         Updates   : Update_Lists.List;
      end record;

   type Kit_Field_Handle is new Kit_Field_Interface with
      record
         Reference : Kit.Db.Kit_Field_Reference :=
            Kit.Db.Null_Kit_Field_Reference;
      end record;

   type Cursor is
      record
         Db : Kit.Db.Kit_Field.Cursor;
      end record;
   type Kit_Field_Selection is tagged
      record
         Db : Kit.Db.Kit_Field.Selection;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Handles.Kit_Field;
