with Kit.Db;
with Kit.Handles.Kit_Root_Record;
with Kit.Handles.Kit_Enumeration;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Kit.Strings;
with Ada.Iterator_Interfaces;
private with Kit.Db.Kit_Literal;

package Kit.Handles.Kit_Literal is

   subtype Kit_Root_Record_Class is
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class;
   subtype Kit_Enumeration_Class is
      Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;

   procedure Get_Cache_Statistics
     (Size   :    out Natural;
      Hits   :    out Natural;
      Misses :    out Natural);

   type Kit_Literal_Update_Handle is tagged limited private;
   type Kit_Literal_Interface is interface
     and Kit_Root_Record.Kit_Root_Record_Interface
     and Handle_Interface;

   subtype Kit_Literal_Class is Kit_Literal_Interface'Class;

   function Reference_Kit_Literal (Handle : Kit_Literal_Interface)
      return Kit.Db.Kit_Literal_Reference
      is abstract;
   function Update_Kit_Literal (Handle : Kit_Literal_Interface)
      return Kit_Literal_Update_Handle'Class
      is abstract;
   function Name (Handle : Kit_Literal_Interface) return String
      is abstract;
   function Kit_Enumeration (Handle : Kit_Literal_Interface)
      return Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class
      is abstract;
   function Value (Handle : Kit_Literal_Interface) return Integer
      is abstract;

   type Kit_Literal_Handle is new Kit_Literal_Interface with private;

   function Get (Reference : Kit.Db.Kit_Literal_Reference)
      return Kit_Literal_Handle;

   function Reference (Handle : Kit_Literal_Handle)
      return Kit.Db.Kit_Literal_Reference;
   overriding function Reference_Kit_Literal (Handle : Kit_Literal_Handle)
      return Kit.Db.Kit_Literal_Reference;
   overriding function Has_Element (Handle : Kit_Literal_Handle)
      return Boolean;

   function Kit_Root_Record_Handle (Handle : Kit_Literal_Handle)
      return Kit.Handles.Kit_Root_Record.Kit_Root_Record_Handle;
   function To_Kit_Literal_Handle (Class : Kit_Literal_Class)
      return Kit_Literal_Handle;

   function Update_Kit_Literal (Target : Kit.Db.Kit_Literal_Reference)
      return Kit_Literal_Update_Handle;
   procedure Done (Update : Kit_Literal_Update_Handle);
   function Set_Name
     (Update : Kit_Literal_Update_Handle;
      Value  : String)
   return Kit_Literal_Update_Handle;
   function Set_Kit_Enumeration
     (Update : Kit_Literal_Update_Handle;
      Value  : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class)
   return Kit_Literal_Update_Handle;
   function Set_Value
     (Update : Kit_Literal_Update_Handle;
      Value  : Integer)
   return Kit_Literal_Update_Handle;
   function Update (Handle : Kit_Literal_Handle)
      return Kit_Literal_Update_Handle'Class;
   overriding function Update_Kit_Literal (Handle : Kit_Literal_Handle)
      return Kit_Literal_Update_Handle'Class;

   overriding function Top_Record (Handle : Kit_Literal_Handle)
      return Kit.Db.Record_Type;
   overriding function Reference_Kit_Root_Record (Handle :
      Kit_Literal_Handle) return Kit.Db.Kit_Root_Record_Reference;
   overriding function Name (Handle : Kit_Literal_Handle) return String;
   overriding function Kit_Enumeration (Handle : Kit_Literal_Handle)
      return Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
   overriding function Value (Handle : Kit_Literal_Handle) return Integer;
   function Empty_Handle return Kit_Literal_Handle;

   function Create
     (Name            : String;
      Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Value           : Integer)
   return Kit_Literal_Handle;
   procedure Create
     (Name            : String;
      Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Value           : Integer);
   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Kit_Literal_Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Literal_Class;
   function Iterate (Container : Kit_Literal_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Kit_Literal_Selection;
      Position  : Cursor)
   return Kit_Literal_Class;
   function Is_Empty (Container : Kit_Literal_Selection) return Boolean;
   function Element (Item : Cursor) return Kit_Literal_Class;
   function Length (Container : Kit_Literal_Selection) return Natural;
   function Scan_By_Top_Record return Kit_Literal_Selection;

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Literal_Handle;

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Literal_Handle;

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Literal_Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Literal_Selection;

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class)
      return Kit_Literal_Handle;

   function Scan_By_Kit_Enumeration return Kit_Literal_Selection;

   function First_By_Kit_Enumeration (Kit_Enumeration :
      Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class)
      return Kit_Literal_Handle;

   function Last_By_Kit_Enumeration (Kit_Enumeration :
      Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class)
      return Kit_Literal_Handle;

   function Select_By_Kit_Enumeration (Kit_Enumeration :
      Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class)
      return Kit_Literal_Selection;

   function Scan_By_Value return Kit_Literal_Selection;

   function First_By_Value (Value : Integer) return Kit_Literal_Handle;

   function Last_By_Value (Value : Integer) return Kit_Literal_Handle;

   function Select_By_Value (Value : Integer) return Kit_Literal_Selection;

   function Select_Bounded_By_Value
     (Start_Value  : Integer;
      Finish_Value : Integer)
   return Kit_Literal_Selection;

   function Scan_By_Enum_Value return Kit_Literal_Selection;

   function Get_By_Enum_Value
     (Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Value           : Integer)
   return Kit_Literal_Handle;

   function Select_By_Enum_Value
     (Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Value           : Integer)
   return Kit_Literal_Selection;

   function Select_Enum_Value_Bounded_By_Value
     (Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Start_Value     : Integer;
      Finish_Value    : Integer)
   return Kit_Literal_Selection;

   function Is_Enum_Value
     (Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Value           : Integer)
   return Boolean;

   function Scan_By_Enum_Name return Kit_Literal_Selection;

   function Get_By_Enum_Name
     (Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Name            : String)
   return Kit_Literal_Handle;

   function Select_By_Enum_Name
     (Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Name            : String)
   return Kit_Literal_Selection;

   function Select_Enum_Name_Bounded_By_Name
     (Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Start_Name      : String;
      Finish_Name     : String)
   return Kit_Literal_Selection;

   function Is_Enum_Name
     (Kit_Enumeration : Kit.Handles.Kit_Enumeration.Kit_Enumeration_Class;
      Name            : String)
   return Boolean;

private

   subtype Kit_Root_Record_Reference is Kit.Db.Kit_Root_Record_Reference;
   subtype Kit_Enumeration_Reference is Kit.Db.Kit_Enumeration_Reference;
   subtype Kit_Literal_Reference is Kit.Db.Kit_Literal_Reference;
   subtype Record_Type is Kit.Db.Record_Type;

   type Kit_Literal_Fields is (Update_Name, Update_Kit_Enumeration,
                               Update_Value);
   type Kit_Literal_Update_Value (Field : Kit_Literal_Fields) is
      record
         case Field is
            when Update_Name =>
               Name_Value            : Kit.Strings.String_Type (64);
            when Update_Kit_Enumeration =>
               Kit_Enumeration_Value : Kit_Enumeration_Reference;
            when Update_Value =>
               Value_Value           : Integer;
         end case;
      end record;
   package Update_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
          (Kit_Literal_Update_Value);
   type Kit_Literal_Update_Handle is tagged limited
      record
         Reference : Kit_Literal_Reference;
         Updates   : Update_Lists.List;
      end record;

   type Kit_Literal_Handle is new Kit_Literal_Interface with
      record
         Reference : Kit.Db.Kit_Literal_Reference :=
            Kit.Db.Null_Kit_Literal_Reference;
      end record;

   type Cursor is
      record
         Db : Kit.Db.Kit_Literal.Cursor;
      end record;
   type Kit_Literal_Selection is tagged
      record
         Db : Kit.Db.Kit_Literal.Selection;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Handles.Kit_Literal;
