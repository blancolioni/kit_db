with Kit.Db;
with Kit.Handles.Kit_Root_Record;
with Kit.Handles.Kit_Type;
with Kit.Handles.Kit_String;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Kit.Strings;
with Ada.Iterator_Interfaces;
private with Kit.Db.Kit_Bounded_String;

package Kit.Handles.Kit_Bounded_String is

   subtype Kit_Root_Record_Class is
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class;
   subtype Kit_Type_Class is Kit.Handles.Kit_Type.Kit_Type_Class;
   subtype Kit_String_Class is Kit.Handles.Kit_String.Kit_String_Class;

   procedure Get_Cache_Statistics
     (Size   :    out Natural;
      Hits   :    out Natural;
      Misses :    out Natural);

   type Kit_Bounded_String_Update_Handle is tagged limited private;
   type Kit_Bounded_String_Interface is interface
     and Kit_Root_Record.Kit_Root_Record_Interface
     and Kit_Type.Kit_Type_Interface
     and Kit_String.Kit_String_Interface
     and Handle_Interface;

   subtype Kit_Bounded_String_Class is Kit_Bounded_String_Interface'Class;

   function Reference_Kit_Bounded_String (Handle :
      Kit_Bounded_String_Interface)
      return Kit.Db.Kit_Bounded_String_Reference
      is abstract;
   function Update_Kit_Bounded_String (Handle : Kit_Bounded_String_Interface)
      return Kit_Bounded_String_Update_Handle'Class
      is abstract;

   type Kit_Bounded_String_Handle is
         new Kit_Bounded_String_Interface with private;

   function Get (Reference : Kit.Db.Kit_Bounded_String_Reference)
      return Kit_Bounded_String_Handle;

   function Reference (Handle : Kit_Bounded_String_Handle)
      return Kit.Db.Kit_Bounded_String_Reference;
   overriding function Reference_Kit_Bounded_String (Handle :
      Kit_Bounded_String_Handle) return Kit.Db.Kit_Bounded_String_Reference;
   overriding function Has_Element (Handle : Kit_Bounded_String_Handle)
      return Boolean;

   function Kit_Root_Record_Handle (Handle : Kit_Bounded_String_Handle)
      return Kit.Handles.Kit_Root_Record.Kit_Root_Record_Handle;
   function Kit_Type_Handle (Handle : Kit_Bounded_String_Handle)
      return Kit.Handles.Kit_Type.Kit_Type_Handle;
   function Kit_String_Handle (Handle : Kit_Bounded_String_Handle)
      return Kit.Handles.Kit_String.Kit_String_Handle;
   function To_Kit_Bounded_String_Handle (Class : Kit_Bounded_String_Class)
      return Kit_Bounded_String_Handle;

   function Update_Kit_Bounded_String (Target :
      Kit.Db.Kit_Bounded_String_Reference)
      return Kit_Bounded_String_Update_Handle;
   procedure Done (Update : Kit_Bounded_String_Update_Handle);
   function Set_Length
     (Update : Kit_Bounded_String_Update_Handle;
      Value  : Integer)
   return Kit_Bounded_String_Update_Handle;
   function Set_Size
     (Update : Kit_Bounded_String_Update_Handle;
      Value  : Integer)
   return Kit_Bounded_String_Update_Handle;
   function Set_Name
     (Update : Kit_Bounded_String_Update_Handle;
      Value  : String)
   return Kit_Bounded_String_Update_Handle;
   function Update (Handle : Kit_Bounded_String_Handle)
      return Kit_Bounded_String_Update_Handle'Class;
   overriding function Update_Kit_Bounded_String (Handle :
      Kit_Bounded_String_Handle)
      return Kit_Bounded_String_Update_Handle'Class;
   overriding function Update_Kit_Type (Handle : Kit_Bounded_String_Handle)
      return Kit_Type.Kit_Type_Update_Handle'Class;
   overriding function Update_Kit_String (Handle : Kit_Bounded_String_Handle)
      return Kit_String.Kit_String_Update_Handle'Class;

   overriding function Top_Record (Handle : Kit_Bounded_String_Handle)
      return Kit.Db.Record_Type;
   overriding function Reference_Kit_Root_Record (Handle :
      Kit_Bounded_String_Handle) return Kit.Db.Kit_Root_Record_Reference;
   overriding function Reference_Kit_Type (Handle :
      Kit_Bounded_String_Handle) return Kit.Db.Kit_Type_Reference;
   overriding function Length (Handle : Kit_Bounded_String_Handle)
      return Integer;
   overriding function Size (Handle : Kit_Bounded_String_Handle)
      return Integer;
   overriding function Name (Handle : Kit_Bounded_String_Handle)
      return String;
   overriding function Reference_Kit_String (Handle :
      Kit_Bounded_String_Handle) return Kit.Db.Kit_String_Reference;
   function Empty_Handle return Kit_Bounded_String_Handle;

   function Create
     (Length : Integer;
      Size   : Integer;
      Name   : String)
   return Kit_Bounded_String_Handle;
   procedure Create
     (Length : Integer;
      Size   : Integer;
      Name   : String);
   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Kit_Bounded_String_Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Bounded_String_Class;
   function Iterate (Container : Kit_Bounded_String_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Kit_Bounded_String_Selection;
      Position  : Cursor)
   return Kit_Bounded_String_Class;
   function Is_Empty (Container : Kit_Bounded_String_Selection)
      return Boolean;
   function Element (Item : Cursor) return Kit_Bounded_String_Class;
   function Length (Container : Kit_Bounded_String_Selection) return Natural;
   function Scan_By_Top_Record return Kit_Bounded_String_Selection;

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Bounded_String_Handle;

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Bounded_String_Handle;

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Bounded_String_Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Bounded_String_Selection;

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class)
      return Kit_Bounded_String_Handle;

   function Scan_By_Name return Kit_Bounded_String_Selection;

   function Get_By_Name (Name : String) return Kit_Bounded_String_Handle;

   function Select_By_Name (Name : String)
      return Kit_Bounded_String_Selection;

   function Select_Bounded_By_Name
     (Start_Name  : String;
      Finish_Name : String)
   return Kit_Bounded_String_Selection;

   function Get_From_Kit_Type (Kit_Type :
      Kit.Handles.Kit_Type.Kit_Type_Class) return Kit_Bounded_String_Handle;

   function Get_From_Kit_String (Kit_String :
      Kit.Handles.Kit_String.Kit_String_Class)
      return Kit_Bounded_String_Handle;

private

   subtype Kit_Root_Record_Reference is Kit.Db.Kit_Root_Record_Reference;
   subtype Kit_Type_Reference is Kit.Db.Kit_Type_Reference;
   subtype Kit_String_Reference is Kit.Db.Kit_String_Reference;
   subtype Kit_Bounded_String_Reference is
      Kit.Db.Kit_Bounded_String_Reference;
   subtype Record_Type is Kit.Db.Record_Type;

   type Kit_Bounded_String_Fields is (Update_Length, Update_Size,
                                      Update_Name);
   type Kit_Bounded_String_Update_Value
      (Field : Kit_Bounded_String_Fields) is
      record
         case Field is
            when Update_Length =>
               Length_Value : Integer;
            when Update_Size =>
               Size_Value   : Integer;
            when Update_Name =>
               Name_Value   : Kit.Strings.String_Type (64);
         end case;
      end record;
   package Update_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
          (Kit_Bounded_String_Update_Value);
   type Kit_Bounded_String_Update_Handle is tagged limited
      record
         Reference : Kit_Bounded_String_Reference;
         Updates   : Update_Lists.List;
      end record;

   type Kit_Bounded_String_Handle is new Kit_Bounded_String_Interface with
      record
         Reference : Kit.Db.Kit_Bounded_String_Reference :=
            Kit.Db.Null_Kit_Bounded_String_Reference;
      end record;

   type Cursor is
      record
         Db : Kit.Db.Kit_Bounded_String.Cursor;
      end record;
   type Kit_Bounded_String_Selection is tagged
      record
         Db : Kit.Db.Kit_Bounded_String.Selection;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Handles.Kit_Bounded_String;
