with Kit.Db;
with Kit.Handles.Kit_Root_Record;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Kit.Strings;
with Ada.Iterator_Interfaces;
private with Kit.Db.Kit_Record;

package Kit.Handles.Kit_Record is

   subtype Kit_Root_Record_Class is
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class;

   procedure Get_Cache_Statistics
     (Size   :    out Natural;
      Hits   :    out Natural;
      Misses :    out Natural);

   type Kit_Record_Update_Handle is tagged limited private;
   type Kit_Record_Interface is interface
     and Kit_Root_Record.Kit_Root_Record_Interface
     and Handle_Interface;

   subtype Kit_Record_Class is Kit_Record_Interface'Class;

   function Reference_Kit_Record (Handle : Kit_Record_Interface)
      return Kit.Db.Kit_Record_Reference
      is abstract;
   function Update_Kit_Record (Handle : Kit_Record_Interface)
      return Kit_Record_Update_Handle'Class
      is abstract;
   function Name (Handle : Kit_Record_Interface) return String
      is abstract;
   function Table_Index (Handle : Kit_Record_Interface) return Integer
      is abstract;
   function Record_Length (Handle : Kit_Record_Interface) return Integer
      is abstract;

   type Kit_Record_Handle is new Kit_Record_Interface with private;

   function Get (Reference : Kit.Db.Kit_Record_Reference)
      return Kit_Record_Handle;

   function Reference (Handle : Kit_Record_Handle)
      return Kit.Db.Kit_Record_Reference;
   overriding function Reference_Kit_Record (Handle : Kit_Record_Handle)
      return Kit.Db.Kit_Record_Reference;
   overriding function Has_Element (Handle : Kit_Record_Handle)
      return Boolean;

   function Kit_Root_Record_Handle (Handle : Kit_Record_Handle)
      return Kit.Handles.Kit_Root_Record.Kit_Root_Record_Handle;
   function To_Kit_Record_Handle (Class : Kit_Record_Class)
      return Kit_Record_Handle;

   function Update_Kit_Record (Target : Kit.Db.Kit_Record_Reference)
      return Kit_Record_Update_Handle;
   procedure Done (Update : Kit_Record_Update_Handle);
   function Set_Name
     (Update : Kit_Record_Update_Handle;
      Value  : String)
   return Kit_Record_Update_Handle;
   function Set_Table_Index
     (Update : Kit_Record_Update_Handle;
      Value  : Integer)
   return Kit_Record_Update_Handle;
   function Set_Record_Length
     (Update : Kit_Record_Update_Handle;
      Value  : Integer)
   return Kit_Record_Update_Handle;
   function Update (Handle : Kit_Record_Handle)
      return Kit_Record_Update_Handle'Class;
   overriding function Update_Kit_Record (Handle : Kit_Record_Handle)
      return Kit_Record_Update_Handle'Class;

   overriding function Top_Record (Handle : Kit_Record_Handle)
      return Kit.Db.Record_Type;
   overriding function Reference_Kit_Root_Record (Handle : Kit_Record_Handle)
      return Kit.Db.Kit_Root_Record_Reference;
   overriding function Name (Handle : Kit_Record_Handle) return String;
   overriding function Table_Index (Handle : Kit_Record_Handle)
      return Integer;
   overriding function Record_Length (Handle : Kit_Record_Handle)
      return Integer;
   function Empty_Handle return Kit_Record_Handle;

   function Create
     (Name          : String;
      Table_Index   : Integer;
      Record_Length : Integer)
   return Kit_Record_Handle;
   procedure Create
     (Name          : String;
      Table_Index   : Integer;
      Record_Length : Integer);
   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Kit_Record_Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Record_Class;
   function Iterate (Container : Kit_Record_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Kit_Record_Selection;
      Position  : Cursor)
   return Kit_Record_Class;
   function Is_Empty (Container : Kit_Record_Selection) return Boolean;
   function Element (Item : Cursor) return Kit_Record_Class;
   function Length (Container : Kit_Record_Selection) return Natural;
   function Scan_By_Top_Record return Kit_Record_Selection;

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Record_Handle;

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Record_Handle;

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Record_Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Record_Selection;

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class)
      return Kit_Record_Handle;

   function Scan_By_Name return Kit_Record_Selection;

   function Get_By_Name (Name : String) return Kit_Record_Handle;

   function Select_By_Name (Name : String) return Kit_Record_Selection;

   function Select_Bounded_By_Name
     (Start_Name  : String;
      Finish_Name : String)
   return Kit_Record_Selection;

   function Is_Name (Name : String) return Boolean;

   function Scan_By_Table_Index return Kit_Record_Selection;

   function Get_By_Table_Index (Table_Index : Integer)
      return Kit_Record_Handle;

   function Select_By_Table_Index (Table_Index : Integer)
      return Kit_Record_Selection;

   function Select_Bounded_By_Table_Index
     (Start_Table_Index  : Integer;
      Finish_Table_Index : Integer)
   return Kit_Record_Selection;

   function Is_Table_Index (Table_Index : Integer) return Boolean;

private

   subtype Kit_Root_Record_Reference is Kit.Db.Kit_Root_Record_Reference;
   subtype Kit_Record_Reference is Kit.Db.Kit_Record_Reference;
   subtype Record_Type is Kit.Db.Record_Type;

   type Kit_Record_Fields is (Update_Name, Update_Table_Index,
                              Update_Record_Length);
   type Kit_Record_Update_Value (Field : Kit_Record_Fields) is
      record
         case Field is
            when Update_Name =>
               Name_Value          : Kit.Strings.String_Type (64);
            when Update_Table_Index =>
               Table_Index_Value   : Integer;
            when Update_Record_Length =>
               Record_Length_Value : Integer;
         end case;
      end record;
   package Update_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
          (Kit_Record_Update_Value);
   type Kit_Record_Update_Handle is tagged limited
      record
         Reference : Kit_Record_Reference;
         Updates   : Update_Lists.List;
      end record;

   type Kit_Record_Handle is new Kit_Record_Interface with
      record
         Reference : Kit.Db.Kit_Record_Reference :=
            Kit.Db.Null_Kit_Record_Reference;
      end record;

   type Cursor is
      record
         Db : Kit.Db.Kit_Record.Cursor;
      end record;
   type Kit_Record_Selection is tagged
      record
         Db : Kit.Db.Kit_Record.Selection;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Handles.Kit_Record;
