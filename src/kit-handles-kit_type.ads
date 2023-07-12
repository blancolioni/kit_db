with Kit.Db;
with Kit.Handles.Kit_Root_Record;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Kit.Strings;
with Ada.Iterator_Interfaces;
private with Kit.Db.Kit_Type;

package Kit.Handles.Kit_Type is

   subtype Kit_Root_Record_Class is
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class;

   procedure Get_Cache_Statistics
     (Size   :    out Natural;
      Hits   :    out Natural;
      Misses :    out Natural);

   type Kit_Type_Update_Handle is tagged limited private;
   type Kit_Type_Interface is interface
     and Kit_Root_Record.Kit_Root_Record_Interface
     and Handle_Interface;

   subtype Kit_Type_Class is Kit_Type_Interface'Class;

   function Reference_Kit_Type (Handle : Kit_Type_Interface)
      return Kit.Db.Kit_Type_Reference
      is abstract;
   function Update_Kit_Type (Handle : Kit_Type_Interface)
      return Kit_Type_Update_Handle'Class
      is abstract;
   function Size (Handle : Kit_Type_Interface) return Integer
      is abstract;
   function Name (Handle : Kit_Type_Interface) return String
      is abstract;

   type Kit_Type_Handle is new Kit_Type_Interface with private;

   function Get (Reference : Kit.Db.Kit_Type_Reference)
      return Kit_Type_Handle;

   function Reference (Handle : Kit_Type_Handle)
      return Kit.Db.Kit_Type_Reference;
   overriding function Reference_Kit_Type (Handle : Kit_Type_Handle)
      return Kit.Db.Kit_Type_Reference;
   overriding function Has_Element (Handle : Kit_Type_Handle) return Boolean;

   function Kit_Root_Record_Handle (Handle : Kit_Type_Handle)
      return Kit.Handles.Kit_Root_Record.Kit_Root_Record_Handle;
   function To_Kit_Type_Handle (Class : Kit_Type_Class)
      return Kit_Type_Handle;

   function Update_Kit_Type (Target : Kit.Db.Kit_Type_Reference)
      return Kit_Type_Update_Handle;
   procedure Done (Update : Kit_Type_Update_Handle);
   function Set_Size
     (Update : Kit_Type_Update_Handle;
      Value  : Integer)
   return Kit_Type_Update_Handle;
   function Set_Name
     (Update : Kit_Type_Update_Handle;
      Value  : String)
   return Kit_Type_Update_Handle;
   function Update (Handle : Kit_Type_Handle)
      return Kit_Type_Update_Handle'Class;
   overriding function Update_Kit_Type (Handle : Kit_Type_Handle)
      return Kit_Type_Update_Handle'Class;

   overriding function Top_Record (Handle : Kit_Type_Handle)
      return Kit.Db.Record_Type;
   overriding function Reference_Kit_Root_Record (Handle : Kit_Type_Handle)
      return Kit.Db.Kit_Root_Record_Reference;
   overriding function Size (Handle : Kit_Type_Handle) return Integer;
   overriding function Name (Handle : Kit_Type_Handle) return String;
   function Empty_Handle return Kit_Type_Handle;

   function Create
     (Size : Integer;
      Name : String)
   return Kit_Type_Handle;
   procedure Create
     (Size : Integer;
      Name : String);
   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Kit_Type_Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Type_Class;
   function Iterate (Container : Kit_Type_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Kit_Type_Selection;
      Position  : Cursor)
   return Kit_Type_Class;
   function Is_Empty (Container : Kit_Type_Selection) return Boolean;
   function Element (Item : Cursor) return Kit_Type_Class;
   function Length (Container : Kit_Type_Selection) return Natural;
   function Scan_By_Top_Record return Kit_Type_Selection;

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Type_Handle;

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Type_Handle;

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Type_Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Type_Selection;

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class)
      return Kit_Type_Handle;

   function Scan_By_Name return Kit_Type_Selection;

   function Get_By_Name (Name : String) return Kit_Type_Handle;

   function Select_By_Name (Name : String) return Kit_Type_Selection;

   function Select_Bounded_By_Name
     (Start_Name  : String;
      Finish_Name : String)
   return Kit_Type_Selection;

   function Is_Name (Name : String) return Boolean;

private

   subtype Kit_Root_Record_Reference is Kit.Db.Kit_Root_Record_Reference;
   subtype Kit_Type_Reference is Kit.Db.Kit_Type_Reference;
   subtype Record_Type is Kit.Db.Record_Type;

   type Kit_Type_Fields is (Update_Size, Update_Name);
   type Kit_Type_Update_Value (Field : Kit_Type_Fields) is
      record
         case Field is
            when Update_Size =>
               Size_Value : Integer;
            when Update_Name =>
               Name_Value : Kit.Strings.String_Type (64);
         end case;
      end record;
   package Update_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
          (Kit_Type_Update_Value);
   type Kit_Type_Update_Handle is tagged limited
      record
         Reference : Kit_Type_Reference;
         Updates   : Update_Lists.List;
      end record;

   type Kit_Type_Handle is new Kit_Type_Interface with
      record
         Reference : Kit.Db.Kit_Type_Reference :=
            Kit.Db.Null_Kit_Type_Reference;
      end record;

   type Cursor is
      record
         Db : Kit.Db.Kit_Type.Cursor;
      end record;
   type Kit_Type_Selection is tagged
      record
         Db : Kit.Db.Kit_Type.Selection;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Handles.Kit_Type;
