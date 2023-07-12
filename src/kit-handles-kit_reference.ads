with Kit.Db;
with Kit.Handles.Kit_Root_Record;
with Kit.Handles.Kit_Type;
with Kit.Handles.Kit_Record;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Kit.Strings;
with Ada.Iterator_Interfaces;
private with Kit.Db.Kit_Reference;

package Kit.Handles.Kit_Reference is

   subtype Kit_Root_Record_Class is
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class;
   subtype Kit_Type_Class is Kit.Handles.Kit_Type.Kit_Type_Class;
   subtype Kit_Record_Class is Kit.Handles.Kit_Record.Kit_Record_Class;

   procedure Get_Cache_Statistics
     (Size   :    out Natural;
      Hits   :    out Natural;
      Misses :    out Natural);

   type Kit_Reference_Update_Handle is tagged limited private;
   type Kit_Reference_Interface is interface
     and Kit_Root_Record.Kit_Root_Record_Interface
     and Kit_Type.Kit_Type_Interface
     and Handle_Interface;

   subtype Kit_Reference_Class is Kit_Reference_Interface'Class;

   function Reference_Kit_Reference (Handle : Kit_Reference_Interface)
      return Kit.Db.Kit_Reference_Reference
      is abstract;
   function Update_Kit_Reference (Handle : Kit_Reference_Interface)
      return Kit_Reference_Update_Handle'Class
      is abstract;
   function Reference (Handle : Kit_Reference_Interface)
      return Kit.Handles.Kit_Record.Kit_Record_Class
      is abstract;

   type Kit_Reference_Handle is new Kit_Reference_Interface with private;

   function Get (Reference : Kit.Db.Kit_Reference_Reference)
      return Kit_Reference_Handle;

   function Reference (Handle : Kit_Reference_Handle)
      return Kit.Db.Kit_Reference_Reference;
   overriding function Reference_Kit_Reference (Handle :
      Kit_Reference_Handle) return Kit.Db.Kit_Reference_Reference;
   overriding function Has_Element (Handle : Kit_Reference_Handle)
      return Boolean;

   function Kit_Root_Record_Handle (Handle : Kit_Reference_Handle)
      return Kit.Handles.Kit_Root_Record.Kit_Root_Record_Handle;
   function Kit_Type_Handle (Handle : Kit_Reference_Handle)
      return Kit.Handles.Kit_Type.Kit_Type_Handle;
   function To_Kit_Reference_Handle (Class : Kit_Reference_Class)
      return Kit_Reference_Handle;

   function Update_Kit_Reference (Target : Kit.Db.Kit_Reference_Reference)
      return Kit_Reference_Update_Handle;
   procedure Done (Update : Kit_Reference_Update_Handle);
   function Set_Size
     (Update : Kit_Reference_Update_Handle;
      Value  : Integer)
   return Kit_Reference_Update_Handle;
   function Set_Name
     (Update : Kit_Reference_Update_Handle;
      Value  : String)
   return Kit_Reference_Update_Handle;
   function Set_Reference
     (Update : Kit_Reference_Update_Handle;
      Value  : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Reference_Update_Handle;
   function Update (Handle : Kit_Reference_Handle)
      return Kit_Reference_Update_Handle'Class;
   overriding function Update_Kit_Reference (Handle : Kit_Reference_Handle)
      return Kit_Reference_Update_Handle'Class;
   overriding function Update_Kit_Type (Handle : Kit_Reference_Handle)
      return Kit_Type.Kit_Type_Update_Handle'Class;

   overriding function Top_Record (Handle : Kit_Reference_Handle)
      return Kit.Db.Record_Type;
   overriding function Reference_Kit_Root_Record (Handle :
      Kit_Reference_Handle) return Kit.Db.Kit_Root_Record_Reference;
   overriding function Size (Handle : Kit_Reference_Handle) return Integer;
   overriding function Name (Handle : Kit_Reference_Handle) return String;
   overriding function Reference_Kit_Type (Handle : Kit_Reference_Handle)
      return Kit.Db.Kit_Type_Reference;
   overriding function Reference (Handle : Kit_Reference_Handle)
      return Kit.Handles.Kit_Record.Kit_Record_Class;
   function Empty_Handle return Kit_Reference_Handle;

   function Create
     (Size      : Integer;
      Name      : String;
      Reference : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Reference_Handle;
   procedure Create
     (Size      : Integer;
      Name      : String;
      Reference : Kit.Handles.Kit_Record.Kit_Record_Class);
   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Kit_Reference_Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Reference_Class;
   function Iterate (Container : Kit_Reference_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Kit_Reference_Selection;
      Position  : Cursor)
   return Kit_Reference_Class;
   function Is_Empty (Container : Kit_Reference_Selection) return Boolean;
   function Element (Item : Cursor) return Kit_Reference_Class;
   function Length (Container : Kit_Reference_Selection) return Natural;
   function Scan_By_Top_Record return Kit_Reference_Selection;

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Reference_Handle;

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Reference_Handle;

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Reference_Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Reference_Selection;

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class)
      return Kit_Reference_Handle;

   function Scan_By_Name return Kit_Reference_Selection;

   function Get_By_Name (Name : String) return Kit_Reference_Handle;

   function Select_By_Name (Name : String) return Kit_Reference_Selection;

   function Select_Bounded_By_Name
     (Start_Name  : String;
      Finish_Name : String)
   return Kit_Reference_Selection;

   function Get_From_Kit_Type (Kit_Type :
      Kit.Handles.Kit_Type.Kit_Type_Class) return Kit_Reference_Handle;

private

   subtype Kit_Root_Record_Reference is Kit.Db.Kit_Root_Record_Reference;
   subtype Kit_Type_Reference is Kit.Db.Kit_Type_Reference;
   subtype Kit_Record_Reference is Kit.Db.Kit_Record_Reference;
   subtype Kit_Reference_Reference is Kit.Db.Kit_Reference_Reference;
   subtype Record_Type is Kit.Db.Record_Type;

   type Kit_Reference_Fields is (Update_Size, Update_Name, Update_Reference);
   type Kit_Reference_Update_Value (Field : Kit_Reference_Fields) is
      record
         case Field is
            when Update_Size =>
               Size_Value      : Integer;
            when Update_Name =>
               Name_Value      : Kit.Strings.String_Type (64);
            when Update_Reference =>
               Reference_Value : Kit_Record_Reference;
         end case;
      end record;
   package Update_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
          (Kit_Reference_Update_Value);
   type Kit_Reference_Update_Handle is tagged limited
      record
         Reference : Kit_Reference_Reference;
         Updates   : Update_Lists.List;
      end record;

   type Kit_Reference_Handle is new Kit_Reference_Interface with
      record
         Reference : Kit.Db.Kit_Reference_Reference :=
            Kit.Db.Null_Kit_Reference_Reference;
      end record;

   type Cursor is
      record
         Db : Kit.Db.Kit_Reference.Cursor;
      end record;
   type Kit_Reference_Selection is tagged
      record
         Db : Kit.Db.Kit_Reference.Selection;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Handles.Kit_Reference;
