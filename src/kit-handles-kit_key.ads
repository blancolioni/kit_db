with Kit.Db;
with Kit.Handles.Kit_Root_Record;
with Kit.Handles.Kit_Record;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Kit.Strings;
with Ada.Iterator_Interfaces;
private with Kit.Db.Kit_Key;

package Kit.Handles.Kit_Key is

   subtype Kit_Root_Record_Class is
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class;
   subtype Kit_Record_Class is Kit.Handles.Kit_Record.Kit_Record_Class;

   procedure Get_Cache_Statistics
     (Size   :    out Natural;
      Hits   :    out Natural;
      Misses :    out Natural);

   type Kit_Key_Update_Handle is tagged limited private;
   type Kit_Key_Interface is interface
     and Kit_Root_Record.Kit_Root_Record_Interface
     and Handle_Interface;

   subtype Kit_Key_Class is Kit_Key_Interface'Class;

   function Reference_Kit_Key (Handle : Kit_Key_Interface)
      return Kit.Db.Kit_Key_Reference
      is abstract;
   function Update_Kit_Key (Handle : Kit_Key_Interface)
      return Kit_Key_Update_Handle'Class
      is abstract;
   function Name (Handle : Kit_Key_Interface) return String
      is abstract;
   function Kit_Record (Handle : Kit_Key_Interface)
      return Kit.Handles.Kit_Record.Kit_Record_Class
      is abstract;
   function Is_Unique (Handle : Kit_Key_Interface) return Boolean
      is abstract;
   function Length (Handle : Kit_Key_Interface) return Integer
      is abstract;

   type Kit_Key_Handle is new Kit_Key_Interface with private;

   function Get (Reference : Kit.Db.Kit_Key_Reference) return Kit_Key_Handle;

   function Reference (Handle : Kit_Key_Handle)
      return Kit.Db.Kit_Key_Reference;
   overriding function Reference_Kit_Key (Handle : Kit_Key_Handle)
      return Kit.Db.Kit_Key_Reference;
   overriding function Has_Element (Handle : Kit_Key_Handle) return Boolean;

   function Kit_Root_Record_Handle (Handle : Kit_Key_Handle)
      return Kit.Handles.Kit_Root_Record.Kit_Root_Record_Handle;
   function To_Kit_Key_Handle (Class : Kit_Key_Class) return Kit_Key_Handle;

   function Update_Kit_Key (Target : Kit.Db.Kit_Key_Reference)
      return Kit_Key_Update_Handle;
   procedure Done (Update : Kit_Key_Update_Handle);
   function Set_Name
     (Update : Kit_Key_Update_Handle;
      Value  : String)
   return Kit_Key_Update_Handle;
   function Set_Kit_Record
     (Update : Kit_Key_Update_Handle;
      Value  : Kit.Handles.Kit_Record.Kit_Record_Class)
   return Kit_Key_Update_Handle;
   function Set_Is_Unique
     (Update : Kit_Key_Update_Handle;
      Value  : Boolean)
   return Kit_Key_Update_Handle;
   function Set_Length
     (Update : Kit_Key_Update_Handle;
      Value  : Integer)
   return Kit_Key_Update_Handle;
   function Update (Handle : Kit_Key_Handle)
      return Kit_Key_Update_Handle'Class;
   overriding function Update_Kit_Key (Handle : Kit_Key_Handle)
      return Kit_Key_Update_Handle'Class;

   overriding function Top_Record (Handle : Kit_Key_Handle)
      return Kit.Db.Record_Type;
   overriding function Reference_Kit_Root_Record (Handle : Kit_Key_Handle)
      return Kit.Db.Kit_Root_Record_Reference;
   overriding function Name (Handle : Kit_Key_Handle) return String;
   overriding function Kit_Record (Handle : Kit_Key_Handle)
      return Kit.Handles.Kit_Record.Kit_Record_Class;
   overriding function Is_Unique (Handle : Kit_Key_Handle) return Boolean;
   overriding function Length (Handle : Kit_Key_Handle) return Integer;
   function Empty_Handle return Kit_Key_Handle;

   function Create
     (Name       : String;
      Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Is_Unique  : Boolean;
      Length     : Integer)
   return Kit_Key_Handle;
   procedure Create
     (Name       : String;
      Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Is_Unique  : Boolean;
      Length     : Integer);
   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Kit_Key_Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Key_Class;
   function Iterate (Container : Kit_Key_Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Kit_Key_Selection;
      Position  : Cursor)
   return Kit_Key_Class;
   function Is_Empty (Container : Kit_Key_Selection) return Boolean;
   function Element (Item : Cursor) return Kit_Key_Class;
   function Length (Container : Kit_Key_Selection) return Natural;
   function Scan_By_Top_Record return Kit_Key_Selection;

   function First_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Key_Handle;

   function Last_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Key_Handle;

   function Select_By_Top_Record (Top_Record : Kit.Db.Record_Type)
      return Kit_Key_Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Kit.Db.Record_Type;
      Finish_Top_Record : Kit.Db.Record_Type)
   return Kit_Key_Selection;

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit.Handles.Kit_Root_Record.Kit_Root_Record_Class)
      return Kit_Key_Handle;

   function Scan_By_Kit_Record return Kit_Key_Selection;

   function First_By_Kit_Record (Kit_Record :
      Kit.Handles.Kit_Record.Kit_Record_Class) return Kit_Key_Handle;

   function Last_By_Kit_Record (Kit_Record :
      Kit.Handles.Kit_Record.Kit_Record_Class) return Kit_Key_Handle;

   function Select_By_Kit_Record (Kit_Record :
      Kit.Handles.Kit_Record.Kit_Record_Class) return Kit_Key_Selection;

   function Scan_By_Record_Key return Kit_Key_Selection;

   function Get_By_Record_Key
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Name       : String)
   return Kit_Key_Handle;

   function Select_By_Record_Key
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Name       : String)
   return Kit_Key_Selection;

   function Select_Record_Key_Bounded_By_Name
     (Kit_Record  : Kit.Handles.Kit_Record.Kit_Record_Class;
      Start_Name  : String;
      Finish_Name : String)
   return Kit_Key_Selection;

   function Is_Record_Key
     (Kit_Record : Kit.Handles.Kit_Record.Kit_Record_Class;
      Name       : String)
   return Boolean;

private

   subtype Kit_Root_Record_Reference is Kit.Db.Kit_Root_Record_Reference;
   subtype Kit_Record_Reference is Kit.Db.Kit_Record_Reference;
   subtype Kit_Key_Reference is Kit.Db.Kit_Key_Reference;
   subtype Record_Type is Kit.Db.Record_Type;

   type Kit_Key_Fields is (Update_Name, Update_Kit_Record, Update_Is_Unique,
                           Update_Length);
   type Kit_Key_Update_Value (Field : Kit_Key_Fields) is
      record
         case Field is
            when Update_Name =>
               Name_Value       : Kit.Strings.String_Type (64);
            when Update_Kit_Record =>
               Kit_Record_Value : Kit_Record_Reference;
            when Update_Is_Unique =>
               Is_Unique_Value  : Boolean;
            when Update_Length =>
               Length_Value     : Integer;
         end case;
      end record;
   package Update_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
          (Kit_Key_Update_Value);
   type Kit_Key_Update_Handle is tagged limited
      record
         Reference : Kit_Key_Reference;
         Updates   : Update_Lists.List;
      end record;

   type Kit_Key_Handle is new Kit_Key_Interface with
      record
         Reference : Kit.Db.Kit_Key_Reference :=
            Kit.Db.Null_Kit_Key_Reference;
      end record;

   type Cursor is
      record
         Db : Kit.Db.Kit_Key.Cursor;
      end record;
   type Kit_Key_Selection is tagged
      record
         Db : Kit.Db.Kit_Key.Selection;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Handles.Kit_Key;
