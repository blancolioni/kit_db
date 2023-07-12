private with Ada.Containers.Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;
with Kit.Db.Kit_Root_Record;
with Kit.Db.Kit_Record;
with Kit.Db.Kit_Type;

package Kit.Db.Kit_Field is

   F_Top_Record : constant String := "top_record";
   F_Kit_Root_Record : constant String := "kit_root_record";
   F_Name : constant String := "name";
   F_Kit_Record : constant String := "kit_record";
   F_Field_Type : constant String := "field_type";
   F_Field_Offset : constant String := "field_offset";
   F_Field_Length : constant String := "field_length";
   F_Created : constant String := "created";
   F_Readable : constant String := "readable";
   F_Writeable : constant String := "writeable";
   F_Display : constant String := "display";
   F_Base_Ref : constant String := "base_ref";

   type Kit_Field_Interface is limited interface
     and Record_Interface
     and Search_Interface
     and Kit.Db.Kit_Root_Record.Kit_Root_Record_Interface;

   subtype Kit_Field_Type is Kit_Field_Interface'Class;

   function Get_Kit_Field_Reference (Item : Kit_Field_Interface)
      return Kit_Field_Reference
      is abstract;

   function Name (Item : Kit_Field_Interface) return String
      is abstract;

   function Kit_Record (Item : Kit_Field_Interface)
      return Kit_Record_Reference
      is abstract;

   function Field_Type (Item : Kit_Field_Interface) return Kit_Type_Reference
      is abstract;

   function Field_Offset (Item : Kit_Field_Interface) return Integer
      is abstract;

   function Field_Length (Item : Kit_Field_Interface) return Integer
      is abstract;

   function Created (Item : Kit_Field_Interface) return Boolean
      is abstract;

   function Readable (Item : Kit_Field_Interface) return Boolean
      is abstract;

   function Writeable (Item : Kit_Field_Interface) return Boolean
      is abstract;

   function Display (Item : Kit_Field_Interface) return Boolean
      is abstract;

   function Base_Ref (Item : Kit_Field_Interface) return Boolean
      is abstract;

   type Kit_Field_Update_Interface is limited interface
     and Kit_Field_Interface
     and Kit.Db.Kit_Root_Record.Kit_Root_Record_Update_Interface;

   subtype Kit_Field_Update is Kit_Field_Update_Interface'Class;
   function Create return Kit_Field_Update;
   procedure Create
     (Name         : String;
      Kit_Record   : Kit_Record_Reference;
      Field_Type   : Kit_Type_Reference;
      Field_Offset : Integer;
      Field_Length : Integer;
      Created      : Boolean;
      Readable     : Boolean;
      Writeable    : Boolean;
      Display      : Boolean;
      Base_Ref     : Boolean);
   function Create
     (Name         : String;
      Kit_Record   : Kit_Record_Reference;
      Field_Type   : Kit_Type_Reference;
      Field_Offset : Integer;
      Field_Length : Integer;
      Created      : Boolean;
      Readable     : Boolean;
      Writeable    : Boolean;
      Display      : Boolean;
      Base_Ref     : Boolean)
   return Kit_Field_Reference;

   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   type Constant_Reference_Type
      (Element : not null access constant Kit_Field_Reference) is private
   with Implicit_Dereference => Element;
   type Reference_Type (Element : not null access Kit_Field_Type) is private
   with Implicit_Dereference => Element;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Field_Reference;
   function Iterate (Container : Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Selection;
      Position  : Cursor)
   return Constant_Reference_Type;
   function Is_Empty (Container : Selection) return Boolean;
   function Element (Item : Cursor) return Kit_Field_Reference;
   function Length (Container : Selection) return Natural;
   function Scan_By_Top_Record return Selection;

   function First_By_Top_Record (Top_Record : Record_Type)
      return Kit_Field_Reference;

   function Last_By_Top_Record (Top_Record : Record_Type)
      return Kit_Field_Reference;

   function Select_By_Top_Record (Top_Record : Record_Type) return Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Record_Type;
      Finish_Top_Record : Record_Type)
   return Selection;

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit_Root_Record_Reference) return Kit_Field_Reference;

   function Scan_By_Kit_Record return Selection;

   function First_By_Kit_Record (Kit_Record : Kit_Record_Reference)
      return Kit_Field_Reference;

   function Last_By_Kit_Record (Kit_Record : Kit_Record_Reference)
      return Kit_Field_Reference;

   function Select_By_Kit_Record (Kit_Record : Kit_Record_Reference)
      return Selection;

   function Scan_By_Record_Field return Selection;

   function Get_By_Record_Field
     (Kit_Record : Kit_Record_Reference;
      Name       : String)
   return Kit_Field_Reference;

   function Select_By_Record_Field
     (Kit_Record : Kit_Record_Reference;
      Name       : String)
   return Selection;

   function Select_Record_Field_Bounded_By_Name
     (Kit_Record  : Kit_Record_Reference;
      Start_Name  : String;
      Finish_Name : String)
   return Selection;

   function Is_Record_Field
     (Kit_Record : Kit_Record_Reference;
      Name       : String)
   return Boolean;

   function Scan_By_Display_Field return Selection;

   function First_By_Display_Field
     (Kit_Record : Kit_Record_Reference;
      Display    : Boolean)
   return Kit_Field_Reference;

   function Last_By_Display_Field
     (Kit_Record : Kit_Record_Reference;
      Display    : Boolean)
   return Kit_Field_Reference;

   function Select_By_Display_Field
     (Kit_Record : Kit_Record_Reference;
      Display    : Boolean)
   return Selection;

   function Select_Display_Field_Bounded_By_Display
     (Kit_Record     : Kit_Record_Reference;
      Start_Display  : Boolean;
      Finish_Display : Boolean)
   return Selection;

   function Get (Ref : Kit_Field_Reference) return Kit_Field_Type;
   function Get_Update (Ref : Kit_Field_Reference) return Kit_Field_Update;

   function First (Container : Selection) return Cursor;
   function Last (Container : Selection) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Previous (Position : Cursor) return Cursor;
   procedure Next (Position : in out Cursor);
   procedure Previous (Position : in out Cursor);

   type Kit_Field_Notify_Handler is access
     procedure (Reference : Kit_Field_Reference);
   type Kit_Field_Table_Notify_Handler is access
     procedure;

   procedure On_Kit_Field_Table_Change (Handler :
      Kit_Field_Table_Notify_Handler);
   procedure On_Kit_Field_Change
     (Reference : Kit_Field_Reference;
      Handler   : Kit_Field_Notify_Handler);
   procedure On_Kit_Field_Created (Handler : Kit_Field_Notify_Handler);
   procedure On_Kit_Field_Deleted (Handler : Kit_Field_Notify_Handler);
   procedure Set_Name
     (Item  : in out Kit_Field_Update_Interface;
      Value : String)
      is abstract;

   procedure Set_Kit_Record
     (Item  : in out Kit_Field_Update_Interface;
      Value : Kit_Record_Reference)
      is abstract;
   procedure Set_Kit_Record
     (Item  : in out Kit_Field_Update_Interface;
      Value : Kit.Db.Kit_Record.Kit_Record_Type)
      is abstract;

   procedure Set_Field_Type
     (Item  : in out Kit_Field_Update_Interface;
      Value : Kit_Type_Reference)
      is abstract;
   procedure Set_Field_Type
     (Item  : in out Kit_Field_Update_Interface;
      Value : Kit.Db.Kit_Type.Kit_Type_Type)
      is abstract;

   procedure Set_Field_Offset
     (Item  : in out Kit_Field_Update_Interface;
      Value : Integer)
      is abstract;

   procedure Set_Field_Length
     (Item  : in out Kit_Field_Update_Interface;
      Value : Integer)
      is abstract;

   procedure Set_Created
     (Item  : in out Kit_Field_Update_Interface;
      Value : Boolean)
      is abstract;

   procedure Set_Readable
     (Item  : in out Kit_Field_Update_Interface;
      Value : Boolean)
      is abstract;

   procedure Set_Writeable
     (Item  : in out Kit_Field_Update_Interface;
      Value : Boolean)
      is abstract;

   procedure Set_Display
     (Item  : in out Kit_Field_Update_Interface;
      Value : Boolean)
      is abstract;

   procedure Set_Base_Ref
     (Item  : in out Kit_Field_Update_Interface;
      Value : Boolean)
      is abstract;

private

   package List_Of_References is
     new Ada.Containers.Doubly_Linked_Lists
          (Kit_Field_Reference);
   type Cursor is
      record
         Current : List_Of_References.Cursor;
      end record;
   type Constant_Reference_Type
      (Element : not null access constant Kit_Field_Reference) is
          null record;
   type Reference_Type (Element : not null access Kit_Field_Type) is
          null record;
   type Selection is tagged
      record
         Elements : List_Of_References.List;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Db.Kit_Field;
