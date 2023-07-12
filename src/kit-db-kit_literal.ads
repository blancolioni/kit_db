private with Ada.Containers.Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;
with Kit.Db.Kit_Root_Record;
with Kit.Db.Kit_Enumeration;

package Kit.Db.Kit_Literal is

   F_Top_Record : constant String := "top_record";
   F_Kit_Root_Record : constant String := "kit_root_record";
   F_Name : constant String := "name";
   F_Kit_Enumeration : constant String := "kit_enumeration";
   F_Value : constant String := "value";

   type Kit_Literal_Interface is limited interface
     and Record_Interface
     and Search_Interface
     and Kit.Db.Kit_Root_Record.Kit_Root_Record_Interface;

   subtype Kit_Literal_Type is Kit_Literal_Interface'Class;

   function Get_Kit_Literal_Reference (Item : Kit_Literal_Interface)
      return Kit_Literal_Reference
      is abstract;

   function Name (Item : Kit_Literal_Interface) return String
      is abstract;

   function Kit_Enumeration (Item : Kit_Literal_Interface)
      return Kit_Enumeration_Reference
      is abstract;

   function Value (Item : Kit_Literal_Interface) return Integer
      is abstract;

   type Kit_Literal_Update_Interface is limited interface
     and Kit_Literal_Interface
     and Kit.Db.Kit_Root_Record.Kit_Root_Record_Update_Interface;

   subtype Kit_Literal_Update is Kit_Literal_Update_Interface'Class;
   function Create return Kit_Literal_Update;
   procedure Create
     (Name            : String;
      Kit_Enumeration : Kit_Enumeration_Reference;
      Value           : Integer);
   function Create
     (Name            : String;
      Kit_Enumeration : Kit_Enumeration_Reference;
      Value           : Integer)
   return Kit_Literal_Reference;

   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   type Constant_Reference_Type
      (Element : not null access constant Kit_Literal_Reference) is private
   with Implicit_Dereference => Element;
   type Reference_Type (Element : not null access Kit_Literal_Type) is
      private
   with Implicit_Dereference => Element;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Literal_Reference;
   function Iterate (Container : Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Selection;
      Position  : Cursor)
   return Constant_Reference_Type;
   function Is_Empty (Container : Selection) return Boolean;
   function Element (Item : Cursor) return Kit_Literal_Reference;
   function Length (Container : Selection) return Natural;
   function Scan_By_Top_Record return Selection;

   function First_By_Top_Record (Top_Record : Record_Type)
      return Kit_Literal_Reference;

   function Last_By_Top_Record (Top_Record : Record_Type)
      return Kit_Literal_Reference;

   function Select_By_Top_Record (Top_Record : Record_Type) return Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Record_Type;
      Finish_Top_Record : Record_Type)
   return Selection;

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit_Root_Record_Reference) return Kit_Literal_Reference;

   function Scan_By_Kit_Enumeration return Selection;

   function First_By_Kit_Enumeration (Kit_Enumeration :
      Kit_Enumeration_Reference) return Kit_Literal_Reference;

   function Last_By_Kit_Enumeration (Kit_Enumeration :
      Kit_Enumeration_Reference) return Kit_Literal_Reference;

   function Select_By_Kit_Enumeration (Kit_Enumeration :
      Kit_Enumeration_Reference) return Selection;

   function Scan_By_Value return Selection;

   function First_By_Value (Value : Integer) return Kit_Literal_Reference;

   function Last_By_Value (Value : Integer) return Kit_Literal_Reference;

   function Select_By_Value (Value : Integer) return Selection;

   function Select_Bounded_By_Value
     (Start_Value  : Integer;
      Finish_Value : Integer)
   return Selection;

   function Scan_By_Enum_Value return Selection;

   function Get_By_Enum_Value
     (Kit_Enumeration : Kit_Enumeration_Reference;
      Value           : Integer)
   return Kit_Literal_Reference;

   function Select_By_Enum_Value
     (Kit_Enumeration : Kit_Enumeration_Reference;
      Value           : Integer)
   return Selection;

   function Select_Enum_Value_Bounded_By_Value
     (Kit_Enumeration : Kit_Enumeration_Reference;
      Start_Value     : Integer;
      Finish_Value    : Integer)
   return Selection;

   function Is_Enum_Value
     (Kit_Enumeration : Kit_Enumeration_Reference;
      Value           : Integer)
   return Boolean;

   function Scan_By_Enum_Name return Selection;

   function Get_By_Enum_Name
     (Kit_Enumeration : Kit_Enumeration_Reference;
      Name            : String)
   return Kit_Literal_Reference;

   function Select_By_Enum_Name
     (Kit_Enumeration : Kit_Enumeration_Reference;
      Name            : String)
   return Selection;

   function Select_Enum_Name_Bounded_By_Name
     (Kit_Enumeration : Kit_Enumeration_Reference;
      Start_Name      : String;
      Finish_Name     : String)
   return Selection;

   function Is_Enum_Name
     (Kit_Enumeration : Kit_Enumeration_Reference;
      Name            : String)
   return Boolean;

   function Get (Ref : Kit_Literal_Reference) return Kit_Literal_Type;
   function Get_Update (Ref : Kit_Literal_Reference)
      return Kit_Literal_Update;

   function First (Container : Selection) return Cursor;
   function Last (Container : Selection) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Previous (Position : Cursor) return Cursor;
   procedure Next (Position : in out Cursor);
   procedure Previous (Position : in out Cursor);

   type Kit_Literal_Notify_Handler is access
     procedure (Reference : Kit_Literal_Reference);
   type Kit_Literal_Table_Notify_Handler is access
     procedure;

   procedure On_Kit_Literal_Table_Change (Handler :
      Kit_Literal_Table_Notify_Handler);
   procedure On_Kit_Literal_Change
     (Reference : Kit_Literal_Reference;
      Handler   : Kit_Literal_Notify_Handler);
   procedure On_Kit_Literal_Created (Handler : Kit_Literal_Notify_Handler);
   procedure On_Kit_Literal_Deleted (Handler : Kit_Literal_Notify_Handler);
   procedure Set_Name
     (Item  : in out Kit_Literal_Update_Interface;
      Value : String)
      is abstract;

   procedure Set_Kit_Enumeration
     (Item  : in out Kit_Literal_Update_Interface;
      Value : Kit_Enumeration_Reference)
      is abstract;
   procedure Set_Kit_Enumeration
     (Item  : in out Kit_Literal_Update_Interface;
      Value : Kit.Db.Kit_Enumeration.Kit_Enumeration_Type)
      is abstract;

   procedure Set_Value
     (Item  : in out Kit_Literal_Update_Interface;
      Value : Integer)
      is abstract;

private

   package List_Of_References is
     new Ada.Containers.Doubly_Linked_Lists
          (Kit_Literal_Reference);
   type Cursor is
      record
         Current : List_Of_References.Cursor;
      end record;
   type Constant_Reference_Type
      (Element : not null access constant Kit_Literal_Reference) is
          null record;
   type Reference_Type (Element : not null access Kit_Literal_Type) is
          null record;
   type Selection is tagged
      record
         Elements : List_Of_References.List;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Db.Kit_Literal;
