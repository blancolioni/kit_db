private with Ada.Containers.Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;
with Kit.Db.Kit_Root_Record;

package Kit.Db.Kit_Record is

   F_Top_Record : constant String := "top_record";
   F_Kit_Root_Record : constant String := "kit_root_record";
   F_Name : constant String := "name";
   F_Table_Index : constant String := "table_index";
   F_Record_Length : constant String := "record_length";

   type Kit_Record_Interface is limited interface
     and Record_Interface
     and Search_Interface
     and Kit.Db.Kit_Root_Record.Kit_Root_Record_Interface;

   subtype Kit_Record_Type is Kit_Record_Interface'Class;

   function Get_Kit_Record_Reference (Item : Kit_Record_Interface)
      return Kit_Record_Reference
      is abstract;

   function Name (Item : Kit_Record_Interface) return String
      is abstract;

   function Table_Index (Item : Kit_Record_Interface) return Integer
      is abstract;

   function Record_Length (Item : Kit_Record_Interface) return Integer
      is abstract;

   type Kit_Record_Update_Interface is limited interface
     and Kit_Record_Interface
     and Kit.Db.Kit_Root_Record.Kit_Root_Record_Update_Interface;

   subtype Kit_Record_Update is Kit_Record_Update_Interface'Class;
   function Create return Kit_Record_Update;
   procedure Create
     (Name          : String;
      Table_Index   : Integer;
      Record_Length : Integer);
   function Create
     (Name          : String;
      Table_Index   : Integer;
      Record_Length : Integer)
   return Kit_Record_Reference;

   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   type Constant_Reference_Type
      (Element : not null access constant Kit_Record_Reference) is private
   with Implicit_Dereference => Element;
   type Reference_Type (Element : not null access Kit_Record_Type) is private
   with Implicit_Dereference => Element;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Record_Reference;
   function Iterate (Container : Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Selection;
      Position  : Cursor)
   return Constant_Reference_Type;
   function Is_Empty (Container : Selection) return Boolean;
   function Element (Item : Cursor) return Kit_Record_Reference;
   function Length (Container : Selection) return Natural;
   function Scan_By_Top_Record return Selection;

   function First_By_Top_Record (Top_Record : Record_Type)
      return Kit_Record_Reference;

   function Last_By_Top_Record (Top_Record : Record_Type)
      return Kit_Record_Reference;

   function Select_By_Top_Record (Top_Record : Record_Type) return Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Record_Type;
      Finish_Top_Record : Record_Type)
   return Selection;

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit_Root_Record_Reference) return Kit_Record_Reference;

   function Scan_By_Name return Selection;

   function Get_By_Name (Name : String) return Kit_Record_Reference;

   function Select_By_Name (Name : String) return Selection;

   function Select_Bounded_By_Name
     (Start_Name  : String;
      Finish_Name : String)
   return Selection;

   function Is_Name (Name : String) return Boolean;

   function Scan_By_Table_Index return Selection;

   function Get_By_Table_Index (Table_Index : Integer)
      return Kit_Record_Reference;

   function Select_By_Table_Index (Table_Index : Integer) return Selection;

   function Select_Bounded_By_Table_Index
     (Start_Table_Index  : Integer;
      Finish_Table_Index : Integer)
   return Selection;

   function Is_Table_Index (Table_Index : Integer) return Boolean;

   function Get (Ref : Kit_Record_Reference) return Kit_Record_Type;
   function Get_Update (Ref : Kit_Record_Reference) return Kit_Record_Update;

   function First (Container : Selection) return Cursor;
   function Last (Container : Selection) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Previous (Position : Cursor) return Cursor;
   procedure Next (Position : in out Cursor);
   procedure Previous (Position : in out Cursor);

   type Kit_Record_Notify_Handler is access
     procedure (Reference : Kit_Record_Reference);
   type Kit_Record_Table_Notify_Handler is access
     procedure;

   procedure On_Kit_Record_Table_Change (Handler :
      Kit_Record_Table_Notify_Handler);
   procedure On_Kit_Record_Change
     (Reference : Kit_Record_Reference;
      Handler   : Kit_Record_Notify_Handler);
   procedure On_Kit_Record_Created (Handler : Kit_Record_Notify_Handler);
   procedure On_Kit_Record_Deleted (Handler : Kit_Record_Notify_Handler);
   procedure Set_Name
     (Item  : in out Kit_Record_Update_Interface;
      Value : String)
      is abstract;

   procedure Set_Table_Index
     (Item  : in out Kit_Record_Update_Interface;
      Value : Integer)
      is abstract;

   procedure Set_Record_Length
     (Item  : in out Kit_Record_Update_Interface;
      Value : Integer)
      is abstract;

private

   package List_Of_References is
     new Ada.Containers.Doubly_Linked_Lists
          (Kit_Record_Reference);
   type Cursor is
      record
         Current : List_Of_References.Cursor;
      end record;
   type Constant_Reference_Type
      (Element : not null access constant Kit_Record_Reference) is
          null record;
   type Reference_Type (Element : not null access Kit_Record_Type) is
          null record;
   type Selection is tagged
      record
         Elements : List_Of_References.List;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Db.Kit_Record;
