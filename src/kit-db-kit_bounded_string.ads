private with Ada.Containers.Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;
with Kit.Db.Kit_Root_Record;
with Kit.Db.Kit_Type;
with Kit.Db.Kit_String;

package Kit.Db.Kit_Bounded_String is

   F_Top_Record : constant String := "top_record";
   F_Kit_Root_Record : constant String := "kit_root_record";
   F_Kit_Type : constant String := "kit_type";
   F_Length : constant String := "length";
   F_Size : constant String := "size";
   F_Name : constant String := "name";
   F_Kit_String : constant String := "kit_string";

   type Kit_Bounded_String_Interface is limited interface
     and Record_Interface
     and Search_Interface
     and Kit.Db.Kit_Root_Record.Kit_Root_Record_Interface
     and Kit.Db.Kit_Type.Kit_Type_Interface
     and Kit.Db.Kit_String.Kit_String_Interface;

   subtype Kit_Bounded_String_Type is Kit_Bounded_String_Interface'Class;

   function Get_Kit_Bounded_String_Reference (Item :
      Kit_Bounded_String_Interface) return Kit_Bounded_String_Reference
      is abstract;

   type Kit_Bounded_String_Update_Interface is limited interface
     and Kit_Bounded_String_Interface
     and Kit.Db.Kit_Root_Record.Kit_Root_Record_Update_Interface
     and Kit.Db.Kit_Type.Kit_Type_Update_Interface
     and Kit.Db.Kit_String.Kit_String_Update_Interface;

   subtype Kit_Bounded_String_Update is
      Kit_Bounded_String_Update_Interface'Class;
   function Create return Kit_Bounded_String_Update;
   procedure Create
     (Length : Integer;
      Size   : Integer;
      Name   : String);
   function Create
     (Length : Integer;
      Size   : Integer;
      Name   : String)
   return Kit_Bounded_String_Reference;

   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   type Constant_Reference_Type
      (Element : not null access constant Kit_Bounded_String_Reference) is
      private
   with Implicit_Dereference => Element;
   type Reference_Type (Element : not null access Kit_Bounded_String_Type) is
      private
   with Implicit_Dereference => Element;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Bounded_String_Reference;
   function Iterate (Container : Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Selection;
      Position  : Cursor)
   return Constant_Reference_Type;
   function Is_Empty (Container : Selection) return Boolean;
   function Element (Item : Cursor) return Kit_Bounded_String_Reference;
   function Length (Container : Selection) return Natural;
   function Scan_By_Top_Record return Selection;

   function First_By_Top_Record (Top_Record : Record_Type)
      return Kit_Bounded_String_Reference;

   function Last_By_Top_Record (Top_Record : Record_Type)
      return Kit_Bounded_String_Reference;

   function Select_By_Top_Record (Top_Record : Record_Type) return Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Record_Type;
      Finish_Top_Record : Record_Type)
   return Selection;

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit_Root_Record_Reference) return Kit_Bounded_String_Reference;

   function Scan_By_Name return Selection;

   function Get_By_Name (Name : String) return Kit_Bounded_String_Reference;

   function Select_By_Name (Name : String) return Selection;

   function Select_Bounded_By_Name
     (Start_Name  : String;
      Finish_Name : String)
   return Selection;

   function Get_From_Kit_Type (Kit_Type : Kit_Type_Reference)
      return Kit_Bounded_String_Reference;

   function Get_From_Kit_String (Kit_String : Kit_String_Reference)
      return Kit_Bounded_String_Reference;

   function Get (Ref : Kit_Bounded_String_Reference)
      return Kit_Bounded_String_Type;
   function Get_Update (Ref : Kit_Bounded_String_Reference)
      return Kit_Bounded_String_Update;

   function First (Container : Selection) return Cursor;
   function Last (Container : Selection) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Previous (Position : Cursor) return Cursor;
   procedure Next (Position : in out Cursor);
   procedure Previous (Position : in out Cursor);

   type Kit_Bounded_String_Notify_Handler is access
     procedure (Reference : Kit_Bounded_String_Reference);
   type Kit_Bounded_String_Table_Notify_Handler is access
     procedure;

   procedure On_Kit_Bounded_String_Table_Change (Handler :
      Kit_Bounded_String_Table_Notify_Handler);
   procedure On_Kit_Bounded_String_Change
     (Reference : Kit_Bounded_String_Reference;
      Handler   : Kit_Bounded_String_Notify_Handler);
   procedure On_Kit_Bounded_String_Created (Handler :
      Kit_Bounded_String_Notify_Handler);
   procedure On_Kit_Bounded_String_Deleted (Handler :
      Kit_Bounded_String_Notify_Handler);

private

   package List_Of_References is
     new Ada.Containers.Doubly_Linked_Lists
          (Kit_Bounded_String_Reference);
   type Cursor is
      record
         Current : List_Of_References.Cursor;
      end record;
   type Constant_Reference_Type
      (Element : not null access constant Kit_Bounded_String_Reference) is
          null record;
   type Reference_Type (Element : not null access Kit_Bounded_String_Type) is
          null record;
   type Selection is tagged
      record
         Elements : List_Of_References.List;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Db.Kit_Bounded_String;
