private with Ada.Containers.Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;
with Kit.Db.Kit_Root_Record;
with Kit.Db.Kit_Record;

package Kit.Db.Kit_Key is

   F_Top_Record : constant String := "top_record";
   F_Kit_Root_Record : constant String := "kit_root_record";
   F_Name : constant String := "name";
   F_Kit_Record : constant String := "kit_record";
   F_Is_Unique : constant String := "is_unique";
   F_Length : constant String := "length";

   type Kit_Key_Interface is limited interface
     and Record_Interface
     and Search_Interface
     and Kit.Db.Kit_Root_Record.Kit_Root_Record_Interface;

   subtype Kit_Key_Type is Kit_Key_Interface'Class;

   function Get_Kit_Key_Reference (Item : Kit_Key_Interface)
      return Kit_Key_Reference
      is abstract;

   function Name (Item : Kit_Key_Interface) return String
      is abstract;

   function Kit_Record (Item : Kit_Key_Interface) return Kit_Record_Reference
      is abstract;

   function Is_Unique (Item : Kit_Key_Interface) return Boolean
      is abstract;

   function Length (Item : Kit_Key_Interface) return Integer
      is abstract;

   type Kit_Key_Update_Interface is limited interface
     and Kit_Key_Interface
     and Kit.Db.Kit_Root_Record.Kit_Root_Record_Update_Interface;

   subtype Kit_Key_Update is Kit_Key_Update_Interface'Class;
   function Create return Kit_Key_Update;
   procedure Create
     (Name       : String;
      Kit_Record : Kit_Record_Reference;
      Is_Unique  : Boolean;
      Length     : Integer);
   function Create
     (Name       : String;
      Kit_Record : Kit_Record_Reference;
      Is_Unique  : Boolean;
      Length     : Integer)
   return Kit_Key_Reference;

   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   type Constant_Reference_Type
      (Element : not null access constant Kit_Key_Reference) is private
   with Implicit_Dereference => Element;
   type Reference_Type (Element : not null access Kit_Key_Type) is private
   with Implicit_Dereference => Element;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Key_Reference;
   function Iterate (Container : Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Selection;
      Position  : Cursor)
   return Constant_Reference_Type;
   function Is_Empty (Container : Selection) return Boolean;
   function Element (Item : Cursor) return Kit_Key_Reference;
   function Length (Container : Selection) return Natural;
   function Scan_By_Top_Record return Selection;

   function First_By_Top_Record (Top_Record : Record_Type)
      return Kit_Key_Reference;

   function Last_By_Top_Record (Top_Record : Record_Type)
      return Kit_Key_Reference;

   function Select_By_Top_Record (Top_Record : Record_Type) return Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Record_Type;
      Finish_Top_Record : Record_Type)
   return Selection;

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit_Root_Record_Reference) return Kit_Key_Reference;

   function Scan_By_Kit_Record return Selection;

   function First_By_Kit_Record (Kit_Record : Kit_Record_Reference)
      return Kit_Key_Reference;

   function Last_By_Kit_Record (Kit_Record : Kit_Record_Reference)
      return Kit_Key_Reference;

   function Select_By_Kit_Record (Kit_Record : Kit_Record_Reference)
      return Selection;

   function Scan_By_Record_Key return Selection;

   function Get_By_Record_Key
     (Kit_Record : Kit_Record_Reference;
      Name       : String)
   return Kit_Key_Reference;

   function Select_By_Record_Key
     (Kit_Record : Kit_Record_Reference;
      Name       : String)
   return Selection;

   function Select_Record_Key_Bounded_By_Name
     (Kit_Record  : Kit_Record_Reference;
      Start_Name  : String;
      Finish_Name : String)
   return Selection;

   function Is_Record_Key
     (Kit_Record : Kit_Record_Reference;
      Name       : String)
   return Boolean;

   function Get (Ref : Kit_Key_Reference) return Kit_Key_Type;
   function Get_Update (Ref : Kit_Key_Reference) return Kit_Key_Update;

   function First (Container : Selection) return Cursor;
   function Last (Container : Selection) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Previous (Position : Cursor) return Cursor;
   procedure Next (Position : in out Cursor);
   procedure Previous (Position : in out Cursor);

   type Kit_Key_Notify_Handler is access
     procedure (Reference : Kit_Key_Reference);
   type Kit_Key_Table_Notify_Handler is access
     procedure;

   procedure On_Kit_Key_Table_Change (Handler : Kit_Key_Table_Notify_Handler)
     ;
   procedure On_Kit_Key_Change
     (Reference : Kit_Key_Reference;
      Handler   : Kit_Key_Notify_Handler);
   procedure On_Kit_Key_Created (Handler : Kit_Key_Notify_Handler);
   procedure On_Kit_Key_Deleted (Handler : Kit_Key_Notify_Handler);
   procedure Set_Name
     (Item  : in out Kit_Key_Update_Interface;
      Value : String)
      is abstract;

   procedure Set_Kit_Record
     (Item  : in out Kit_Key_Update_Interface;
      Value : Kit_Record_Reference)
      is abstract;
   procedure Set_Kit_Record
     (Item  : in out Kit_Key_Update_Interface;
      Value : Kit.Db.Kit_Record.Kit_Record_Type)
      is abstract;

   procedure Set_Is_Unique
     (Item  : in out Kit_Key_Update_Interface;
      Value : Boolean)
      is abstract;

   procedure Set_Length
     (Item  : in out Kit_Key_Update_Interface;
      Value : Integer)
      is abstract;

private

   package List_Of_References is
     new Ada.Containers.Doubly_Linked_Lists
          (Kit_Key_Reference);
   type Cursor is
      record
         Current : List_Of_References.Cursor;
      end record;
   type Constant_Reference_Type
      (Element : not null access constant Kit_Key_Reference) is  null record;
   type Reference_Type (Element : not null access Kit_Key_Type) is
          null record;
   type Selection is tagged
      record
         Elements : List_Of_References.List;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Db.Kit_Key;
