private with Ada.Containers.Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;

package Kit.Db.Kit_Root_Record is

   F_Top_Record : constant String := "top_record";

   type Kit_Root_Record_Interface is limited interface
     and Record_Interface
     and Search_Interface;

   subtype Kit_Root_Record_Type is Kit_Root_Record_Interface'Class;

   function Get_Kit_Root_Record_Reference (Item : Kit_Root_Record_Interface)
      return Kit_Root_Record_Reference
      is abstract;

   function Top_Record (Item : Kit_Root_Record_Interface) return Record_Type
      is abstract;

   type Kit_Root_Record_Update_Interface is limited interface
     and Kit_Root_Record_Interface;

   subtype Kit_Root_Record_Update is Kit_Root_Record_Update_Interface'Class;
   procedure Delete (Item : in out Kit_Root_Record_Update_Interface)
      is abstract;
   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   type Constant_Reference_Type
      (Element : not null access constant Kit_Root_Record_Reference) is
      private
   with Implicit_Dereference => Element;
   type Reference_Type (Element : not null access Kit_Root_Record_Type) is
      private
   with Implicit_Dereference => Element;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Root_Record_Reference;
   function Iterate (Container : Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Selection;
      Position  : Cursor)
   return Constant_Reference_Type;
   function Is_Empty (Container : Selection) return Boolean;
   function Element (Item : Cursor) return Kit_Root_Record_Reference;
   function Length (Container : Selection) return Natural;
   function Scan_By_Top_Record return Selection;

   function First_By_Top_Record (Top_Record : Record_Type)
      return Kit_Root_Record_Reference;

   function Last_By_Top_Record (Top_Record : Record_Type)
      return Kit_Root_Record_Reference;

   function Select_By_Top_Record (Top_Record : Record_Type) return Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Record_Type;
      Finish_Top_Record : Record_Type)
   return Selection;

   function Get (Ref : Kit_Root_Record_Reference)
      return Kit_Root_Record_Type;
   function Get_Update (Ref : Kit_Root_Record_Reference)
      return Kit_Root_Record_Update;

   function First (Container : Selection) return Cursor;
   function Last (Container : Selection) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Previous (Position : Cursor) return Cursor;
   procedure Next (Position : in out Cursor);
   procedure Previous (Position : in out Cursor);

   type Kit_Root_Record_Notify_Handler is access
     procedure (Reference : Kit_Root_Record_Reference);
   type Kit_Root_Record_Table_Notify_Handler is access
     procedure;

   procedure On_Kit_Root_Record_Table_Change (Handler :
      Kit_Root_Record_Table_Notify_Handler);
   procedure On_Kit_Root_Record_Change
     (Reference : Kit_Root_Record_Reference;
      Handler   : Kit_Root_Record_Notify_Handler);
   procedure On_Kit_Root_Record_Created (Handler :
      Kit_Root_Record_Notify_Handler);
   procedure On_Kit_Root_Record_Deleted (Handler :
      Kit_Root_Record_Notify_Handler);

private

   package List_Of_References is
     new Ada.Containers.Doubly_Linked_Lists
          (Kit_Root_Record_Reference);
   type Cursor is
      record
         Current : List_Of_References.Cursor;
      end record;
   type Constant_Reference_Type
      (Element : not null access constant Kit_Root_Record_Reference) is
          null record;
   type Reference_Type (Element : not null access Kit_Root_Record_Type) is
          null record;
   type Selection is tagged
      record
         Elements : List_Of_References.List;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Db.Kit_Root_Record;
