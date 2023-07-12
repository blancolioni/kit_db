private with Ada.Containers.Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;
with Kit.Db.Kit_Root_Record;
with Kit.Db.Kit_Record;
with Kit.Db.Kit_Field;

package Kit.Db.Kit_Display_Field is

   F_Top_Record : constant String := "top_record";
   F_Kit_Root_Record : constant String := "kit_root_record";
   F_Kit_Record : constant String := "kit_record";
   F_Kit_Field : constant String := "kit_field";

   type Kit_Display_Field_Interface is limited interface
     and Record_Interface
     and Search_Interface
     and Kit.Db.Kit_Root_Record.Kit_Root_Record_Interface;

   subtype Kit_Display_Field_Type is Kit_Display_Field_Interface'Class;

   function Get_Kit_Display_Field_Reference (Item :
      Kit_Display_Field_Interface) return Kit_Display_Field_Reference
      is abstract;

   function Kit_Record (Item : Kit_Display_Field_Interface)
      return Kit_Record_Reference
      is abstract;

   function Kit_Field (Item : Kit_Display_Field_Interface)
      return Kit_Field_Reference
      is abstract;

   type Kit_Display_Field_Update_Interface is limited interface
     and Kit_Display_Field_Interface
     and Kit.Db.Kit_Root_Record.Kit_Root_Record_Update_Interface;

   subtype Kit_Display_Field_Update is
      Kit_Display_Field_Update_Interface'Class;
   function Create return Kit_Display_Field_Update;
   procedure Create
     (Kit_Record : Kit_Record_Reference;
      Kit_Field  : Kit_Field_Reference);
   function Create
     (Kit_Record : Kit_Record_Reference;
      Kit_Field  : Kit_Field_Reference)
   return Kit_Display_Field_Reference;

   type Cursor is private;
   function Has_Element (Item : Cursor) return Boolean;
   type Constant_Reference_Type
      (Element : not null access constant Kit_Display_Field_Reference) is
      private
   with Implicit_Dereference => Element;
   type Reference_Type (Element : not null access Kit_Display_Field_Type) is
      private
   with Implicit_Dereference => Element;
   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
          (Cursor,
         Has_Element);
   type Selection is tagged private
   with Constant_Indexing => Constant_Reference,
        Default_Iterator => Iterate,
        Iterator_Element => Kit_Display_Field_Reference;
   function Iterate (Container : Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;
   function Constant_Reference
     (Container : aliased Selection;
      Position  : Cursor)
   return Constant_Reference_Type;
   function Is_Empty (Container : Selection) return Boolean;
   function Element (Item : Cursor) return Kit_Display_Field_Reference;
   function Length (Container : Selection) return Natural;
   function Scan_By_Top_Record return Selection;

   function First_By_Top_Record (Top_Record : Record_Type)
      return Kit_Display_Field_Reference;

   function Last_By_Top_Record (Top_Record : Record_Type)
      return Kit_Display_Field_Reference;

   function Select_By_Top_Record (Top_Record : Record_Type) return Selection;

   function Select_Bounded_By_Top_Record
     (Start_Top_Record  : Record_Type;
      Finish_Top_Record : Record_Type)
   return Selection;

   function Get_From_Kit_Root_Record (Kit_Root_Record :
      Kit_Root_Record_Reference) return Kit_Display_Field_Reference;

   function Scan_By_Kit_Record return Selection;

   function First_By_Kit_Record (Kit_Record : Kit_Record_Reference)
      return Kit_Display_Field_Reference;

   function Last_By_Kit_Record (Kit_Record : Kit_Record_Reference)
      return Kit_Display_Field_Reference;

   function Select_By_Kit_Record (Kit_Record : Kit_Record_Reference)
      return Selection;

   function Scan_By_Kit_Field return Selection;

   function First_By_Kit_Field (Kit_Field : Kit_Field_Reference)
      return Kit_Display_Field_Reference;

   function Last_By_Kit_Field (Kit_Field : Kit_Field_Reference)
      return Kit_Display_Field_Reference;

   function Select_By_Kit_Field (Kit_Field : Kit_Field_Reference)
      return Selection;

   function Get (Ref : Kit_Display_Field_Reference)
      return Kit_Display_Field_Type;
   function Get_Update (Ref : Kit_Display_Field_Reference)
      return Kit_Display_Field_Update;

   function First (Container : Selection) return Cursor;
   function Last (Container : Selection) return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Previous (Position : Cursor) return Cursor;
   procedure Next (Position : in out Cursor);
   procedure Previous (Position : in out Cursor);

   type Kit_Display_Field_Notify_Handler is access
     procedure (Reference : Kit_Display_Field_Reference);
   type Kit_Display_Field_Table_Notify_Handler is access
     procedure;

   procedure On_Kit_Display_Field_Table_Change (Handler :
      Kit_Display_Field_Table_Notify_Handler);
   procedure On_Kit_Display_Field_Change
     (Reference : Kit_Display_Field_Reference;
      Handler   : Kit_Display_Field_Notify_Handler);
   procedure On_Kit_Display_Field_Created (Handler :
      Kit_Display_Field_Notify_Handler);
   procedure On_Kit_Display_Field_Deleted (Handler :
      Kit_Display_Field_Notify_Handler);
   procedure Set_Kit_Record
     (Item  : in out Kit_Display_Field_Update_Interface;
      Value : Kit_Record_Reference)
      is abstract;
   procedure Set_Kit_Record
     (Item  : in out Kit_Display_Field_Update_Interface;
      Value : Kit.Db.Kit_Record.Kit_Record_Type)
      is abstract;

   procedure Set_Kit_Field
     (Item  : in out Kit_Display_Field_Update_Interface;
      Value : Kit_Field_Reference)
      is abstract;
   procedure Set_Kit_Field
     (Item  : in out Kit_Display_Field_Update_Interface;
      Value : Kit.Db.Kit_Field.Kit_Field_Type)
      is abstract;

private

   package List_Of_References is
     new Ada.Containers.Doubly_Linked_Lists
          (Kit_Display_Field_Reference);
   type Cursor is
      record
         Current : List_Of_References.Cursor;
      end record;
   type Constant_Reference_Type
      (Element : not null access constant Kit_Display_Field_Reference) is
          null record;
   type Reference_Type (Element : not null access Kit_Display_Field_Type) is
          null record;
   type Selection is tagged
      record
         Elements : List_Of_References.List;
      end record;
   pragma Inline (Constant_Reference);

end Kit.Db.Kit_Display_Field;
