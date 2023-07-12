with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Long_Float_Text_IO;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Marlowe.Data_Stores;
with Marlowe.Key_Storage;

with Kit.Handles.Kit_Display_Field;
with Kit.Handles.Kit_Enumeration;
with Kit.Handles.Kit_Field;
with Kit.Handles.Kit_Key;
with Kit.Handles.Kit_Key_Field;
with Kit.Handles.Kit_Literal;
with Kit.Handles.Kit_Record;
with Kit.Handles.Kit_Record_Base;
with Kit.Handles.Kit_Reference;
with Kit.Handles.Kit_Type;

with Kit.Db.Marlowe_Keys;

package body Kit.Db.Tables is

   package Reference_Vectors is
     new Ada.Containers.Vectors (Positive, Record_Reference);

   function Storage_To_String
     (Value : System.Storage_Elements.Storage_Array;
      Value_Type : Kit.Handles.Kit_Type.Kit_Type_Class)
      return String;

   procedure String_To_Storage
     (Value      : String;
      Value_Type : Kit.Handles.Kit_Type.Kit_Type_Class;
      Storage    : out System.Storage_Elements.Storage_Array);

   function Key_To_Storage
     (Table     : Database_Table'Class;
      Key_Name  : String;
      Key_Value : String;
      First     : Boolean)
      return System.Storage_Elements.Storage_Array;

   function Get_Key_Reference
     (Table     : Database_Table'Class;
      Key_Name  : String)
      return Marlowe.Data_Stores.Key_Reference;

   function Get_Kit_Key_Reference
     (Table     : Database_Table'Class;
      Key_Name  : String)
      return Kit.Handles.Kit_Key.Kit_Key_Class;

   function Get_Field_Table
     (In_Record  : Database_Record'Class;
      Field_Name : String)
      return Database_Table'Class;

   function Get_Field_Names
     (Table_Index : Positive)
      return String_Vectors.Vector;

   function Get_Key_Names
     (Table_Index : Positive)
      return String_Vectors.Vector;

   procedure Get_Fields
     (From_Record : Database_Record'Class);

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Item : in out Database_Record) is
   begin
      Item.Fields := new Database_Fields'(Item.Fields.all);
   end Adjust;

   -----------------
   -- Default_Key --
   -----------------

   function Default_Key
     (Table : Database_Table'Class)
     return String
   is
      use Kit.Handles.Kit_Record;
      use Kit.Handles.Kit_Key;
      Rec : constant Kit_Record_Handle := Get_By_Name (Table.Name);
      Key : constant Kit_Key_Class := First_By_Kit_Record (Rec);
   begin
      if Key.Has_Element then
         return Key.Name;
      else
         for Base of
           Kit.Handles.Kit_Record_Base.Select_By_Derived (Rec)
         loop
            declare
               Base_Key : constant Kit_Key_Class :=
                            First_By_Kit_Record (Base.Base);
            begin
               if Base_Key.Has_Element then
                  return Base_Key.Name;
               end if;
            end;
         end loop;
      end if;
      return "";
   end Default_Key;

   -----------------
   -- Field_Count --
   -----------------

   function Field_Count (Table : Database_Table'Class) return Natural is
   begin
      return Table.Fields.Last_Index;
   end Field_Count;

   -----------------
   -- Field_Count --
   -----------------

   function Field_Count
     (Rec : Database_Record'Class)
      return Natural
   is
   begin
      if not Rec.Fields.Got_Fields then
         Get_Fields (Rec);
      end if;
      return Rec.Fields.Field_Values.Last_Index;
   end Field_Count;

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name (Table : Database_Table'Class;
                        Index : Positive)
                        return String
   is
   begin
      return Table.Fields (Index);
   end Field_Name;

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name (Rec : Database_Record'Class;
                        Index : Positive)
                        return String
   is
   begin
      if not Rec.Fields.Got_Fields then
         Get_Fields (Rec);
      end if;
      return Rec.Fields.Field_Names.Element (Index);
   end Field_Name;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Item : in out Database_Record) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Database_Fields,
                                        Database_Fields_Access);
   begin
      Free (Item.Fields);
   end Finalize;

   ---------
   -- Get --
   ---------

   function Get
     (Table        : Database_Table'Class;
      Reference    : Record_Reference)
      return Database_Record
   is
      use System.Storage_Elements;
      use Kit.Handles.Kit_Record;
      Rec : constant Kit_Record_Class :=
              Get_By_Table_Index
                (Positive (Table.Index));
      Length  : constant Storage_Count :=
                  Storage_Count (Rec.Record_Length);
      Storage : Storage_Array (0 .. Length - 1);
   begin
      return Item : Database_Record do
         Item.Table := Table.Index;
         Item.Index := Marlowe.Database_Index (Reference);
         Item.Rec_Ref := Kit_Record_Reference (Item.Table);

         Marlowe_Keys.Handle.Get_Record
           (Table.Index, Item.Index, Storage'Address);
         Item.Value.Append (Storage);

         for Base of
           Kit.Handles.Kit_Record_Base.Select_By_Derived (Rec)
         loop
            declare
               Base_Record  : constant Kit_Record_Class := Base.Base;
               Length       : constant Storage_Count :=
                                Storage_Count (Base_Record.Record_Length);
               Base_Storage : Storage_Array (0 .. Length - 1);
               Index        : Marlowe.Database_Index;
               Base_Offset  : constant Storage_Offset :=
                                Storage_Offset ((Base.Offset - 1) * 8 + 4);
            begin
               Marlowe.Key_Storage.From_Storage
                 (Index, Storage (Base_Offset .. Base_Offset + 7));
               Marlowe_Keys.Handle.Get_Record
                 (Marlowe.Table_Index (Base_Record.Table_Index),
                  Index,
                  Base_Storage'Address);
               Item.Value.Append (Base_Storage);
            end;
         end loop;
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Table        : Database_Table'Class;
      Key_Name     : String;
      Key_Value    : String)
      return Database_Record
   is
      Ref : constant Marlowe.Data_Stores.Key_Reference :=
        Get_Key_Reference (Table, Key_Name);
      First : constant System.Storage_Elements.Storage_Array :=
        Key_To_Storage (Table, Key_Name, Key_Value, True);
      Last  : constant System.Storage_Elements.Storage_Array :=
        Key_To_Storage (Table, Key_Name, Key_Value, False);
      Mark  : constant Marlowe.Data_Stores.Data_Store_Cursor :=
                Marlowe_Keys.Handle.Search
                  (Ref,
                   First, Last,
                   Marlowe.Closed, Marlowe.Closed,
                   Marlowe.Forward);
   begin
      if Mark.Valid then
         declare
            Index : constant Marlowe.Database_Index :=
                      Marlowe.Key_Storage.To_Database_Index
                        (Mark.Get_Key);
         begin
            return Get (Table,
                        Record_Reference (Index));
         end;
      else
         return Item : Database_Record do
            Item.Table := Table.Index;
            Item.Index := 0;
         end return;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (From_Record : Database_Record'Class;
      Field_Name  : String)
      return String
   is
      function Get_Single_Field (Name : String) return String;

      ----------------------
      -- Get_Single_Field --
      ----------------------

      function Get_Single_Field (Name : String) return String is
      begin
         for I in 1 .. From_Record.Fields.Field_Names.Last_Index loop
            if From_Record.Fields.Field_Names (I) = Name then
               return From_Record.Fields.Field_Values (I);
            end if;
         end loop;
         raise Constraint_Error with
           Name & ": no such field";
      end Get_Single_Field;

      First_Point : constant Natural :=
                      Ada.Strings.Fixed.Index (Field_Name, ".");
   begin
      if not From_Record.Fields.Got_Fields then
         Get_Fields (From_Record);
      end if;

      if First_Point = 0 then
         return Get_Single_Field (Field_Name);
      else
         declare
            Ref_Field : constant String :=
                          Field_Name (Field_Name'First .. First_Point - 1);
            Ref_Value : constant String :=
                          Get_Single_Field (Ref_Field);
            Ref       : constant Record_Reference :=
                          Record_Reference'Value (Ref_Value);
            Child_Table : constant Database_Table'Class :=
                            Get_Field_Table (From_Record, Ref_Field);
            Child       : constant Database_Record :=
                            Child_Table.Get (Ref);
         begin
            return Child.Get (Field_Name (First_Point + 1 .. Field_Name'Last));
         end;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (From_Record : Database_Record'Class;
      Field_Index : Positive)
      return String
   is
   begin
      if not From_Record.Fields.Got_Fields then
         Get_Fields (From_Record);
      end if;

      return From_Record.Fields.Field_Values (Field_Index);
   end Get;

   ---------------------
   -- Get_Field_Names --
   ---------------------

   function Get_Field_Names
     (Table_Index : Positive)
      return String_Vectors.Vector
   is
      Result : String_Vectors.Vector;
      Rec    : constant Kit.Handles.Kit_Record.Kit_Record_Class :=
                 Kit.Handles.Kit_Record.Get_By_Table_Index (Table_Index);
   begin
      for Rec_Base of
        Kit.Handles.Kit_Record_Base.Select_By_Derived (Rec)
      loop
         for Field of
           Kit.Handles.Kit_Field.Select_By_Kit_Record (Rec_Base.Base)
         loop
            if (Field.Readable or else Field.Writeable)
              and then not Field.Base_Ref
            then
               Result.Append (Field.Name);
            end if;
         end loop;
      end loop;

      for Field of
        Kit.Handles.Kit_Field.Select_By_Kit_Record (Rec)
      loop
         if (Field.Readable or else Field.Writeable)
           and then not Field.Base_Ref
         then
            Result.Append (Field.Name);
         end if;
      end loop;
      return Result;
   end Get_Field_Names;

   ---------------------
   -- Get_Field_Table --
   ---------------------

   function Get_Field_Table
     (In_Record  : Database_Record'Class;
      Field_Name : String)
      return Database_Table'Class
   is
      use Kit.Handles.Kit_Record, Kit.Handles.Kit_Field;
      Rec : constant Kit.Handles.Kit_Record.Kit_Record_Class :=
              Get_By_Table_Index
                (Positive (In_Record.Table));
      pragma Assert (Rec.Has_Element);
      Field  : constant Kit_Field_Class :=
                 Get_By_Record_Field
                   (Rec, Field_Name);
   begin
      if Field.Field_Type.Has_Element then
         return Get_Table (Field.Field_Type.Name);
      else
         for Base of
           Kit.Handles.Kit_Record_Base.Select_By_Derived (Rec)
         loop
            declare
               Base_Field : constant Kit_Field_Class :=
                              Get_By_Record_Field (Base.Base, Field_Name);
            begin
               if Base_Field.Has_Element then
                  return Get_Table (Base_Field.Field_Type.Name);
               end if;
            end;
         end loop;

         raise Constraint_Error
           with "no field '" & Field_Name & "' for record "
           & Rec.Name;
      end if;

   end Get_Field_Table;

   --------------------
   -- Get_Field_Type --
   --------------------

   function Get_Field_Type
     (From_Record : Database_Record'Class;
      Field_Name  : String)
      return Database_Field_Type
   is
      Start : Positive := Field_Name'First;
      First_Dot : Natural :=
                    Ada.Strings.Fixed.Index (Field_Name, ".");
      Last      : Positive :=
                    (if First_Dot = 0 then Field_Name'Last else First_Dot - 1);
      Rec_Table   : Marlowe.Table_Index := From_Record.Table;
      Rec_Ref     : Record_Reference := From_Record.Reference;
      Field_Type  : Kit.Handles.Kit_Type.Kit_Type_Handle;
   begin
      loop
         if First_Dot > 0 then
            declare
               Table : constant Database_Table :=
                         (Index  => Rec_Table,
                          Fields => String_Vectors.Empty_Vector,
                          Keys   => String_Vectors.Empty_Vector);
               Rec   : constant Database_Record'Class :=
                         Get (Table, Rec_Ref);
            begin
               Rec_Table :=
                 Get_Field_Table (Rec, Field_Name (Start .. Last)).Index;
               Rec_Ref :=
                 Record_Reference'Value
                   (Rec.Get (Field_Name (Start .. Last)));
               Start := First_Dot + 1;
               First_Dot :=
                 Ada.Strings.Fixed.Index (Field_Name, ".", Start);
               Last := (if First_Dot = 0
                        then Field_Name'Last
                        else First_Dot - 1);
            end;
         else
            declare
               Rec : constant Kit.Handles.Kit_Record.Kit_Record_Class :=
                       Kit.Handles.Kit_Record.Get_By_Table_Index
                         (Positive (Rec_Table));
               Field : constant Kit.Handles.Kit_Field.Kit_Field_Class :=
                         Kit.Handles.Kit_Field.Get_By_Record_Field
                           (Rec, Field_Name (Start .. Last));
            begin
               if Field.Has_Element then
                  Field_Type :=
                    Kit.Handles.Kit_Type.Get
                      (Field.Field_Type.Reference_Kit_Type);
               else
                  for Base of
                    Kit.Handles.Kit_Record_Base.Select_By_Derived (Rec)
                  loop
                     declare
                        Base_Field : constant Kit.Handles.Kit_Field
                          .Kit_Field_Class :=
                            Kit.Handles.Kit_Field.Get_By_Record_Field
                              (Base.Base, Field_Name (Start .. Last));
                     begin
                        if Base_Field.Has_Element then
                           Field_Type :=
                             Kit.Handles.Kit_Type.Get
                               (Base_Field.Field_Type.Reference_Kit_Type);
                           exit;
                        end if;
                     end;
                  end loop;

                  if not Field_Type.Has_Element then
                     raise Constraint_Error
                       with "no field '" & Field_Name
                       & "' for record "
                          & Rec.Name;
                  end if;
               end if;
            end;
            exit;
         end if;
      end loop;

      case Field_Type.Top_Record is
         when R_Kit_Integer =>
            return Integer_Type;
         when R_Kit_Float =>
            return Float_Type;
         when R_Kit_Long_Float =>
            return Long_Float_Type;
         when R_Kit_String | R_Kit_Fixed_String | R_Kit_Bounded_String =>
            return String_Type;
         when R_Kit_Reference =>
            return Reference_Type;
         when R_Kit_Enumeration =>
            return Enumerated_Type;
         when others =>
            declare
               Message : constant String :=
                           "bad field type: "
                           & Record_Type'Image (Field_Type.Top_Record);
            begin
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Get_Field_Type: " & Message);
               raise Constraint_Error
                 with Message;
            end;
      end case;
   end Get_Field_Type;

   ----------------
   -- Get_Fields --
   ----------------

   procedure Get_Fields
     (From_Record : Database_Record'Class)
   is

      use System.Storage_Elements;
      use Kit.Handles.Kit_Record;
      Rec : constant Kit_Record_Handle :=
              Get_By_Table_Index
                (Positive (From_Record.Table));
      pragma Assert (Rec.Has_Element);
      Store_Index : Positive := 2;
   begin

      for Base of
        Kit.Handles.Kit_Record_Base.Select_By_Derived (Rec)
      loop
         for Field of
           Kit.Handles.Kit_Field.Select_By_Kit_Record (Base.Base)
         loop
            if Field.Readable and then not Field.Base_Ref then
               declare
                  Start       : constant Storage_Offset :=
                                  Storage_Offset (Field.Field_Offset);
                  Last        : constant Storage_Offset :=
                                  Start +
                                    Storage_Offset
                                      (Field.Field_Length) - 1;
                  Field_Type  : constant Kit.Handles.Kit_Type.Kit_Type_Class :=
                                  Field.Field_Type;
                  Field_Value : constant String :=
                                  Storage_To_String
                                    (From_Record.Value.Element
                                       (Store_Index) (Start .. Last),
                                     Field_Type);
               begin
                  From_Record.Fields.Field_Names.Append (Field.Name);
                  From_Record.Fields.Field_Values.Append (Field_Value);
               end;
            end if;
         end loop;
         Store_Index := Store_Index + 1;
      end loop;

      for Field of Kit.Handles.Kit_Field.Select_By_Kit_Record (Rec) loop
         if Field.Readable and then not Field.Base_Ref then
            declare
               Start       : constant Storage_Offset :=
                               Storage_Offset (Field.Field_Offset);
               Last        : constant Storage_Offset :=
                               Start +
                                 Storage_Offset (Field.Field_Length) - 1;
               Field_Type  : constant Kit.Handles.Kit_Type.Kit_Type_Class :=
                               Field.Field_Type;
               Field_Value : constant String :=
                               Storage_To_String
                                 (From_Record.Value.Element
                                    (1) (Start .. Last),
                                  Field_Type);
            begin
               From_Record.Fields.Field_Names.Append (Field.Name);
               From_Record.Fields.Field_Values.Append (Field_Value);
            end;
         end if;
      end loop;
      From_Record.Fields.Got_Fields := True;
   end Get_Fields;

   -------------------
   -- Get_Key_Names --
   -------------------

   function Get_Key_Names
     (Table_Index : Positive)
      return String_Vectors.Vector
   is
      Result : String_Vectors.Vector;
      Rec  : constant Kit.Handles.Kit_Record.Kit_Record_Handle :=
                   Kit.Handles.Kit_Record.Get_By_Table_Index (Table_Index);
      pragma Assert (Rec.Has_Element);
   begin
      for Rec_Base of
        Kit.Handles.Kit_Record_Base.Select_By_Derived (Rec)
      loop
         for Key of
           Kit.Handles.Kit_Key.Select_By_Kit_Record (Rec_Base.Base)
         loop
            Result.Append
              (Key.Name);
         end loop;
      end loop;

      for Key of
        Kit.Handles.Kit_Key.Select_By_Kit_Record (Rec)
      loop
         Result.Append (Key.Name);
      end loop;

      return Result;

   end Get_Key_Names;

   -----------------------
   -- Get_Key_Reference --
   -----------------------

   function Get_Key_Reference
     (Table     : Database_Table'Class;
      Key_Name  : String)
      return Marlowe.Data_Stores.Key_Reference
   is
      Key_Full_Name : constant String := Table.Name & "_" & Key_Name;
   begin
      return Marlowe_Keys.Handle.Get_Reference (Key_Full_Name);
   end Get_Key_Reference;

   ---------------------------
   -- Get_Kit_Key_Reference --
   ---------------------------

   function Get_Kit_Key_Reference
     (Table     : Database_Table'Class;
      Key_Name  : String)
      return Kit.Handles.Kit_Key.Kit_Key_Class
   is
      Rec    : constant Kit.Handles.Kit_Record.Kit_Record_Handle :=
                 Kit.Handles.Kit_Record.Get_By_Table_Index
                   (Positive (Table.Index));
      Key_Class : constant Kit.Handles.Kit_Key.Kit_Key_Class :=
                    Kit.Handles.Kit_Key.Get_By_Record_Key
                      (Rec, Key_Name);
   begin
      if Key_Class.Has_Element then
         return Key_Class;
      end if;

      for Base of Kit.Handles.Kit_Record_Base.Select_By_Derived (Rec) loop
         declare
            Base_Key : constant Kit.Handles.Kit_Key.Kit_Key_Class :=
                         Kit.Handles.Kit_Key.Get_By_Record_Key
                           (Base.Base, Key_Name);
         begin
            if Base_Key.Has_Element then
               return Base_Key;
            end if;
         end;
      end loop;
      raise Constraint_Error with
        "no such key '" & Key_Name & "' in table '" & Table.Name & "'";
   end Get_Kit_Key_Reference;

   ---------------
   -- Get_Table --
   ---------------

   function Get_Table
     (Table_Name : String)
      return Database_Table
   is
      Rec : constant Kit.Handles.Kit_Record.Kit_Record_Handle :=
                  Kit.Handles.Kit_Record.Get_By_Name (Table_Name);
   begin
      if Rec.Has_Element then
         return (Index  => Marlowe.Table_Index (Rec.Table_Index),
                 Fields => Get_Field_Names (Rec.Table_Index),
                 Keys   => Get_Key_Names (Rec.Table_Index));
      else
         return (Index  => 0,
                 Fields => String_Vectors.Empty_Vector,
                 Keys   => String_Vectors.Empty_Vector);
      end if;
   end Get_Table;

   ---------------
   -- Get_Table --
   ---------------

   function Get_Table (Table_Index : Marlowe.Table_Index)
                       return Database_Table
   is
   begin
      return (Index => Table_Index,
              Fields =>
                Get_Field_Names
                  (Positive (Table_Index)),
              Keys   =>
                Get_Key_Names
                  (Positive (Table_Index)));
   end Get_Table;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Table : Database_Table'Class) return Boolean is
      use type Marlowe.Table_Index;
   begin
      return Table.Index /= 0;
   end Has_Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Rec : Database_Record'Class) return Boolean is
      use type Marlowe.Database_Index;
   begin
      return Rec.Index /= 0;
   end Has_Element;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Item : in out Database_Record) is
   begin
      Item.Fields := new Database_Fields;
   end Initialize;

   --------------
   -- Is_Float --
   --------------

   function Is_Float (Field_Type : Database_Field_Type) return Boolean is
   begin
      return Field_Type = Float_Type;
   end Is_Float;

   ----------------
   -- Is_Integer --
   ----------------

   function Is_Integer (Field_Type : Database_Field_Type) return Boolean is
   begin
      return Field_Type = Integer_Type;
   end Is_Integer;

   ------------
   -- Is_Key --
   ------------

   function Is_Key (Table : Database_Table'Class;
                    Name  : String)
                    return Boolean
   is
   begin
      return Table.Keys.Contains (Name);
   end Is_Key;

   -------------------
   -- Is_Long_Float --
   -------------------

   function Is_Long_Float (Field_Type : Database_Field_Type) return Boolean is
   begin
      return Field_Type = Long_Float_Type;
   end Is_Long_Float;

   ------------------
   -- Is_Reference --
   ------------------

   function Is_Reference (Field_Type : Database_Field_Type) return Boolean is
   begin
      return Field_Type = Reference_Type;
   end Is_Reference;

   ---------------
   -- Is_String --
   ---------------

   function Is_String (Field_Type : Database_Field_Type) return Boolean is
   begin
      return Field_Type = String_Type;
   end Is_String;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Table        : Database_Table'Class;
      Key_Name     : String;
      Process      : not null access procedure
        (Item : Database_Record'Class))
   is
      Ref : constant Marlowe.Data_Stores.Key_Reference :=
              Get_Key_Reference (Table, Key_Name);
      Mark : Marlowe.Data_Stores.Data_Store_Cursor :=
               Marlowe_Keys.Handle.Search (Ref, Marlowe.Forward);
   begin
      while Mark.Valid loop
         declare
            Index : constant Marlowe.Database_Index :=
                      Marlowe.Key_Storage.To_Database_Index
                        (Mark.Get_Key);
            Rec   : constant Database_Record :=
                      Get (Table,
                           Record_Reference (Index));
         begin
            Process (Rec);
         end;
         Mark.Next;
      end loop;
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Table        : Database_Table'Class;
      Key_Name     : String;
      Key_Value    : String;
      Process      : not null access procedure
        (Item : Database_Record'Class))
   is
      Ref : constant Marlowe.Data_Stores.Key_Reference :=
              Get_Key_Reference (Table, Key_Name);
      First : constant System.Storage_Elements.Storage_Array :=
        Key_To_Storage (Table, Key_Name, Key_Value, True);
      Last : constant System.Storage_Elements.Storage_Array :=
                Key_To_Storage (Table, Key_Name, Key_Value, False);
      Mark  : Marlowe.Data_Stores.Data_Store_Cursor :=
                Marlowe_Keys.Handle.Search
                  (Ref,
                   First, Last,
                   Marlowe.Closed, Marlowe.Closed,
                   Marlowe.Forward);
   begin
      while Mark.Valid loop
         declare
            Index : constant Marlowe.Database_Index :=
                      Marlowe.Key_Storage.To_Database_Index
                        (Mark.Get_Key);
            Rec   : constant Database_Record :=
                      Get (Table,
                           Record_Reference (Index));
         begin
            Process (Rec);
         end;
         Mark.Next;
      end loop;
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Table        : Database_Table'Class;
      Key_Name     : String;
      First        : String;
      Last         : String;
      Process      : not null access procedure
        (Item : Database_Record'Class))
   is
      Ref : constant Marlowe.Data_Stores.Key_Reference :=
              Get_Key_Reference (Table, Key_Name);
      First_Storage : constant System.Storage_Elements.Storage_Array :=
                        Key_To_Storage (Table, Key_Name, First, True);
      Last_Storage : constant System.Storage_Elements.Storage_Array :=
                        Key_To_Storage (Table, Key_Name, Last, False);
      Mark  : Marlowe.Data_Stores.Data_Store_Cursor :=
                Marlowe_Keys.Handle.Search
                  (Ref,
                   First_Storage, Last_Storage,
                   Marlowe.Closed, Marlowe.Closed,
                   Marlowe.Forward);
   begin
      while Mark.Valid loop
         declare
            Index : constant Marlowe.Database_Index :=
                      Marlowe.Key_Storage.To_Database_Index
                        (Mark.Get_Key);
            Rec   : constant Database_Record :=
                      Get (Table,
                           Record_Reference (Index));
         begin
            Process (Rec);
         end;
         Mark.Next;
      end loop;
   end Iterate;

   --------------------
   -- Key_To_Storage --
   --------------------

   function Key_To_Storage
     (Table     : Database_Table'Class;
      Key_Name  : String;
      Key_Value : String;
      First     : Boolean)
      return System.Storage_Elements.Storage_Array
   is
      use System.Storage_Elements;
      Rec : constant Kit.Handles.Kit_Record.Kit_Record_Handle :=
              Kit.Handles.Kit_Record.Get_By_Table_Index
                (Positive (Table.Index));
      pragma Assert (Rec.Has_Element);
      Key : constant Kit.Handles.Kit_Key.Kit_Key_Class :=
              Get_Kit_Key_Reference (Table, Key_Name);
      pragma Assert (Key.Has_Element);
      Result : Storage_Array (1 .. Storage_Count (Key.Length));
      Start  : Storage_Offset := 1;
   begin
      if First then
         Result := (others => 0);
      else
         Result := (others => Storage_Element'Last);
      end if;
      for Key_Field of
        Kit.Handles.Kit_Key_Field.Select_By_Kit_Key (Key)
      loop
         declare
            Field     : constant Kit.Handles.Kit_Field.Kit_Field_Class :=
                          Key_Field.Kit_Field;
            Field_Type : constant Kit.Handles.Kit_Type.Kit_Type_Class :=
                           Field.Field_Type;
            Last       : constant Storage_Offset :=
                           Start + Storage_Count (Field.Field_Length) - 1;
         begin
            String_To_Storage
              (Value      => Key_Value,
               Value_Type => Field_Type,
               Storage    => Result (Start .. Last));
            Start := Last + 1;
         end;
      end loop;
      return Result;
   end Key_To_Storage;

   ----------
   -- Name --
   ----------

   function Name (Table : Database_Table'Class) return String is
      Rec : constant Kit.Handles.Kit_Record.Kit_Record_Handle :=
              Kit.Handles.Kit_Record.Get_By_Table_Index
                (Natural (Table.Index));
   begin
      return Rec.Name;
   end Name;

   ---------------
   -- Reference --
   ---------------

   function Reference (Rec : Database_Record'Class) return Record_Reference is
   begin
      return Record_Reference (Rec.Index);
   end Reference;

   ---------------
   -- Select_By --
   ---------------

   function Select_By
     (Table        : Database_Table'Class;
      Key_Name     : String;
      Key_Value    : String)
      return Array_Of_References
   is
      Ref : constant Marlowe.Data_Stores.Key_Reference :=
              Get_Key_Reference (Table, Key_Name);
      First : constant System.Storage_Elements.Storage_Array :=
        Key_To_Storage (Table, Key_Name, Key_Value, True);
      Last : constant System.Storage_Elements.Storage_Array :=
        Key_To_Storage (Table, Key_Name, Key_Value, False);
      Mark  : Marlowe.Data_Stores.Data_Store_Cursor :=
                Marlowe_Keys.Handle.Search
                  (Ref,
                   First, Last,
                   Marlowe.Closed, Marlowe.Closed,
                   Marlowe.Forward);
      Refs  : Reference_Vectors.Vector;
   begin
      while Mark.Valid loop
         declare
            Index : constant Marlowe.Database_Index :=
                      Marlowe.Key_Storage.To_Database_Index
                        (Mark.Get_Key);
         begin
            Refs.Append (Record_Reference (Index));
         end;
         Mark.Next;
      end loop;

      declare
         Result : Array_Of_References (1 .. Refs.Last_Index);
      begin
         for I in Result'Range loop
            Result (I) := Refs (I);
         end loop;
         return Result;
      end;
   end Select_By;

   -----------------------
   -- Storage_To_String --
   -----------------------

   function Storage_To_String
     (Value : System.Storage_Elements.Storage_Array;
      Value_Type : Kit.Handles.Kit_Type.Kit_Type_Class)
      return String
   is
   begin
      case Value_Type.Top_Record is
         when R_Kit_Integer =>
            declare
               X : Integer;
            begin
               Marlowe.Key_Storage.From_Storage (X, Value);
               return Integer'Image (X);
            end;
         when R_Kit_Long_Integer =>
            declare
               X : Integer_64;
            begin
               Integer_64_Storage.From_Storage (X, Value);
               return Integer_64'Image (X);
            end;
         when R_Kit_Float =>
            declare
               X      : Float;
               Buffer : String (1 .. 32);
            begin
               Marlowe.Key_Storage.From_Storage (X, Value);
               if X = 0.0 then
                  return "0";
               elsif abs X < 1.0E10
                 and then abs X > 1.0E-5
               then
                  Ada.Long_Float_Text_IO.Put
                    (To   => Buffer,
                     Item => Long_Float (X),
                     Aft  => 4,
                     Exp  => 0);

                  declare
                     Result : constant String :=
                                Ada.Strings.Fixed.Trim
                                  (Buffer, Ada.Strings.Both);
                     Last   : Natural := Result'Last;
                  begin
                     while Last >= Result'First
                       and then Result (Last) = '0'
                     loop
                        Last := Last - 1;
                     end loop;
                     if Last > 12 then
                        Last := 12;
                     end if;
                     if Result (Last) = '.' then
                        Last := Last - 1;
                     end if;
                     return Result (Result'First .. Last);
                  end;
               else
                  return Ada.Strings.Fixed.Trim (Float'Image (X),
                                                 Ada.Strings.Left);
               end if;
            end;
         when R_Kit_Long_Float =>
            declare
               X : Long_Float;
               Buffer : String (1 .. 32);
            begin
               Marlowe.Key_Storage.From_Storage (X, Value);
               if X = 0.0 then
                  return "0";
               elsif abs X < 1.0E10
                 and then abs X > 1.0E-5
               then
                  Ada.Long_Float_Text_IO.Put
                    (To   => Buffer,
                     Item => X,
                     Aft  => 8,
                     Exp  => 0);

                  declare
                     Result : constant String :=
                                Ada.Strings.Fixed.Trim
                                  (Buffer, Ada.Strings.Both);
                     Last   : Natural := Result'Last;
                  begin
                     while Last >= Result'First
                       and then Result (Last) = '0'
                     loop
                        Last := Last - 1;
                     end loop;
                     if Last > 12 then
                        Last := 12;
                     end if;
                     if Result (Last) = '.' then
                        Last := Last - 1;
                     end if;
                     return Result (Result'First .. Last);
                  end;
               else
                  return Ada.Strings.Fixed.Trim (Long_Float'Image (X),
                                                 Ada.Strings.Left);
               end if;
            end;
         when R_Kit_Bounded_String =>
            declare
               X : String (1 .. Value'Length);
               Last : Natural;
            begin
               Marlowe.Key_Storage.Bounded_String_From_Storage
                 (X, Last, Value);
               return X (1 .. Last);
            end;
         when Kit.Db.R_Kit_Fixed_String =>
            declare
               X     : String (1 .. Natural (Value'Length));
               Index : Natural := 0;
            begin
               for Unit of Value loop
                  Index := Index + 1;
                  X (Index) := Character'Val (Unit);
               end loop;
               return X;
            end;
         when R_Kit_Enumeration =>
            declare
               use Kit.Handles.Kit_Enumeration;
               X    : Marlowe.Key_Storage.Unsigned_Integer := 0;
               Enum : constant Kit_Enumeration_Handle :=
                        Get_By_Name (Value_Type.Name);
            begin
               Marlowe.Key_Storage.From_Storage (X, Value);

               declare
                  use Kit.Handles.Kit_Literal;
                  Lit : constant Kit_Literal_Handle :=
                          Get_By_Enum_Value
                            (Enum, Natural (X));
               begin
                  return Lit.Name;
               end;
            end;
         when R_Kit_Reference =>
            declare
               use Kit.Handles.Kit_Reference;
               use Kit.Handles.Kit_Display_Field;
               use type Marlowe.Database_Index;
               Ref_Type : constant Kit_Reference_Handle :=
                            Get_From_Kit_Type (Value_Type);
               Display_Field : constant Kit_Display_Field_Class :=
                                 First_By_Kit_Record (Ref_Type.Reference);
                                 --  Kit_Display_Field.First_By_Kit_Record
                                 --    (Kit_Reference.Get (Ref).Reference);
               Index      : Marlowe.Database_Index;
            begin
               Marlowe.Key_Storage.From_Storage (Index, Value);
               if Display_Field.Has_Element then
                  if Index = 0 then
                     return "";
                  else
                     declare
                        Table         : constant Database_Table :=
                                          Get_Table
                                            (Marlowe.Table_Index
                                               (Ref_Type.Reference
                                                .Table_Index));
                        Rec   : constant Database_Record :=
                                          Table.Get (Record_Reference (Index));
                        Field           : constant Kit.Handles.Kit_Field
                          .Kit_Field_Class :=
                            Display_Field.Kit_Field;
                     begin
                        return Rec.Get (Field.Name);
                     end;
                  end if;
               else
                  return Marlowe.Database_Index'Image (Index);
               end if;
            end;

         when others =>
            return Marlowe.Key_Storage.Image (Value);
      end case;
   end Storage_To_String;

   -----------------------
   -- String_To_Storage --
   -----------------------

   procedure String_To_Storage
     (Value      : String;
      Value_Type : Kit.Handles.Kit_Type.Kit_Type_Class;
      Storage    : out System.Storage_Elements.Storage_Array)
   is
   begin
      case Value_Type.Top_Record is
         when R_Kit_Integer =>
            declare
               X : constant Integer := Integer'Value (Value);
            begin
               Marlowe.Key_Storage.To_Storage (X, Storage);
            end;
         when R_Kit_Float =>
            declare
               X : constant Float := Float'Value (Value);
            begin
               Marlowe.Key_Storage.To_Storage (X, Storage);
            end;
         when R_Kit_Long_Float =>
            declare
               X : constant Long_Float := Long_Float'Value (Value);
            begin
               Marlowe.Key_Storage.To_Storage (X, Storage);
            end;
         when R_Kit_String =>
            Marlowe.Key_Storage.Bounded_String_To_Storage (Value, Storage);
         when Kit.Db.R_Kit_Fixed_String =>
            declare
               Index : Natural := Value'First - 1;
            begin
               for Unit of Storage loop
                  Index := Index + 1;
                  Unit := Character'Pos (Value (Index));
               end loop;
            end;
         when R_Kit_Enumeration =>
            declare
               use Kit.Handles.Kit_Enumeration;
               use Kit.Handles.Kit_Literal;
               Enum : constant Kit_Enumeration_Handle :=
                        Kit.Handles.Kit_Enumeration.Get_By_Name
                          (Value_Type.Name);
               Lit : constant Kit_Literal_Handle :=
                        Get_By_Enum_Name
                          (Enum, Value);
               X   : constant Marlowe.Key_Storage.Unsigned_Integer :=
                       Marlowe.Key_Storage.Unsigned_Integer (Lit.Value);
            begin
               Marlowe.Key_Storage.To_Storage (X, Storage);
            end;
         when R_Kit_Reference =>
            declare
               Index : constant Marlowe.Database_Index :=
                         Marlowe.Database_Index'Value (Value);
            begin
               Marlowe.Key_Storage.To_Storage (Index, Storage);
            end;
         when others =>
            Storage := (others => 0);
      end case;
   end String_To_Storage;

   ---------------
   -- To_String --
   ---------------

   function To_String (Reference : Record_Reference) return String is
      Result : constant String := Record_Reference'Image (Reference);
   begin
      return Result (2 .. Result'Last);
   end To_String;

end Kit.Db.Tables;
