with Kit.Db.Marlowe_Keys;
with Kit.Db.Kit_Field;
with Kit.Db.Kit_Key;
with Kit.Db.Kit_Key_Field;
with Kit.Db.Kit_Record;
with Kit.Db.Kit_Record_Base;
with Kit.Db.Kit_Reference;
with Kit.Db.Kit_Type;
with Kit.Db.Database.Db_Kit_Root_Record;

package body Kit.Db.Database.Db_Kit_Literal is

   ------------
   -- Create --
   ------------

   procedure Create is
   begin
      Marlowe_Keys.Handle.Add_Table ("kit_literal", 88);
      Marlowe_Keys.T14_Top_Record_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_literal_top_record", Kit_Literal_Table_Index, 12);
      Marlowe_Keys.T14_T1_Idx_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_literal_t1_idx", Kit_Literal_Table_Index, 16);
      Marlowe_Keys.T14_Kit_Enumeration_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_literal_kit_enumeration", Kit_Literal_Table_Index, 16);
      Marlowe_Keys.T14_Value_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_literal_value", Kit_Literal_Table_Index, 12);
      Marlowe_Keys.T14_Enum_Value_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_literal_enum_value", Kit_Literal_Table_Index, 20);
      Marlowe_Keys.T14_Enum_Name_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_literal_enum_name", Kit_Literal_Table_Index, 80);
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Kit_Literal_Ref := Kit_Record.Create ("kit_literal", 14, 88);
      Kit_Reference.Create
        (8,
         "kit_literal",
         Kit_Literal_Ref);
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("kit_root_record");
      begin
         Kit_Field.Create
           ("kit_root_record",
            Kit_Literal_Ref,
            Type_Ref,
            4,
            8,
            False,
            False,
            False,
            False,
            True);
      end;
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("bounded_string_64");
      begin
         Kit_Field.Create
           ("name",
            Kit_Literal_Ref,
            Type_Ref,
            12,
            64,
            True,
            True,
            True,
            False,
            False);
      end;
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("kit_enumeration");
      begin
         Kit_Field.Create
           ("kit_enumeration",
            Kit_Literal_Ref,
            Type_Ref,
            76,
            8,
            True,
            True,
            True,
            False,
            False);
      end;
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("natural");
      begin
         Kit_Field.Create
           ("value",
            Kit_Literal_Ref,
            Type_Ref,
            84,
            4,
            True,
            True,
            True,
            False,
            False);
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("t1_idx",
            Kit_Literal_Ref, True, 16);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Literal_Ref,
               "kit_root_record");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create
            ("kit_enumeration", Kit_Literal_Ref, False, 16);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Literal_Ref,
               "kit_enumeration");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("value",
            Kit_Literal_Ref, False, 12);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Literal_Ref, "value");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("enum_value",
            Kit_Literal_Ref, True, 20);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Literal_Ref,
               "kit_enumeration");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Literal_Ref, "value");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("enum_name",
            Kit_Literal_Ref, True, 80);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Literal_Ref,
               "kit_enumeration");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Literal_Ref, "name");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      Kit_Record_Base.Create
        (1,
         Db_Kit_Root_Record.Kit_Root_Record_Ref,
         Kit_Literal_Ref);
   end Initialize;

   ----------
   -- Open --
   ----------

   procedure Open is
   begin
      Marlowe_Keys.T14_Top_Record_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_literal_top_record");
      Marlowe_Keys.T14_T1_Idx_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_literal_t1_idx");
      Marlowe_Keys.T14_Kit_Enumeration_Ref :=
         Marlowe_Keys.Handle.Get_Reference ("kit_literal_kit_enumeration");
      Marlowe_Keys.T14_Value_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_literal_value");
      Marlowe_Keys.T14_Enum_Value_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_literal_enum_value");
      Marlowe_Keys.T14_Enum_Name_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_literal_enum_name");
   end Open;

end Kit.Db.Database.Db_Kit_Literal;
