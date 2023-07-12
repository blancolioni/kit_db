with Kit.Db.Marlowe_Keys;
with Kit.Db.Kit_Field;
with Kit.Db.Kit_Key;
with Kit.Db.Kit_Key_Field;
with Kit.Db.Kit_Record;
with Kit.Db.Kit_Record_Base;
with Kit.Db.Kit_Reference;
with Kit.Db.Kit_Type;
with Kit.Db.Database.Db_Kit_Root_Record;

package body Kit.Db.Database.Db_Kit_Field is

   ------------
   -- Create --
   ------------

   procedure Create is
   begin
      Marlowe_Keys.Handle.Add_Table ("kit_field", 105);
      Marlowe_Keys.T15_Top_Record_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_field_top_record", Kit_Field_Table_Index, 12);
      Marlowe_Keys.T15_T1_Idx_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_field_t1_idx", Kit_Field_Table_Index, 16);
      Marlowe_Keys.T15_Kit_Record_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_field_kit_record", Kit_Field_Table_Index, 16);
      Marlowe_Keys.T15_Record_Field_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_field_record_field", Kit_Field_Table_Index, 80);
      Marlowe_Keys.T15_Display_Field_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_field_display_field", Kit_Field_Table_Index, 17);
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Kit_Field_Ref := Kit_Record.Create ("kit_field", 15, 105);
      Kit_Reference.Create
        (8,
         "kit_field",
         Kit_Field_Ref);
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("kit_root_record");
      begin
         Kit_Field.Create
           ("kit_root_record",
            Kit_Field_Ref,
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
            Kit_Field_Ref,
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
            ("kit_record");
      begin
         Kit_Field.Create
           ("kit_record",
            Kit_Field_Ref,
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
            ("kit_type");
      begin
         Kit_Field.Create
           ("field_type",
            Kit_Field_Ref,
            Type_Ref,
            84,
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
           ("field_offset",
            Kit_Field_Ref,
            Type_Ref,
            92,
            4,
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
           ("field_length",
            Kit_Field_Ref,
            Type_Ref,
            96,
            4,
            True,
            True,
            True,
            False,
            False);
      end;
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("boolean");
      begin
         Kit_Field.Create
           ("created",
            Kit_Field_Ref,
            Type_Ref,
            100,
            1,
            True,
            True,
            True,
            False,
            False);
      end;
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("boolean");
      begin
         Kit_Field.Create
           ("readable",
            Kit_Field_Ref,
            Type_Ref,
            101,
            1,
            True,
            True,
            True,
            False,
            False);
      end;
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("boolean");
      begin
         Kit_Field.Create
           ("writeable",
            Kit_Field_Ref,
            Type_Ref,
            102,
            1,
            True,
            True,
            True,
            False,
            False);
      end;
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("boolean");
      begin
         Kit_Field.Create
           ("display",
            Kit_Field_Ref,
            Type_Ref,
            103,
            1,
            True,
            True,
            True,
            False,
            False);
      end;
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("boolean");
      begin
         Kit_Field.Create
           ("base_ref",
            Kit_Field_Ref,
            Type_Ref,
            104,
            1,
            True,
            True,
            True,
            False,
            False);
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("t1_idx",
            Kit_Field_Ref, True, 16);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Field_Ref,
               "kit_root_record");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("kit_record",
            Kit_Field_Ref, False, 16);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Field_Ref, "kit_record");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("record_field",
            Kit_Field_Ref, True, 80);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Field_Ref, "kit_record");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Field_Ref, "name");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("display_field",
            Kit_Field_Ref, False, 17);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Field_Ref, "kit_record");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Field_Ref, "display");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      Kit_Record_Base.Create
        (1,
         Db_Kit_Root_Record.Kit_Root_Record_Ref,
         Kit_Field_Ref);
   end Initialize;

   ----------
   -- Open --
   ----------

   procedure Open is
   begin
      Marlowe_Keys.T15_Top_Record_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_field_top_record");
      Marlowe_Keys.T15_T1_Idx_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_field_t1_idx");
      Marlowe_Keys.T15_Kit_Record_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_field_kit_record");
      Marlowe_Keys.T15_Record_Field_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_field_record_field");
      Marlowe_Keys.T15_Display_Field_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_field_display_field");
   end Open;

end Kit.Db.Database.Db_Kit_Field;
