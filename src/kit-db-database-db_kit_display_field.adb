with Kit.Db.Marlowe_Keys;
with Kit.Db.Kit_Field;
with Kit.Db.Kit_Key;
with Kit.Db.Kit_Key_Field;
with Kit.Db.Kit_Record;
with Kit.Db.Kit_Record_Base;
with Kit.Db.Kit_Reference;
with Kit.Db.Kit_Type;
with Kit.Db.Database.Db_Kit_Root_Record;

package body Kit.Db.Database.Db_Kit_Display_Field is

   ------------
   -- Create --
   ------------

   procedure Create is
   begin
      Marlowe_Keys.Handle.Add_Table ("kit_display_field", 28);
      Marlowe_Keys.T16_Top_Record_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_display_field_top_record", Kit_Display_Field_Table_Index, 12);
      Marlowe_Keys.T16_T1_Idx_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_display_field_t1_idx", Kit_Display_Field_Table_Index, 16);
      Marlowe_Keys.T16_Kit_Record_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_display_field_kit_record", Kit_Display_Field_Table_Index, 16);
      Marlowe_Keys.T16_Kit_Field_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_display_field_kit_field", Kit_Display_Field_Table_Index, 16);
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Kit_Display_Field_Ref := Kit_Record.Create ("kit_display_field", 16,
         28);
      Kit_Reference.Create
        (8,
         "kit_display_field",
         Kit_Display_Field_Ref);
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("kit_root_record");
      begin
         Kit_Field.Create
           ("kit_root_record",
            Kit_Display_Field_Ref,
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
            ("kit_record");
      begin
         Kit_Field.Create
           ("kit_record",
            Kit_Display_Field_Ref,
            Type_Ref,
            12,
            8,
            True,
            True,
            True,
            False,
            False);
      end;
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("kit_field");
      begin
         Kit_Field.Create
           ("kit_field",
            Kit_Display_Field_Ref,
            Type_Ref,
            20,
            8,
            True,
            True,
            True,
            False,
            False);
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("t1_idx",
            Kit_Display_Field_Ref, True, 16);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Display_Field_Ref,
               "kit_root_record");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("kit_record",
            Kit_Display_Field_Ref, False, 16);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Display_Field_Ref,
               "kit_record");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("kit_field",
            Kit_Display_Field_Ref, False, 16);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Display_Field_Ref,
               "kit_field");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      Kit_Record_Base.Create
        (1,
         Db_Kit_Root_Record.Kit_Root_Record_Ref,
         Kit_Display_Field_Ref);
   end Initialize;

   ----------
   -- Open --
   ----------

   procedure Open is
   begin
      Marlowe_Keys.T16_Top_Record_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_display_field_top_record");
      Marlowe_Keys.T16_T1_Idx_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_display_field_t1_idx");
      Marlowe_Keys.T16_Kit_Record_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_display_field_kit_record");
      Marlowe_Keys.T16_Kit_Field_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_display_field_kit_field");
   end Open;

end Kit.Db.Database.Db_Kit_Display_Field;
