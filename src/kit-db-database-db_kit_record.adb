with Kit.Db.Marlowe_Keys;
with Kit.Db.Kit_Field;
with Kit.Db.Kit_Key;
with Kit.Db.Kit_Key_Field;
with Kit.Db.Kit_Record;
with Kit.Db.Kit_Record_Base;
with Kit.Db.Kit_Reference;
with Kit.Db.Kit_Type;
with Kit.Db.Database.Db_Kit_Root_Record;

package body Kit.Db.Database.Db_Kit_Record is

   ------------
   -- Create --
   ------------

   procedure Create is
   begin
      Marlowe_Keys.Handle.Add_Table ("kit_record", 84);
      Marlowe_Keys.T2_Top_Record_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_record_top_record", Kit_Record_Table_Index, 12);
      Marlowe_Keys.T2_T1_Idx_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_record_t1_idx", Kit_Record_Table_Index, 16);
      Marlowe_Keys.T2_Name_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_record_name", Kit_Record_Table_Index, 72);
      Marlowe_Keys.T2_Table_Index_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_record_table_index", Kit_Record_Table_Index, 12);
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Kit_Record_Ref := Kit_Record.Create ("kit_record", 2, 84);
      Kit_Reference.Create
        (8,
         "kit_record",
         Kit_Record_Ref);
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("kit_root_record");
      begin
         Kit_Field.Create
           ("kit_root_record",
            Kit_Record_Ref,
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
            Kit_Record_Ref,
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
            ("positive");
      begin
         Kit_Field.Create
           ("table_index",
            Kit_Record_Ref,
            Type_Ref,
            76,
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
           ("record_length",
            Kit_Record_Ref,
            Type_Ref,
            80,
            4,
            True,
            True,
            True,
            False,
            False);
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("t1_idx",
            Kit_Record_Ref, True, 16);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Record_Ref,
               "kit_root_record");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("name",
            Kit_Record_Ref, True, 72);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Record_Ref, "name");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("table_index",
            Kit_Record_Ref, True, 12);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Record_Ref, "table_index");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      Kit_Record_Base.Create
        (1,
         Db_Kit_Root_Record.Kit_Root_Record_Ref,
         Kit_Record_Ref);
   end Initialize;

   ----------
   -- Open --
   ----------

   procedure Open is
   begin
      Marlowe_Keys.T2_Top_Record_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_record_top_record");
      Marlowe_Keys.T2_T1_Idx_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_record_t1_idx");
      Marlowe_Keys.T2_Name_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_record_name");
      Marlowe_Keys.T2_Table_Index_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_record_table_index");
   end Open;

end Kit.Db.Database.Db_Kit_Record;
