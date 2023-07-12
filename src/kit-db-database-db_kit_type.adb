with Kit.Db.Marlowe_Keys;
with Kit.Db.Kit_Field;
with Kit.Db.Kit_Key;
with Kit.Db.Kit_Key_Field;
with Kit.Db.Kit_Record;
with Kit.Db.Kit_Record_Base;
with Kit.Db.Kit_Reference;
with Kit.Db.Kit_Type;
with Kit.Db.Database.Db_Kit_Root_Record;

package body Kit.Db.Database.Db_Kit_Type is

   ------------
   -- Create --
   ------------

   procedure Create is
   begin
      Marlowe_Keys.Handle.Add_Table ("kit_type", 80);
      Marlowe_Keys.T4_Top_Record_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_type_top_record", Kit_Type_Table_Index, 12);
      Marlowe_Keys.T4_T1_Idx_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_type_t1_idx", Kit_Type_Table_Index, 16);
      Marlowe_Keys.T4_Name_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_type_name", Kit_Type_Table_Index, 72);
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Kit_Type_Ref := Kit_Record.Create ("kit_type", 4, 80);
      Kit_Reference.Create
        (8,
         "kit_type",
         Kit_Type_Ref);
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("kit_root_record");
      begin
         Kit_Field.Create
           ("kit_root_record",
            Kit_Type_Ref,
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
            ("natural");
      begin
         Kit_Field.Create
           ("size",
            Kit_Type_Ref,
            Type_Ref,
            12,
            4,
            True,
            True,
            True,
            False,
            False);
      end;
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("bounded_string_64");
      begin
         Kit_Field.Create
           ("name",
            Kit_Type_Ref,
            Type_Ref,
            16,
            64,
            True,
            True,
            True,
            False,
            False);
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("t1_idx",
            Kit_Type_Ref, True, 16);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Type_Ref,
               "kit_root_record");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("name",
            Kit_Type_Ref, True, 72);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Type_Ref, "name");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      Kit_Record_Base.Create
        (1,
         Db_Kit_Root_Record.Kit_Root_Record_Ref,
         Kit_Type_Ref);
   end Initialize;

   ----------
   -- Open --
   ----------

   procedure Open is
   begin
      Marlowe_Keys.T4_Top_Record_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_type_top_record");
      Marlowe_Keys.T4_T1_Idx_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_type_t1_idx");
      Marlowe_Keys.T4_Name_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_type_name");
   end Open;

end Kit.Db.Database.Db_Kit_Type;
