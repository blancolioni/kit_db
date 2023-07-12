with Kit.Db.Marlowe_Keys;
with Kit.Db.Kit_Field;
with Kit.Db.Kit_Key;
with Kit.Db.Kit_Key_Field;
with Kit.Db.Kit_Record;
with Kit.Db.Kit_Record_Base;
with Kit.Db.Kit_Reference;
with Kit.Db.Kit_Type;
with Kit.Db.Database.Db_Kit_Root_Record;

package body Kit.Db.Database.Db_Kit_Record_Base is

   ------------
   -- Create --
   ------------

   procedure Create is
   begin
      Marlowe_Keys.Handle.Add_Table ("kit_record_base", 32);
      Marlowe_Keys.T3_Top_Record_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_record_base_top_record", Kit_Record_Base_Table_Index, 12);
      Marlowe_Keys.T3_T1_Idx_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_record_base_t1_idx", Kit_Record_Base_Table_Index, 16);
      Marlowe_Keys.T3_Base_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_record_base_base", Kit_Record_Base_Table_Index, 16);
      Marlowe_Keys.T3_Derived_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_record_base_derived", Kit_Record_Base_Table_Index, 16);
      Marlowe_Keys.T3_Base_Record_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_record_base_base_record", Kit_Record_Base_Table_Index, 24);
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Kit_Record_Base_Ref := Kit_Record.Create ("kit_record_base", 3, 32);
      Kit_Reference.Create
        (8,
         "kit_record_base",
         Kit_Record_Base_Ref);
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("kit_root_record");
      begin
         Kit_Field.Create
           ("kit_root_record",
            Kit_Record_Base_Ref,
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
           ("offset",
            Kit_Record_Base_Ref,
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
            ("kit_record");
      begin
         Kit_Field.Create
           ("base",
            Kit_Record_Base_Ref,
            Type_Ref,
            16,
            8,
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
           ("derived",
            Kit_Record_Base_Ref,
            Type_Ref,
            24,
            8,
            True,
            True,
            True,
            False,
            False);
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("t1_idx",
            Kit_Record_Base_Ref, True, 16);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Record_Base_Ref,
               "kit_root_record");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("base",
            Kit_Record_Base_Ref, False, 16);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Record_Base_Ref, "base");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("derived",
            Kit_Record_Base_Ref, False, 16);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Record_Base_Ref,
               "derived");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("base_record",
            Kit_Record_Base_Ref, False, 24);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Record_Base_Ref, "base");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Record_Base_Ref,
               "derived");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      Kit_Record_Base.Create
        (1,
         Db_Kit_Root_Record.Kit_Root_Record_Ref,
         Kit_Record_Base_Ref);
   end Initialize;

   ----------
   -- Open --
   ----------

   procedure Open is
   begin
      Marlowe_Keys.T3_Top_Record_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_record_base_top_record");
      Marlowe_Keys.T3_T1_Idx_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_record_base_t1_idx");
      Marlowe_Keys.T3_Base_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_record_base_base");
      Marlowe_Keys.T3_Derived_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_record_base_derived");
      Marlowe_Keys.T3_Base_Record_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_record_base_base_record");
   end Open;

end Kit.Db.Database.Db_Kit_Record_Base;
