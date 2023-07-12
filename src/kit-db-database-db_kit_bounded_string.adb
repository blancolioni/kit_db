with Kit.Db.Marlowe_Keys;
with Kit.Db.Kit_Field;
with Kit.Db.Kit_Key;
with Kit.Db.Kit_Key_Field;
with Kit.Db.Kit_Record;
with Kit.Db.Kit_Record_Base;
with Kit.Db.Kit_Reference;
with Kit.Db.Kit_Type;
with Kit.Db.Database.Db_Kit_Root_Record;
with Kit.Db.Database.Db_Kit_Type;
with Kit.Db.Database.Db_Kit_String;

package body Kit.Db.Database.Db_Kit_Bounded_String is

   ------------
   -- Create --
   ------------

   procedure Create is
   begin
      Marlowe_Keys.Handle.Add_Table ("kit_bounded_string", 28);
      Marlowe_Keys.T12_Top_Record_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_bounded_string_top_record", Kit_Bounded_String_Table_Index,
         12);
      Marlowe_Keys.T12_T1_Idx_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_bounded_string_t1_idx", Kit_Bounded_String_Table_Index, 16);
      Marlowe_Keys.T12_Name_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_bounded_string_name", Kit_Bounded_String_Table_Index, 72);
      Marlowe_Keys.T12_T4_Idx_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_bounded_string_t4_idx", Kit_Bounded_String_Table_Index, 16);
      Marlowe_Keys.T12_T10_Idx_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_bounded_string_t10_idx", Kit_Bounded_String_Table_Index, 16);
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Kit_Bounded_String_Ref := Kit_Record.Create ("kit_bounded_string", 12,
         28);
      Kit_Reference.Create
        (8,
         "kit_bounded_string",
         Kit_Bounded_String_Ref);
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("kit_root_record");
      begin
         Kit_Field.Create
           ("kit_root_record",
            Kit_Bounded_String_Ref,
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
            ("kit_type");
      begin
         Kit_Field.Create
           ("kit_type",
            Kit_Bounded_String_Ref,
            Type_Ref,
            12,
            8,
            False,
            False,
            False,
            False,
            True);
      end;
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("kit_string");
      begin
         Kit_Field.Create
           ("kit_string",
            Kit_Bounded_String_Ref,
            Type_Ref,
            20,
            8,
            False,
            False,
            False,
            False,
            True);
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("t10_idx",
            Kit_Bounded_String_Ref, True, 16);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Bounded_String_Ref,
               "kit_string");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
      Kit_Record_Base.Create
        (1,
         Db_Kit_Root_Record.Kit_Root_Record_Ref,
         Kit_Bounded_String_Ref);
      Kit_Record_Base.Create
        (2,
         Db_Kit_Type.Kit_Type_Ref,
         Kit_Bounded_String_Ref);
      Kit_Record_Base.Create
        (3,
         Db_Kit_String.Kit_String_Ref,
         Kit_Bounded_String_Ref);
   end Initialize;

   ----------
   -- Open --
   ----------

   procedure Open is
   begin
      Marlowe_Keys.T12_Top_Record_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_bounded_string_top_record");
      Marlowe_Keys.T12_T1_Idx_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_bounded_string_t1_idx");
      Marlowe_Keys.T12_Name_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_bounded_string_name");
      Marlowe_Keys.T12_T4_Idx_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_bounded_string_t4_idx");
      Marlowe_Keys.T12_T10_Idx_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_bounded_string_t10_idx");
   end Open;

end Kit.Db.Database.Db_Kit_Bounded_String;
