with Kit.Db.Marlowe_Keys;
with Kit.Db.Kit_Field;
with Kit.Db.Kit_Key;
with Kit.Db.Kit_Key_Field;
with Kit.Db.Kit_Record;
with Kit.Db.Kit_Reference;
with Kit.Db.Kit_Type;

package body Kit.Db.Database.Db_Kit_Root_Record is

   ------------
   -- Create --
   ------------

   procedure Create is
   begin
      Marlowe_Keys.Handle.Add_Table ("kit_root_record", 8);
      Marlowe_Keys.T1_Top_Record_Ref := Marlowe_Keys.Handle.Add_Key
         ("kit_root_record_top_record", Kit_Root_Record_Table_Index, 12);
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Kit_Root_Record_Ref := Kit_Record.Create ("kit_root_record", 1, 8);
      Kit_Reference.Create
        (8,
         "kit_root_record",
         Kit_Root_Record_Ref);
      declare
         Type_Ref : constant Kit_Type_Reference := Kit_Type.Get_By_Name
            ("record_type");
      begin
         Kit_Field.Create
           ("top_record",
            Kit_Root_Record_Ref,
            Type_Ref,
            4,
            4,
            False,
            True,
            False,
            False,
            False);
      end;
      declare
         Ref : constant Kit_Key_Reference := Kit_Key.Create ("top_record",
            Kit_Root_Record_Ref, False, 12);
      begin
         declare
            Field_Ref : constant Kit_Field_Reference :=
               Kit_Field.Get_By_Record_Field (Kit_Root_Record_Ref,
               "top_record");
         begin
            Kit_Key_Field.Create (Ref, Field_Ref);
         end;
      end;
   end Initialize;

   ----------
   -- Open --
   ----------

   procedure Open is
   begin
      Marlowe_Keys.T1_Top_Record_Ref := Marlowe_Keys.Handle.Get_Reference
         ("kit_root_record_top_record");
   end Open;

end Kit.Db.Database.Db_Kit_Root_Record;
