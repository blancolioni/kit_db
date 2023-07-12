with Marlowe.Data_Stores.Btrees;
with Kit.Cache;
with Kit.Notifier;
with Kit.Db.Marlowe_Keys;
with Kit.Db.Kit_Deferred_Keys;
with Kit.Db.Database.Types;
with Kit.Db.Database.Db_Kit_Root_Record;
with Kit.Db.Database.Db_Kit_Record;
with Kit.Db.Database.Db_Kit_Record_Base;
with Kit.Db.Database.Db_Kit_Type;
with Kit.Db.Database.Db_Kit_Integer;
with Kit.Db.Database.Db_Kit_Long_Integer;
with Kit.Db.Database.Db_Kit_Float;
with Kit.Db.Database.Db_Kit_Long_Float;
with Kit.Db.Database.Db_Kit_Reference;
with Kit.Db.Database.Db_Kit_String;
with Kit.Db.Database.Db_Kit_Fixed_String;
with Kit.Db.Database.Db_Kit_Bounded_String;
with Kit.Db.Database.Db_Kit_Enumeration;
with Kit.Db.Database.Db_Kit_Literal;
with Kit.Db.Database.Db_Kit_Field;
with Kit.Db.Database.Db_Kit_Display_Field;
with Kit.Db.Database.Db_Kit_Key;
with Kit.Db.Database.Db_Kit_Key_Field;

package body Kit.Db.Database is

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      Kit.Notifier.Stop;
      Kit_Deferred_Keys.Close_Deferred_Keys;
      Kit.Cache.Close;
      Database_Mutex.Lock;
      Marlowe_Keys.Handle.Close;
      Database_Mutex.Unlock;
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create (Path : String := "kit.marlowe") is
   begin
      Kit.Cache.Start_Cache;
      Database_Mutex.Lock;
      Marlowe_Keys.Handle := new Marlowe.Data_Stores.Btrees.Btree_Data_Store;
      Marlowe_Keys.Handle.Create (Path, Database_Magic_Number);
      Db_Kit_Root_Record.Create;
      Db_Kit_Record.Create;
      Db_Kit_Record_Base.Create;
      Db_Kit_Type.Create;
      Db_Kit_Integer.Create;
      Db_Kit_Long_Integer.Create;
      Db_Kit_Float.Create;
      Db_Kit_Long_Float.Create;
      Db_Kit_Reference.Create;
      Db_Kit_String.Create;
      Db_Kit_Fixed_String.Create;
      Db_Kit_Bounded_String.Create;
      Db_Kit_Enumeration.Create;
      Db_Kit_Literal.Create;
      Db_Kit_Field.Create;
      Db_Kit_Display_Field.Create;
      Db_Kit_Key.Create;
      Db_Kit_Key_Field.Create;
      Database_Mutex.Unlock;
      Kit.Db.Database.Types.Create_Types;
      declare
      begin
         Kit.Db.Database.Db_Kit_Root_Record.Initialize;
         Kit.Db.Database.Db_Kit_Record.Initialize;
         Kit.Db.Database.Db_Kit_Record_Base.Initialize;
         Kit.Db.Database.Db_Kit_Type.Initialize;
         Kit.Db.Database.Db_Kit_Integer.Initialize;
         Kit.Db.Database.Db_Kit_Long_Integer.Initialize;
         Kit.Db.Database.Db_Kit_Float.Initialize;
         Kit.Db.Database.Db_Kit_Long_Float.Initialize;
         Kit.Db.Database.Db_Kit_Reference.Initialize;
         Kit.Db.Database.Db_Kit_String.Initialize;
         Kit.Db.Database.Db_Kit_Fixed_String.Initialize;
         Kit.Db.Database.Db_Kit_Bounded_String.Initialize;
         Kit.Db.Database.Db_Kit_Enumeration.Initialize;
         Kit.Db.Database.Db_Kit_Literal.Initialize;
         Kit.Db.Database.Db_Kit_Field.Initialize;
         Kit.Db.Database.Db_Kit_Display_Field.Initialize;
         Kit.Db.Database.Db_Kit_Key.Initialize;
         Kit.Db.Database.Db_Kit_Key_Field.Initialize;
      end;
   end Create;

   ----------
   -- Open --
   ----------

   procedure Open (Path : String := "kit.marlowe") is
   begin
      Kit.Cache.Start_Cache;
      Database_Mutex.Lock;
      Marlowe_Keys.Handle := new Marlowe.Data_Stores.Btrees.Btree_Data_Store;
      Marlowe_Keys.Handle.Open (Path, Database_Magic_Number);
      Db_Kit_Root_Record.Open;
      Db_Kit_Record.Open;
      Db_Kit_Record_Base.Open;
      Db_Kit_Type.Open;
      Db_Kit_Integer.Open;
      Db_Kit_Long_Integer.Open;
      Db_Kit_Float.Open;
      Db_Kit_Long_Float.Open;
      Db_Kit_Reference.Open;
      Db_Kit_String.Open;
      Db_Kit_Fixed_String.Open;
      Db_Kit_Bounded_String.Open;
      Db_Kit_Enumeration.Open;
      Db_Kit_Literal.Open;
      Db_Kit_Field.Open;
      Db_Kit_Display_Field.Open;
      Db_Kit_Key.Open;
      Db_Kit_Key_Field.Open;
      Database_Mutex.Unlock;
   end Open;

end Kit.Db.Database;
