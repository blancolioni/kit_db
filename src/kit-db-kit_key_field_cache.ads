with Kit.Generic_Cache;
with Kit.Db.Kit_Key_Field_Impl;

private package Kit.Db.Kit_Key_Field_Cache is
  new Kit.Generic_Cache
       (18,
      1856305798,
      Kit_Key_Field_Impl.Kit_Key_Field_Database_Record,
      Kit_Key_Field_Impl.Read,
      Kit_Key_Field_Impl.Write);
