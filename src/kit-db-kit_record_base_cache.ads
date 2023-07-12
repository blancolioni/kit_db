with Kit.Generic_Cache;
with Kit.Db.Kit_Record_Base_Impl;

private package Kit.Db.Kit_Record_Base_Cache is
  new Kit.Generic_Cache
       (3,
      1146515157,
      Kit_Record_Base_Impl.Kit_Record_Base_Database_Record,
      Kit_Record_Base_Impl.Read,
      Kit_Record_Base_Impl.Write);
