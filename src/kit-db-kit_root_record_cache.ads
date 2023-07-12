with Kit.Generic_Cache;
with Kit.Db.Kit_Root_Record_Impl;

private package Kit.Db.Kit_Root_Record_Cache is
  new Kit.Generic_Cache
       (1,
      683468704,
      Kit_Root_Record_Impl.Kit_Root_Record_Database_Record,
      Kit_Root_Record_Impl.Read,
      Kit_Root_Record_Impl.Write);
