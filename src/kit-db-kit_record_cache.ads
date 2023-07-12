with Kit.Generic_Cache;
with Kit.Db.Kit_Record_Impl;

private package Kit.Db.Kit_Record_Cache is
  new Kit.Generic_Cache
       (2,
      1962384374,
      Kit_Record_Impl.Kit_Record_Database_Record,
      Kit_Record_Impl.Read,
      Kit_Record_Impl.Write);
