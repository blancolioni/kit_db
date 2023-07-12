with Kit.Generic_Cache;
with Kit.Db.Kit_Integer_Impl;

private package Kit.Db.Kit_Integer_Cache is
  new Kit.Generic_Cache
       (5,
      799722610,
      Kit_Integer_Impl.Kit_Integer_Database_Record,
      Kit_Integer_Impl.Read,
      Kit_Integer_Impl.Write);
