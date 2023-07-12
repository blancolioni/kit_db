with Kit.Generic_Cache;
with Kit.Db.Kit_Long_Integer_Impl;

private package Kit.Db.Kit_Long_Integer_Cache is
  new Kit.Generic_Cache
       (6,
      1557915027,
      Kit_Long_Integer_Impl.Kit_Long_Integer_Database_Record,
      Kit_Long_Integer_Impl.Read,
      Kit_Long_Integer_Impl.Write);
