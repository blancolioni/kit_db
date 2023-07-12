with Kit.Generic_Cache;
with Kit.Db.Kit_Reference_Impl;

private package Kit.Db.Kit_Reference_Cache is
  new Kit.Generic_Cache
       (9,
      1198811527,
      Kit_Reference_Impl.Kit_Reference_Database_Record,
      Kit_Reference_Impl.Read,
      Kit_Reference_Impl.Write);
