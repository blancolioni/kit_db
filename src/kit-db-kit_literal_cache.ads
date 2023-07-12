with Kit.Generic_Cache;
with Kit.Db.Kit_Literal_Impl;

private package Kit.Db.Kit_Literal_Cache is
  new Kit.Generic_Cache
       (14,
      1450743020,
      Kit_Literal_Impl.Kit_Literal_Database_Record,
      Kit_Literal_Impl.Read,
      Kit_Literal_Impl.Write);
