with Kit.Generic_Cache;
with Kit.Db.Kit_Type_Impl;

private package Kit.Db.Kit_Type_Cache is
  new Kit.Generic_Cache
       (4,
      228631241,
      Kit_Type_Impl.Kit_Type_Database_Record,
      Kit_Type_Impl.Read,
      Kit_Type_Impl.Write);
