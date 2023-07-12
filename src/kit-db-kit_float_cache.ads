with Kit.Generic_Cache;
with Kit.Db.Kit_Float_Impl;

private package Kit.Db.Kit_Float_Cache is
  new Kit.Generic_Cache
       (7,
      1796745100,
      Kit_Float_Impl.Kit_Float_Database_Record,
      Kit_Float_Impl.Read,
      Kit_Float_Impl.Write);
