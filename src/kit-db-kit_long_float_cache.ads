with Kit.Generic_Cache;
with Kit.Db.Kit_Long_Float_Impl;

private package Kit.Db.Kit_Long_Float_Cache is
  new Kit.Generic_Cache
       (8,
      2002356665,
      Kit_Long_Float_Impl.Kit_Long_Float_Database_Record,
      Kit_Long_Float_Impl.Read,
      Kit_Long_Float_Impl.Write);
