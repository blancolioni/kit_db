with Kit.Generic_Cache;
with Kit.Db.Kit_Enumeration_Impl;

private package Kit.Db.Kit_Enumeration_Cache is
  new Kit.Generic_Cache
       (13,
      1569090318,
      Kit_Enumeration_Impl.Kit_Enumeration_Database_Record,
      Kit_Enumeration_Impl.Read,
      Kit_Enumeration_Impl.Write);
