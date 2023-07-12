with Kit.Generic_Cache;
with Kit.Db.Kit_String_Impl;

private package Kit.Db.Kit_String_Cache is
  new Kit.Generic_Cache
       (10,
      1860831239,
      Kit_String_Impl.Kit_String_Database_Record,
      Kit_String_Impl.Read,
      Kit_String_Impl.Write);
