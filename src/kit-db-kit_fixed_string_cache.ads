with Kit.Generic_Cache;
with Kit.Db.Kit_Fixed_String_Impl;

private package Kit.Db.Kit_Fixed_String_Cache is
  new Kit.Generic_Cache
       (11,
      2136970049,
      Kit_Fixed_String_Impl.Kit_Fixed_String_Database_Record,
      Kit_Fixed_String_Impl.Read,
      Kit_Fixed_String_Impl.Write);
