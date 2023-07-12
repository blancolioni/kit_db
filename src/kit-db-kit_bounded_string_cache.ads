with Kit.Generic_Cache;
with Kit.Db.Kit_Bounded_String_Impl;

private package Kit.Db.Kit_Bounded_String_Cache is
  new Kit.Generic_Cache
       (12,
      1772781407,
      Kit_Bounded_String_Impl.Kit_Bounded_String_Database_Record,
      Kit_Bounded_String_Impl.Read,
      Kit_Bounded_String_Impl.Write);
