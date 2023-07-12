with Kit.Generic_Cache;
with Kit.Db.Kit_Key_Impl;

private package Kit.Db.Kit_Key_Cache is
  new Kit.Generic_Cache
       (17,
      1204280153,
      Kit_Key_Impl.Kit_Key_Database_Record,
      Kit_Key_Impl.Read,
      Kit_Key_Impl.Write);
