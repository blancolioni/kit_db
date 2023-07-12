with Kit.Generic_Cache;
with Kit.Db.Kit_Field_Impl;

private package Kit.Db.Kit_Field_Cache is
  new Kit.Generic_Cache
       (15,
      1790729970,
      Kit_Field_Impl.Kit_Field_Database_Record,
      Kit_Field_Impl.Read,
      Kit_Field_Impl.Write);
