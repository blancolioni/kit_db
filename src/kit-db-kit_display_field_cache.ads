with Kit.Generic_Cache;
with Kit.Db.Kit_Display_Field_Impl;

private package Kit.Db.Kit_Display_Field_Cache is
  new Kit.Generic_Cache
       (16,
      1345351530,
      Kit_Display_Field_Impl.Kit_Display_Field_Database_Record,
      Kit_Display_Field_Impl.Read,
      Kit_Display_Field_Impl.Write);
