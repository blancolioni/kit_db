with Ada.Containers;

package Kit.Db.Kit_Root_Record_Hashes is

   function Hash
     (Reference : Kit_Root_Record_Reference)
      return Ada.Containers.Hash_Type;

private

   function Hash
     (Reference : Kit_Root_Record_Reference)
      return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Reference));

end Kit.Db.Kit_Root_Record_Hashes;
