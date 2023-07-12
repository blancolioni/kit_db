with Ada.Containers;

package Kit.Db.Kit_Record_Base_Hashes is

   function Hash
     (Reference : Kit_Record_Base_Reference)
      return Ada.Containers.Hash_Type;

private

   function Hash
     (Reference : Kit_Record_Base_Reference)
      return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Reference));

end Kit.Db.Kit_Record_Base_Hashes;
