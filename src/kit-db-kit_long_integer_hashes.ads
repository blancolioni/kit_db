with Ada.Containers;

package Kit.Db.Kit_Long_Integer_Hashes is

   function Hash
     (Reference : Kit_Long_Integer_Reference)
      return Ada.Containers.Hash_Type;

private

   function Hash
     (Reference : Kit_Long_Integer_Reference)
      return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Reference));

end Kit.Db.Kit_Long_Integer_Hashes;
