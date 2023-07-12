with Ada.Containers;

package Kit.Db.Kit_Literal_Hashes is

   function Hash
     (Reference : Kit_Literal_Reference)
      return Ada.Containers.Hash_Type;

private

   function Hash
     (Reference : Kit_Literal_Reference)
      return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Reference));

end Kit.Db.Kit_Literal_Hashes;
