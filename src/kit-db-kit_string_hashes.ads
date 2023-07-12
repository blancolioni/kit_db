with Ada.Containers;

package Kit.Db.Kit_String_Hashes is

   function Hash
     (Reference : Kit_String_Reference)
      return Ada.Containers.Hash_Type;

private

   function Hash
     (Reference : Kit_String_Reference)
      return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Reference));

end Kit.Db.Kit_String_Hashes;
