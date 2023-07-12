package body Kit.Db is

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Enumeration_Reference) return String
   is (Kit_Enumeration_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Integer_Reference) return String
   is (Kit_Integer_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Fixed_String_Reference) return String
   is (Kit_Fixed_String_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Record_Reference) return String
   is (Kit_Record_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Bounded_String_Reference) return String
   is (Kit_Bounded_String_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Long_Integer_Reference) return String
   is (Kit_Long_Integer_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Literal_Reference) return String
   is (Kit_Literal_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_String_Reference) return String
   is (Kit_String_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Type_Reference) return String
   is (Kit_Type_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Reference_Reference) return String
   is (Kit_Reference_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Root_Record_Reference) return String
   is (Kit_Root_Record_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Key_Field_Reference) return String
   is (Kit_Key_Field_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Long_Float_Reference) return String
   is (Kit_Long_Float_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Key_Reference) return String
   is (Kit_Key_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Display_Field_Reference) return String
   is (Kit_Display_Field_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Record_Base_Reference) return String
   is (Kit_Record_Base_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Float_Reference) return String
   is (Kit_Float_Reference'Image (Item));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Kit_Field_Reference) return String
   is (Kit_Field_Reference'Image (Item));

end Kit.Db;
