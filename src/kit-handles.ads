package Kit.Handles is

   type Handle_Interface is interface;
   function Has_Element (Handle : Handle_Interface) return Boolean
      is abstract;

private

   type Constraint_Operator is (Op_None, Op_Not, Op_EQ, Op_NE, Op_LE, Op_GT,
                                Op_LT, Op_GE);

end Kit.Handles;
