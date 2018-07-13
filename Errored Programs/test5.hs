  data G a where { GBool :: G Bool }
  foo x = case x of GBool -> True
