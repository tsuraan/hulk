{-# LANGUAGE ScopedTypeVariables #-}
module Hulk.Ldap
( authenticate
) where

import LDAP.Init
import LDAP.Exceptions
import Control.Exception

authenticate :: String -> Int -> String -> String -> IO Bool
authenticate host port dn pass = do
  ldap <- ldapInit host (fromIntegral port)
  catches (ldapSimpleBind ldap dn pass >> return True)
    [ Handler (\(_ :: IOException) -> return False)
    , Handler (\(_ :: LDAPException) -> return False) ]

