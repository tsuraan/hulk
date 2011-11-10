{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}
module Hulk.Config
    (Config(..)
    ,getConfig)
    where

import Data.Word
import Data.ConfigFile

import Hulk.Types

getConfig :: FilePath -> IO Config
getConfig conf = do
  contents <- readFile conf
  let config = do
        c <- readstring emptyCP contents
        hostname <- get c "LISTEN" "hostname"
        listen <- get c "LISTEN" "port"
        motd <- get c "STRINGS" "motd_file"
        preface <- get c "STRINGS" "preface_file"
        passwd <- get c "AUTH" "passwd_file"
        key <- get c "AUTH" "passwd_key"
        users <- get c "USERS" "data_dir"
        logs <- get c "LOGS" "event_log"
        chans <- fmap words $ get c "LOGS" "channels"
        let ldap_host = either (const Nothing) Just $ get c "AUTH" "ldap_host"
        let ldap_port = either (const Nothing) Just $ get c "AUTH" "ldap_port"
        let ldap_dn = either (const Nothing) Just $ get c "AUTH" "ldap_dn"
        return Config { configListen = fromIntegral (listen::Word16)
                      , configMotd = Just motd 
                      , configHostname = hostname
                      , configPasswd = passwd
                      , configPasswdKey = key
                      , configLdapHost = ldap_host
                      , configLdapPort = ldap_port
                      , configLdapDn = ldap_dn
                      , configPreface = Just preface
                      , configUserData = users
                      , configLogFile = logs
                      , configLogChans = chans
                      }
  case config of
    Left cperr -> error $ show cperr
    Right config -> return config
