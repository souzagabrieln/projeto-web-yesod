{-# LANGUAGE OverloadedStrings #-}

import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core
import Yesod.Static

import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.IO.Class (liftIO)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- Le uma variavel de ambiente; se ela nao existir, usa o valor padrao.
envOr :: String -> String -> IO String
envOr nome padrao = fromMaybe padrao <$> lookupEnv nome

-- Coloca o valor entre aspas simples, como o libpq espera,
-- para senhas com espaco, aspas ou barra funcionarem.
aspas :: String -> String
aspas v = "'" ++ concatMap escapa v ++ "'"
  where
    escapa '\'' = "\\'"
    escapa '\\' = "\\\\"
    escapa c    = [c]

-- Monta a string de conexao do PostgreSQL a partir das variaveis
-- DB_HOST, DB_PORT, DB_USER, DB_PASSWORD e DB_NAME.
lerConnStr :: IO ConnectionString
lerConnStr = do
  host  <- envOr "DB_HOST"     "localhost"
  porta <- envOr "DB_PORT"     "5432"
  user  <- envOr "DB_USER"     "fulano"
  senha <- envOr "DB_PASSWORD" "123"
  banco <- envOr "DB_NAME"     "proj"
  let campos = [ ("dbname", banco), ("host", host), ("user", user)
               , ("password", senha), ("port", porta) ]
      texto  = unwords [ k ++ "=" ++ aspas v | (k, v) <- campos ]
  return (TE.encodeUtf8 (T.pack texto))

main::IO()
main = do
  connStr <- lerConnStr
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
       liftIO $ putStrLn "teste"
       flip runSqlPersistMPool pool $ do
              runMigration migrateAll
              --insert $ Pessoa "Felipe" "Cannarozzo" 34
              let pid = (toSqlKey 2 :: Key Pessoa)
             -- nn <- update pid [PessoaIdade +=. 1]
             -- liftIO $ print nn
              pessoa <- get pid
              liftIO $ print pessoa
       static@(Static settings) <- static "static"
       warp 8080 (App pool static)
-- Pattern Match
       
{-
main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       static@(Static settings) <- static "static"
       warp 8080 (App pool static)
-}