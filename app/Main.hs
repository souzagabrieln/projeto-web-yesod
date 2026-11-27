{-# LANGUAGE OverloadedStrings #-}

import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core
import Yesod.Static

import Database.Persist.Sqlite
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.IO.Class (liftIO)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

-- Caminho do arquivo do banco SQLite. Pode ser trocado pela
-- variavel de ambiente DB_PATH; o padrao e "proj.sqlite3",
-- criado na pasta onde o "stack run" e executado.
lerCaminhoBanco :: IO T.Text
lerCaminhoBanco = T.pack . fromMaybe "proj.sqlite3" <$> lookupEnv "DB_PATH"

main::IO()
main = do
  caminho <- lerCaminhoBanco
  runStdoutLoggingT $ withSqlitePool caminho 10 $ \pool -> liftIO $ do
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
main = runStdoutLoggingT $ withSqlitePool "proj.sqlite3" 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       static@(Static settings) <- static "static"
       warp 8080 (App pool static)
-}