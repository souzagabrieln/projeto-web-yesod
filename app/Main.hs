{-# LANGUAGE OverloadedStrings #-}

import Application () -- instancia YesodDispatch
import Foundation
import Yesod
import Yesod.Static

import Database.Persist.Sqlite
import Control.Monad.Logger (runStdoutLoggingT)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

-- Caminho do arquivo do banco SQLite. Pode ser trocado pela
-- variavel de ambiente DB_PATH; o padrao e "proj.sqlite3",
-- criado na pasta onde o "stack run" e executado.
lerCaminhoBanco :: IO T.Text
lerCaminhoBanco = T.pack . fromMaybe "proj.sqlite3" <$> lookupEnv "DB_PATH"

main :: IO ()
main = do
    caminho <- lerCaminhoBanco
    runStdoutLoggingT $ withSqlitePool caminho 10 $ \pool -> liftIO $ do
        -- Cria ou ajusta as tabelas de acordo com o modelo em Foundation.hs
        runSqlPersistMPool (runMigration migrateAll) pool
        arquivos <- static "static"
        putStrLn "Servidor rodando em http://localhost:8080"
        warp 8080 (App pool arquivos)
