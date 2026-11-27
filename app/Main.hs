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

-- Porta do servidor. Pode ser trocada pela variavel de ambiente PORT
-- (o Render define essa variavel automaticamente); o padrao e 3000.
lerPorta :: IO Int
lerPorta = do
    valor <- lookupEnv "PORT"
    return $ case valor >>= lerInt of
        Just p  -> p
        Nothing -> 3000
  where
    lerInt s = case reads s of
        [(n, "")] -> Just n
        _         -> Nothing

main :: IO ()
main = do
    caminho <- lerCaminhoBanco
    porta   <- lerPorta
    runStdoutLoggingT $ withSqlitePool caminho 10 $ \pool -> liftIO $ do
        -- Cria ou ajusta as tabelas de acordo com o modelo em Foundation.hs
        runSqlPersistMPool (runMigration migrateAll) pool
        arquivos <- static "static"
        putStrLn $ "Servidor rodando em http://localhost:" ++ show porta
        warp porta (App pool arquivos)
