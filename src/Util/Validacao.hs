{-# LANGUAGE OverloadedStrings #-}

-- Funcoes de validacao reaproveitadas pelos formularios.
module Util.Validacao
    ( nomeObrigatorio
    , textoOpcional
    , anoValido
    , obterAnoAtual
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (getCurrentTime, toGregorian, utctDay)

-- Remove espacos das pontas e exige entre 1 e o limite de caracteres.
-- Devolve Left com a mensagem de erro ou Right com o texto ja limpo.
nomeObrigatorio :: Int -> Text -> Either Text Text
nomeObrigatorio limite texto
    | T.null limpo           = Left "Informe um nome."
    | T.length limpo > limite =
        Left ("O nome pode ter no máximo " <> T.pack (show limite) <> " caracteres.")
    | otherwise              = Right limpo
  where
    limpo = T.strip texto

-- Para campos opcionais: texto so com espacos e tratado como vazio.
textoOpcional :: Maybe Text -> Maybe Text
textoOpcional Nothing = Nothing
textoOpcional (Just t)
    | T.null (T.strip t) = Nothing
    | otherwise          = Just (T.strip t)

-- Ano entre 1900 e o ano atual (recebido como parametro).
anoValido :: Integer -> Int -> Either Text Int
anoValido anoAtual ano
    | ano < 1900                      = Left "O ano deve ser 1900 ou posterior."
    | toInteger ano > anoAtual        = Left "O ano não pode estar no futuro."
    | otherwise                       = Right ano

-- Ano atual, lido do relogio do sistema. Usado para validar anos.
obterAnoAtual :: MonadIO m => m Integer
obterAnoAtual = do
    agora <- liftIO getCurrentTime
    let (ano, _, _) = toGregorian (utctDay agora)
    pure ano
