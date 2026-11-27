{-# LANGUAGE OverloadedStrings #-}

-- Funcoes de validacao reaproveitadas pelos formularios.
module Util.Validacao
    ( nomeObrigatorio
    , tituloObrigatorio
    , precoValido
    , lerPreco
    , textoOpcional
    , anoValido
    , obterAnoAtual
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (getCurrentTime, toGregorian, utctDay)
import Text.Read (readMaybe)

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

-- Mesma regra do nome, com a mensagem adequada para titulos.
tituloObrigatorio :: Int -> Text -> Either Text Text
tituloObrigatorio limite texto =
    case nomeObrigatorio limite texto of
        Left _ | T.null (T.strip texto) -> Left "Informe o título."
        Left _  -> Left ("O título pode ter no máximo " <> T.pack (show limite) <> " caracteres.")
        Right t -> Right t

-- Preco maior que zero e com limite razoavel.
precoValido :: Double -> Either Text Double
precoValido preco
    | preco <= 0     = Left "O preço deve ser maior que zero."
    | preco > 100000 = Left "Preço alto demais. Confira o valor."
    | otherwise      = Right preco

-- Le um preco digitado com virgula ou ponto: "59,90" ou "59.90".
lerPreco :: Text -> Either Text Double
lerPreco texto =
    case readMaybe (T.unpack (T.replace "," "." (T.strip texto))) of
        Just valor -> precoValido valor
        Nothing    -> Left "Digite um valor numérico, por exemplo 59,90."

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
