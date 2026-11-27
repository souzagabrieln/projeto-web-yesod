{-# LANGUAGE OverloadedStrings #-}

-- Conversoes de valores para texto exibido na tela.
module Util.Formatacao
    ( formatarPreco
    , formatarDuracao
    , lerDuracao
    , formatarDataHora
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.Time (UTCTime, TimeZone, utcToLocalTime, formatTime, defaultTimeLocale)

-- 59.9 -> "R$ 59,90" e 1050 -> "R$ 1.050,00"
-- (com espaco que nao quebra linha depois do R$)
formatarPreco :: Double -> Text
formatarPreco valor = "R$\x00A0" <> sinal <> milhares inteiro <> "," <> centavos
  where
    texto = T.pack (printf "%.2f" (abs valor))
    (inteiro, resto) = T.breakOn "." texto
    centavos = T.drop 1 resto
    sinal = if valor < 0 then "-" else ""
    -- "1234567" -> "1.234.567": agrupa de 3 em 3 a partir da direita
    milhares = T.reverse . T.intercalate "." . T.chunksOf 3 . T.reverse

-- 225 segundos -> "3:45"
formatarDuracao :: Int -> Text
formatarDuracao total = T.pack (printf "%d:%02d" minutos segundos)
  where
    (minutos, segundos) = total `divMod` 60

-- "3:45" -> Right 225. Aceita apenas minutos:segundos com segundos < 60.
lerDuracao :: Text -> Either Text Int
lerDuracao texto =
    case T.splitOn ":" (T.strip texto) of
        [m, s] | T.length s == 2 ->
            case (readMaybe (T.unpack m), readMaybe (T.unpack s)) of
                (Just minutos, Just segundos)
                    | minutos >= 0 && segundos >= 0 && segundos < 60 ->
                        Right (minutos * 60 + segundos)
                _ -> erro
        _ -> erro
  where
    erro = Left "Use o formato minutos:segundos, por exemplo 3:45."

-- Data e hora no fuso do servidor: "27/11/2026 21:45".
-- O banco guarda em UTC; a conversao e feita so para exibir.
formatarDataHora :: TimeZone -> UTCTime -> Text
formatarDataHora fuso t = T.pack (formatTime defaultTimeLocale "%d/%m/%Y %H:%M" (utcToLocalTime fuso t))
