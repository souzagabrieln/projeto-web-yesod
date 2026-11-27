{-# LANGUAGE OverloadedStrings #-}

-- Conversoes de valores para texto exibido na tela.
module Util.Formatacao
    ( formatarPreco
    , formatarDuracao
    , lerDuracao
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import Text.Read (readMaybe)

-- 59.9 -> "R$ 59,90" (com espaco que nao quebra linha)
formatarPreco :: Double -> Text
formatarPreco valor = "R$\x00A0" <> T.replace "." "," (T.pack (printf "%.2f" valor))

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
