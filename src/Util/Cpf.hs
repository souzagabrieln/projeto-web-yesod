{-# LANGUAGE OverloadedStrings #-}

-- Validacao e formatacao de CPF.
--
-- O CPF tem 11 digitos: 9 de identificacao e 2 verificadores. O primeiro
-- verificador usa os pesos 10, 9, ..., 2 sobre os 9 primeiros digitos;
-- o segundo usa 11, 10, ..., 2 sobre os 10 primeiros. Em cada caso:
-- (soma * 10) mod 11, e o resultado 10 vira 0.
module Util.Cpf
    ( validarCpf
    , formatarCpf
    ) where

import Data.Char (isDigit, digitToInt, intToDigit)
import Data.Text (Text)
import qualified Data.Text as T

-- Aceita com ou sem pontuacao ("529.982.247-25" ou "52998224725").
-- Devolve so os digitos, que e o que fica gravado no banco.
validarCpf :: Text -> Either Text Text
validarCpf texto
    | T.any invalido texto        = Left "Use apenas números, pontos e hífen."
    | length digitos /= 11        = Left "O CPF deve ter 11 dígitos."
    | todosIguais digitos         = Left "CPF inválido."
    | drop 9 digitos /= [dv1, dv2] = Left "CPF inválido: os dígitos verificadores não conferem."
    | otherwise                   = Right (T.pack (map intToDigit digitos))
  where
    invalido c = not (isDigit c || c `elem` (" .-" :: String))
    digitos = map digitToInt (filter isDigit (T.unpack texto))
    dv1 = verificador [10, 9 .. 2] (take 9 digitos)
    dv2 = verificador [11, 10 .. 2] (take 9 digitos ++ [dv1])
    -- 000.000.000-00, 111.111.111-11... passam na conta mas nao valem.
    todosIguais xs = and (zipWith (==) xs (drop 1 xs))

verificador :: [Int] -> [Int] -> Int
verificador pesos ds =
    let resto = (sum (zipWith (*) pesos ds) * 10) `mod` 11
    in if resto == 10 then 0 else resto

-- "52998224725" -> "529.982.247-25"
formatarCpf :: Text -> Text
formatarCpf c
    | T.length c /= 11 = c
    | otherwise = T.concat [T.take 3 c, ".", fatia 3, ".", fatia 6, "-", T.drop 9 c]
  where
    fatia i = T.take 3 (T.drop i c)
