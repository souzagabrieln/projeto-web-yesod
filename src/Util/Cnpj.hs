{-# LANGUAGE OverloadedStrings #-}

-- Validacao e formatacao de CNPJ.
--
-- O CNPJ tem 14 digitos: 12 de identificacao e 2 digitos verificadores.
-- Cada verificador e calculado a partir dos digitos anteriores com uma
-- soma ponderada modulo 11. Se os verificadores informados nao baterem
-- com os calculados, o numero foi digitado errado.
module Util.Cnpj
    ( validarCnpj
    , formatarCnpj
    ) where

import Data.Char (isDigit, digitToInt, intToDigit)
import Data.Text (Text)
import qualified Data.Text as T

-- Aceita com ou sem pontuacao ("11.222.333/0001-81" ou "11222333000181").
-- Devolve so os digitos, que e o que fica gravado no banco.
validarCnpj :: Text -> Either Text Text
validarCnpj texto
    | T.any invalido texto                = Left "Use apenas números, pontos, barra e hífen."
    | length digitos /= 14                = Left "O CNPJ deve ter 14 dígitos."
    | todosIguais digitos                 = Left "CNPJ inválido."
    | drop 12 digitos /= [dv1, dv2]       = Left "CNPJ inválido: os dígitos verificadores não conferem."
    | otherwise                           = Right (T.pack (map intToDigit digitos))
  where
    invalido c = not (isDigit c || c `elem` (" ./-" :: String))
    digitos = map digitToInt (filter isDigit (T.unpack texto))
    dv1 = verificador [5,4,3,2,9,8,7,6,5,4,3,2]   (take 12 digitos)
    dv2 = verificador [6,5,4,3,2,9,8,7,6,5,4,3,2] (take 12 digitos ++ [dv1])
    -- 00000000000000, 11111111111111... passam na conta mas nao valem.
    todosIguais xs = and (zipWith (==) xs (drop 1 xs))

-- Soma ponderada modulo 11: resto < 2 vira 0, senao 11 - resto.
verificador :: [Int] -> [Int] -> Int
verificador pesos ds =
    let resto = sum (zipWith (*) pesos ds) `mod` 11
    in if resto < 2 then 0 else 11 - resto

-- "11222333000181" -> "11.222.333/0001-81"
formatarCnpj :: Text -> Text
formatarCnpj c
    | T.length c /= 14 = c
    | otherwise = T.concat
        [ T.take 2 c, ".", fatia 2 3, ".", fatia 5 3, "/", fatia 8 4, "-", T.drop 12 c ]
  where
    fatia i n = T.take n (T.drop i c)
