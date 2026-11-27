{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

-- Tipo de movimentacao de estoque. Fica em modulo separado pelo mesmo
-- motivo do Formato: o derivePersistField precisa do tipo ja compilado.
module Model.Movimentacao
    ( TipoMovimentacao (..)
    , nomeTipo
    ) where

import Database.Persist.TH (derivePersistField)
import Data.Text (Text)

-- Entrada: mercadoria comprada de um fornecedor (soma no estoque).
-- Saida: mercadoria que deixou a loja, por exemplo numa venda (subtrai).
data TipoMovimentacao = Entrada | Saida
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

derivePersistField "TipoMovimentacao"

nomeTipo :: TipoMovimentacao -> Text
nomeTipo Entrada = "Entrada"
nomeTipo Saida   = "Saída"
