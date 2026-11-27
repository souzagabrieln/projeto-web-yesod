{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

-- Formato fisico do album. Fica em um modulo separado porque o
-- derivePersistField (Template Haskell) precisa que o tipo ja exista
-- antes de ser usado no modelo do banco em Foundation.hs.
module Model.Formato
    ( Formato (..)
    , nomeFormato
    , opcoesFormato
    ) where

import Database.Persist.TH (derivePersistField)
import Data.Text (Text)

data Formato = CD | Vinil | Cassete
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- Grava o formato no banco como texto ("CD", "Vinil", "Cassete").
derivePersistField "Formato"

-- Nome exibido na tela.
nomeFormato :: Formato -> Text
nomeFormato CD      = "CD"
nomeFormato Vinil   = "Vinil (LP)"
nomeFormato Cassete = "Fita cassete"

-- Lista usada no <select> do formulario: todos os construtores, em ordem.
opcoesFormato :: [(Text, Formato)]
opcoesFormato = [ (nomeFormato f, f) | f <- [minBound .. maxBound] ]
