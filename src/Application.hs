{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Yesod.Core

-- Cada modulo de handler precisa ser importado aqui para que
-- o mkYesodDispatch encontre as funcoes getXxxR / postXxxR.
import Handler.Home
import Handler.Artista
import Handler.Genero
import Handler.Album
import Handler.Faixa
import Handler.Importacao
import Handler.Fornecedor
import Handler.Estoque

mkYesodDispatch "App" resourcesApp
