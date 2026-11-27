{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- Pedacos de interface reaproveitados pelas paginas de cadastro.
module Util.Pagina
    ( paginaFormulario
    , botaoExcluir
    ) where

import Foundation
import Yesod
import Data.Text (Text)

-- Pagina com um formulario de cadastro/edicao.
--   titulo  : texto do <h1> e da aba do navegador
--   destino : rota que recebe o POST do formulario
--   voltar  : rota do botao "Cancelar"
paginaFormulario :: Text -> Route App -> Route App -> Widget -> Enctype -> Widget
paginaFormulario titulo destino voltar campos enctype = do
    setTitle (toHtml titulo)
    [whamlet|
        <h1>#{titulo}
        <form .formulario method=post action=@{destino} enctype=#{enctype}>
            ^{campos}
            <input type=submit value="Salvar">
            <a .botao.secundario href=@{voltar}>Cancelar
    |]

-- Botao que envia um POST para a rota de exclusao,
-- pedindo confirmacao no navegador antes. O nome vai num atributo
-- data-nome para que aspas no nome nao quebrem o JavaScript.
botaoExcluir :: Route App -> Text -> Widget
botaoExcluir rota nome =
    [whamlet|
        <form method=post action=@{rota} data-nome=#{nome} onsubmit="return confirm('Excluir ' + this.dataset.nome + '?');">
            <button .perigo type=submit>Excluir
    |]
