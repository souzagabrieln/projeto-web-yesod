{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Pagina inicial: resumo do catalogo e atalhos para os cadastros.
module Handler.Home where

import Foundation
import Yesod

getHomeR :: Handler Html
getHomeR = do
    -- count conta os registros de cada tabela ([] = sem filtro)
    totalArtistas <- runDB $ count ([] :: [Filter Artista])
    totalGeneros  <- runDB $ count ([] :: [Filter Genero])
    defaultLayout $ do
        setTitle "Início"
        [whamlet|
            <h1>Loja de Álbuns
            <p>
                Sistema de controle de estoque de uma loja de álbuns de música:
                catálogo de artistas, gêneros e álbuns, entrada de mercadoria e vendas.
            <div .cartoes>
                <div .cartao>
                    <div .numero>#{totalArtistas}
                    <p>artistas cadastrados
                    <a .botao href=@{ArtistasR}>Ver artistas
                <div .cartao>
                    <div .numero>#{totalGeneros}
                    <p>gêneros cadastrados
                    <a .botao href=@{GenerosR}>Ver gêneros
        |]
