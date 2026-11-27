{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- CRUD de artistas: listar, cadastrar, editar e excluir.
module Handler.Artista where

import Foundation
import Yesod
import Data.Text (Text)
import Util.Validacao
import Util.Pagina

-- Formulario de artista. Serve para cadastro (Nothing) e edicao (Just ...).
--   anoAtual : usado para nao aceitar ano de inicio no futuro
--   mId      : id do artista em edicao, para a checagem de nome repetido
--   mArtista : valores que ja aparecem preenchidos nos campos
artistaForm :: Integer -> Maybe ArtistaId -> Maybe Artista -> Form Artista
artistaForm anoAtual mId mArtista = renderDivs $ Artista
    <$> areq campoNome "Nome" (artistaNome <$> mArtista)
    <*> (textoOpcional <$> aopt textField "País de origem" (artistaPais <$> mArtista))
    <*> aopt campoAno "Ano de início da carreira" (artistaAnoInicio <$> mArtista)
  where
    -- check valida sem acessar o banco; checkM pode rodar consultas.
    campoNome = checkM (nomeDisponivel mId) (check (nomeObrigatorio 100) textField)
    campoAno  = check (anoValido anoAtual) intField

-- Nao deixa cadastrar dois artistas com o mesmo nome.
-- Na edicao, o proprio artista pode manter o nome que ja tinha.
nomeDisponivel :: Maybe ArtistaId -> Text -> Handler (Either Text Text)
nomeDisponivel mId nome = do
    existente <- runDB $ getBy (UniqueArtistaNome nome)
    pure $ case existente of
        Just (Entity outroId _) | Just outroId /= mId ->
            Left "Já existe um artista com esse nome."
        _ -> Right nome

-- GET /artistas : lista todos os artistas em ordem alfabetica.
getArtistasR :: Handler Html
getArtistasR = do
    artistas <- runDB $ selectList [] [Asc ArtistaNome]
    defaultLayout $ do
        setTitle "Artistas"
        [whamlet|
            <div .cabecalho-pagina>
                <h1>Artistas
                <a .botao href=@{ArtistaNovoR}>Novo artista
            $if null artistas
                <p .vazio>Nenhum artista cadastrado ainda.
            $else
                <table>
                    <thead>
                        <tr>
                            <th>Nome
                            <th>País
                            <th>Início
                            <th>Ações
                    <tbody>
                        $forall Entity artistaId artista <- artistas
                            <tr>
                                <td>#{artistaNome artista}
                                <td>#{maybe "-" id (artistaPais artista)}
                                <td>#{maybe "-" show (artistaAnoInicio artista)}
                                <td .acoes>
                                    <a .botao.secundario href=@{ArtistaEditarR artistaId}>Editar
                                    ^{botaoExcluir (ArtistaExcluirR artistaId) (artistaNome artista)}
        |]

-- GET /artistas/novo : mostra o formulario vazio.
getArtistaNovoR :: Handler Html
getArtistaNovoR = do
    ano <- obterAnoAtual
    (campos, enctype) <- generateFormPost (artistaForm ano Nothing Nothing)
    defaultLayout $ paginaFormulario "Novo artista" ArtistaNovoR ArtistasR campos enctype

-- POST /artistas/novo : valida e grava. Se houver erro, mostra o
-- formulario de novo com as mensagens ao lado dos campos.
postArtistaNovoR :: Handler Html
postArtistaNovoR = do
    ano <- obterAnoAtual
    ((resultado, campos), enctype) <- runFormPost (artistaForm ano Nothing Nothing)
    case resultado of
        FormSuccess artista -> do
            _ <- runDB $ insert artista
            setMessage $ toHtml ("Artista \"" <> artistaNome artista <> "\" cadastrado.")
            redirect ArtistasR
        _ -> defaultLayout $ paginaFormulario "Novo artista" ArtistaNovoR ArtistasR campos enctype

-- GET /artistas/#ArtistaId/editar : formulario ja preenchido.
-- get404 responde "nao encontrado" se o id nao existir.
getArtistaEditarR :: ArtistaId -> Handler Html
getArtistaEditarR artistaId = do
    artista <- runDB $ get404 artistaId
    ano <- obterAnoAtual
    (campos, enctype) <- generateFormPost (artistaForm ano (Just artistaId) (Just artista))
    defaultLayout $
        paginaFormulario "Editar artista" (ArtistaEditarR artistaId) ArtistasR campos enctype

-- POST /artistas/#ArtistaId/editar : valida e substitui o registro.
postArtistaEditarR :: ArtistaId -> Handler Html
postArtistaEditarR artistaId = do
    artista <- runDB $ get404 artistaId
    ano <- obterAnoAtual
    ((resultado, campos), enctype) <-
        runFormPost (artistaForm ano (Just artistaId) (Just artista))
    case resultado of
        FormSuccess novo -> do
            runDB $ replace artistaId novo
            setMessage $ toHtml ("Artista \"" <> artistaNome novo <> "\" atualizado.")
            redirect ArtistasR
        _ -> defaultLayout $
            paginaFormulario "Editar artista" (ArtistaEditarR artistaId) ArtistasR campos enctype

-- POST /artistas/#ArtistaId/excluir : remove o artista.
postArtistaExcluirR :: ArtistaId -> Handler Html
postArtistaExcluirR artistaId = do
    artista <- runDB $ get404 artistaId
    runDB $ delete artistaId
    setMessage $ toHtml ("Artista \"" <> artistaNome artista <> "\" excluído.")
    redirect ArtistasR
