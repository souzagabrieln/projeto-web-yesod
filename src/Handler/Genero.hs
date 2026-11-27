{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- CRUD de generos musicais: listar, cadastrar, editar e excluir.
module Handler.Genero where

import Foundation
import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Util.Validacao
import Util.Pagina

-- Formulario de genero. Serve para cadastro (Nothing) e edicao (Just ...).
generoForm :: Maybe GeneroId -> Maybe Genero -> Form Genero
generoForm mId mGenero = renderDivs $ Genero
    <$> areq campoNome "Nome" (generoNome <$> mGenero)
    <*> (limparDescricao <$> aopt textareaField "Descrição" (generoDescricao <$> mGenero))
  where
    campoNome = checkM (nomeDisponivel mId) (check (nomeObrigatorio 50) textField)

-- Descricao preenchida so com espacos e gravada como vazia (Nothing).
limparDescricao :: Maybe Textarea -> Maybe Textarea
limparDescricao = fmap Textarea . textoOpcional . fmap unTextarea

-- Nao deixa cadastrar dois generos com o mesmo nome.
nomeDisponivel :: Maybe GeneroId -> Text -> Handler (Either Text Text)
nomeDisponivel mId nome = do
    existente <- runDB $ getBy (UniqueGeneroNome nome)
    pure $ case existente of
        Just (Entity outroId _) | Just outroId /= mId ->
            Left "Já existe um gênero com esse nome."
        _ -> Right nome

-- Mostra so o comeco de descricoes longas na tabela.
resumo :: Maybe Textarea -> Text
resumo Nothing = "-"
resumo (Just (Textarea t))
    | T.length t > 80 = T.take 80 t <> "..."
    | otherwise       = t

-- GET /generos : lista todos os generos em ordem alfabetica.
getGenerosR :: Handler Html
getGenerosR = do
    generos <- runDB $ selectList [] [Asc GeneroNome]
    defaultLayout $ do
        setTitle "Gêneros"
        [whamlet|
            <div .cabecalho-pagina>
                <h1>Gêneros
                <a .botao href=@{GeneroNovoR}>Novo gênero
            $if null generos
                <p .vazio>Nenhum gênero cadastrado ainda.
            $else
                <table>
                    <thead>
                        <tr>
                            <th>Nome
                            <th>Descrição
                            <th>Ações
                    <tbody>
                        $forall Entity generoId genero <- generos
                            <tr>
                                <td>#{generoNome genero}
                                <td>#{resumo (generoDescricao genero)}
                                <td .acoes>
                                    <a .botao.secundario href=@{GeneroEditarR generoId}>Editar
                                    ^{botaoExcluir (GeneroExcluirR generoId) (generoNome genero)}
        |]

-- GET /generos/novo : mostra o formulario vazio.
getGeneroNovoR :: Handler Html
getGeneroNovoR = do
    (campos, enctype) <- generateFormPost (generoForm Nothing Nothing)
    defaultLayout $ paginaFormulario "Novo gênero" GeneroNovoR GenerosR campos enctype

-- POST /generos/novo : valida e grava.
postGeneroNovoR :: Handler Html
postGeneroNovoR = do
    ((resultado, campos), enctype) <- runFormPost (generoForm Nothing Nothing)
    case resultado of
        FormSuccess genero -> do
            _ <- runDB $ insert genero
            setMessage $ toHtml ("Gênero \"" <> generoNome genero <> "\" cadastrado.")
            redirect GenerosR
        _ -> defaultLayout $ paginaFormulario "Novo gênero" GeneroNovoR GenerosR campos enctype

-- GET /generos/#GeneroId/editar : formulario ja preenchido.
getGeneroEditarR :: GeneroId -> Handler Html
getGeneroEditarR generoId = do
    genero <- runDB $ get404 generoId
    (campos, enctype) <- generateFormPost (generoForm (Just generoId) (Just genero))
    defaultLayout $
        paginaFormulario "Editar gênero" (GeneroEditarR generoId) GenerosR campos enctype

-- POST /generos/#GeneroId/editar : valida e substitui o registro.
postGeneroEditarR :: GeneroId -> Handler Html
postGeneroEditarR generoId = do
    genero <- runDB $ get404 generoId
    ((resultado, campos), enctype) <-
        runFormPost (generoForm (Just generoId) (Just genero))
    case resultado of
        FormSuccess novo -> do
            runDB $ replace generoId novo
            setMessage $ toHtml ("Gênero \"" <> generoNome novo <> "\" atualizado.")
            redirect GenerosR
        _ -> defaultLayout $
            paginaFormulario "Editar gênero" (GeneroEditarR generoId) GenerosR campos enctype

-- POST /generos/#GeneroId/excluir : remove o genero, se nao tiver albuns.
postGeneroExcluirR :: GeneroId -> Handler Html
postGeneroExcluirR generoId = do
    genero <- runDB $ get404 generoId
    -- Nao exclui se houver albuns ligados: o album ficaria apontando
    -- para um registro que nao existe mais (chave estrangeira).
    albuns <- runDB $ count [AlbumGeneroId ==. generoId]
    if albuns > 0
        then do
            setMessage $ toHtml ("Não é possível excluir \"" <> generoNome genero
                <> "\": há " <> T.pack (show albuns) <> " álbum(ns) com esse gênero. Exclua ou altere esses álbuns antes.")
            redirect GenerosR
        else do
            runDB $ delete generoId
            setMessage $ toHtml ("Gênero \"" <> generoNome genero <> "\" excluído.")
            redirect GenerosR
