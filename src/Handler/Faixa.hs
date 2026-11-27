{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Faixas de um album: formulario, edicao e exclusao.
-- O cadastro de faixa (postFaixaNovaR) fica em Handler.Album, porque
-- usa a pagina de detalhes do album.
module Handler.Faixa where

import Foundation
import Yesod
import Data.Text (Text)
import Util.Validacao
import Util.Formatacao
import Util.Pagina

-- Formulario de faixa. O album vem da URL, nao do formulario.
--   mNumero : numero sugerido para a proxima faixa
--   mFaixa  : valores atuais, na edicao
faixaForm :: AlbumId -> Maybe Int -> Maybe Faixa -> Form Faixa
faixaForm albumId mNumero mFaixa = renderDivs $ Faixa
    <$> pure albumId
    <*> areq campoNumero "Número" (maybe mNumero (Just . faixaNumero) mFaixa)
    <*> areq (check (tituloObrigatorio 150) textField) "Título" (faixaTitulo <$> mFaixa)
    <*> aopt campoDuracao "Duração (min:seg)" (faixaDuracao <$> mFaixa)
  where
    campoNumero = check numeroValido intField
    -- O usuario digita "3:45"; no banco fica 225 (segundos).
    campoDuracao = checkMMap (pure . lerDuracao) formatarDuracao textField

numeroValido :: Int -> Either Text Int
numeroValido n
    | n < 1     = Left "O número da faixa começa em 1."
    | n > 999   = Left "Número de faixa alto demais."
    | otherwise = Right n

-- Proximo numero livre: maior numero ja cadastrado + 1.
proximoNumero :: AlbumId -> Handler Int
proximoNumero albumId = do
    ultima <- runDB $ selectFirst [FaixaAlbumId ==. albumId] [Desc FaixaNumero]
    pure $ maybe 1 (\(Entity _ f) -> faixaNumero f + 1) ultima

avisoNumeroRepetido :: Int -> Handler ()
avisoNumeroRepetido n = setMessage $ toHtml
    ("Este álbum já tem uma faixa com o número " <> show n <> ".")

-- GET /faixas/#FaixaId/editar
getFaixaEditarR :: FaixaId -> Handler Html
getFaixaEditarR faixaId = do
    faixa <- runDB $ get404 faixaId
    (campos, enctype) <- generateFormPost (faixaForm (faixaAlbumId faixa) Nothing (Just faixa))
    defaultLayout $
        paginaFormulario "Editar faixa" (FaixaEditarR faixaId) (AlbumR (faixaAlbumId faixa)) campos enctype

-- POST /faixas/#FaixaId/editar
postFaixaEditarR :: FaixaId -> Handler Html
postFaixaEditarR faixaId = do
    faixa <- runDB $ get404 faixaId
    let albumId = faixaAlbumId faixa
    ((resultado, campos), enctype) <- runFormPost (faixaForm albumId Nothing (Just faixa))
    let formulario = defaultLayout $
            paginaFormulario "Editar faixa" (FaixaEditarR faixaId) (AlbumR albumId) campos enctype
    case resultado of
        FormSuccess nova -> do
            conflito <- runDB $ replaceUnique faixaId nova
            case conflito of
                Nothing -> do
                    setMessage "Faixa atualizada."
                    redirect (AlbumR albumId)
                Just _ -> avisoNumeroRepetido (faixaNumero nova) >> formulario
        _ -> formulario

-- POST /faixas/#FaixaId/excluir
postFaixaExcluirR :: FaixaId -> Handler Html
postFaixaExcluirR faixaId = do
    faixa <- runDB $ get404 faixaId
    runDB $ delete faixaId
    setMessage $ toHtml ("Faixa \"" <> faixaTitulo faixa <> "\" excluída.")
    redirect (AlbumR (faixaAlbumId faixa))
