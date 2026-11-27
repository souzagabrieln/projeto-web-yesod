{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- CRUD de fornecedores: listar, cadastrar, editar e excluir.
module Handler.Fornecedor where

import Foundation
import Yesod
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Util.Validacao
import Util.Pagina
import Util.Cnpj

-- Formulario de fornecedor. Serve para cadastro (Nothing) e edicao (Just ...).
fornecedorForm :: Maybe FornecedorId -> Maybe Fornecedor -> Form Fornecedor
fornecedorForm mId mForn = renderDivs $ Fornecedor
    <$> areq (check (nomeObrigatorio 100) textField) "Nome / razão social" (fornecedorNome <$> mForn)
    <*> areq campoCnpj "CNPJ" (fornecedorCnpj <$> mForn)
    <*> aopt emailField "E-mail" (fornecedorEmail <$> mForn)
    <*> aopt campoTelefone "Telefone" (fornecedorTelefone <$> mForn)
  where
    -- O usuario digita com ou sem pontuacao; no banco ficam so os digitos.
    -- Na edicao, o campo mostra o CNPJ ja formatado.
    campoCnpj = checkM (cnpjDisponivel mId)
              $ checkMMap (pure . validarCnpj) formatarCnpj textField
    campoTelefone = check validarTelefone textField

-- Telefone com DDD: 10 ou 11 digitos, aceitando espacos, parenteses e hifen.
validarTelefone :: Text -> Either Text Text
validarTelefone t
    | n == 10 || n == 11 = Right (T.strip t)
    | otherwise          = Left "Informe o telefone com DDD, por exemplo (11) 98765-4321."
  where
    n = T.length (T.filter isDigit t)

-- Nao deixa cadastrar dois fornecedores com o mesmo CNPJ.
cnpjDisponivel :: Maybe FornecedorId -> Text -> Handler (Either Text Text)
cnpjDisponivel mId cnpj = do
    existente <- runDB $ getBy (UniqueFornecedorCnpj cnpj)
    pure $ case existente of
        Just (Entity outroId outro) | Just outroId /= mId ->
            Left ("Esse CNPJ já está cadastrado para " <> fornecedorNome outro <> ".")
        _ -> Right cnpj

-- GET /fornecedores
getFornecedoresR :: Handler Html
getFornecedoresR = do
    fornecedores <- runDB $ selectList [] [Asc FornecedorNome]
    defaultLayout $ do
        setTitle "Fornecedores"
        [whamlet|
            <div .cabecalho-pagina>
                <h1>Fornecedores
                <a .botao href=@{FornecedorNovoR}>Novo fornecedor
            $if null fornecedores
                <p .vazio>Nenhum fornecedor cadastrado ainda.
            $else
                <table>
                    <thead>
                        <tr>
                            <th>Nome
                            <th>CNPJ
                            <th>E-mail
                            <th>Telefone
                            <th>Ações
                    <tbody>
                        $forall Entity fornecedorId f <- fornecedores
                            <tr>
                                <td>#{fornecedorNome f}
                                <td .nowrap>#{formatarCnpj (fornecedorCnpj f)}
                                <td>#{maybe "-" id (fornecedorEmail f)}
                                <td .nowrap>#{maybe "-" id (fornecedorTelefone f)}
                                <td .acoes>
                                    <a .botao.secundario href=@{FornecedorEditarR fornecedorId}>Editar
                                    ^{botaoExcluir (FornecedorExcluirR fornecedorId) (fornecedorNome f)}
        |]

-- GET /fornecedores/novo
getFornecedorNovoR :: Handler Html
getFornecedorNovoR = do
    (campos, enctype) <- generateFormPost (fornecedorForm Nothing Nothing)
    defaultLayout $ paginaFormulario "Novo fornecedor" FornecedorNovoR FornecedoresR campos enctype

-- POST /fornecedores/novo
postFornecedorNovoR :: Handler Html
postFornecedorNovoR = do
    ((resultado, campos), enctype) <- runFormPost (fornecedorForm Nothing Nothing)
    case resultado of
        FormSuccess f -> do
            _ <- runDB $ insert f
            setMessage $ toHtml ("Fornecedor \"" <> fornecedorNome f <> "\" cadastrado.")
            redirect FornecedoresR
        _ -> defaultLayout $ paginaFormulario "Novo fornecedor" FornecedorNovoR FornecedoresR campos enctype

-- GET /fornecedores/#FornecedorId/editar
getFornecedorEditarR :: FornecedorId -> Handler Html
getFornecedorEditarR fornecedorId = do
    f <- runDB $ get404 fornecedorId
    (campos, enctype) <- generateFormPost (fornecedorForm (Just fornecedorId) (Just f))
    defaultLayout $
        paginaFormulario "Editar fornecedor" (FornecedorEditarR fornecedorId) FornecedoresR campos enctype

-- POST /fornecedores/#FornecedorId/editar
postFornecedorEditarR :: FornecedorId -> Handler Html
postFornecedorEditarR fornecedorId = do
    f <- runDB $ get404 fornecedorId
    ((resultado, campos), enctype) <- runFormPost (fornecedorForm (Just fornecedorId) (Just f))
    case resultado of
        FormSuccess novo -> do
            runDB $ replace fornecedorId novo
            setMessage $ toHtml ("Fornecedor \"" <> fornecedorNome novo <> "\" atualizado.")
            redirect FornecedoresR
        _ -> defaultLayout $
            paginaFormulario "Editar fornecedor" (FornecedorEditarR fornecedorId) FornecedoresR campos enctype

-- POST /fornecedores/#FornecedorId/excluir
-- Fornecedor com entradas registradas nao pode ser excluido: o
-- historico de estoque perderia a informacao de onde veio a mercadoria.
postFornecedorExcluirR :: FornecedorId -> Handler Html
postFornecedorExcluirR fornecedorId = do
    f <- runDB $ get404 fornecedorId
    entradas <- runDB $ count [MovimentacaoEstoqueFornecedorId ==. Just fornecedorId]
    if entradas > 0
        then setMessage $ toHtml ("Não é possível excluir \"" <> fornecedorNome f <> "\": há "
                <> T.pack (show entradas) <> " entrada(s) de estoque registradas com esse fornecedor.")
        else do
            runDB $ delete fornecedorId
            setMessage $ toHtml ("Fornecedor \"" <> fornecedorNome f <> "\" excluído.")
    redirect FornecedoresR
