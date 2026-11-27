{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- CRUD de clientes: listar, cadastrar, editar e excluir.
module Handler.Cliente where

import Foundation
import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Util.Validacao
import Util.Pagina
import Util.Cpf
import Handler.Fornecedor (validarTelefone)

-- Formulario de cliente. Serve para cadastro (Nothing) e edicao (Just ...).
clienteForm :: Maybe ClienteId -> Maybe Cliente -> Form Cliente
clienteForm mId mCliente = renderDivs $ Cliente
    <$> areq (check (nomeObrigatorio 100) textField) "Nome" (clienteNome <$> mCliente)
    <*> areq campoCpf "CPF" (clienteCpf <$> mCliente)
    <*> aopt emailField "E-mail" (clienteEmail <$> mCliente)
    <*> aopt (check validarTelefone textField) "Telefone" (clienteTelefone <$> mCliente)
  where
    -- Digita com ou sem pontuacao; no banco ficam so os 11 digitos.
    campoCpf = checkM (cpfDisponivel mId)
             $ checkMMap (pure . validarCpf) formatarCpf textField

-- Nao deixa cadastrar dois clientes com o mesmo CPF.
cpfDisponivel :: Maybe ClienteId -> Text -> Handler (Either Text Text)
cpfDisponivel mId cpf = do
    existente <- runDB $ getBy (UniqueClienteCpf cpf)
    pure $ case existente of
        Just (Entity outroId outro) | Just outroId /= mId ->
            Left ("Esse CPF já está cadastrado para " <> clienteNome outro <> ".")
        _ -> Right cpf

-- GET /clientes : lista com a quantidade de compras de cada cliente.
getClientesR :: Handler Html
getClientesR = do
    clientes <- runDB $ selectList [] [Asc ClienteNome]
    vendas   <- runDB $ selectList [VendaCancelada ==. False] []
    -- Map cliente -> numero de compras (fromListWith soma os repetidos)
    let compras = Map.fromListWith (+) [ (vendaClienteId v, 1 :: Int) | Entity _ v <- vendas ]
        comprasDe k = Map.findWithDefault 0 k compras
    defaultLayout $ do
        setTitle "Clientes"
        [whamlet|
            <div .cabecalho-pagina>
                <h1>Clientes
                <a .botao href=@{ClienteNovoR}>Novo cliente
            $if null clientes
                <p .vazio>Nenhum cliente cadastrado ainda.
            $else
                <table>
                    <thead>
                        <tr>
                            <th>Nome
                            <th>CPF
                            <th>E-mail
                            <th>Telefone
                            <th>Compras
                            <th>Ações
                    <tbody>
                        $forall Entity clienteId c <- clientes
                            <tr>
                                <td>#{clienteNome c}
                                <td .nowrap>#{formatarCpf (clienteCpf c)}
                                <td>#{maybe "-" id (clienteEmail c)}
                                <td .nowrap>#{maybe "-" id (clienteTelefone c)}
                                <td>#{comprasDe clienteId}
                                <td .acoes>
                                    <a .botao.secundario href=@{ClienteEditarR clienteId}>Editar
                                    ^{botaoExcluir (ClienteExcluirR clienteId) (clienteNome c)}
        |]

-- GET /clientes/novo
getClienteNovoR :: Handler Html
getClienteNovoR = do
    (campos, enctype) <- generateFormPost (clienteForm Nothing Nothing)
    defaultLayout $ paginaFormulario "Novo cliente" ClienteNovoR ClientesR campos enctype

-- POST /clientes/novo
postClienteNovoR :: Handler Html
postClienteNovoR = do
    ((resultado, campos), enctype) <- runFormPost (clienteForm Nothing Nothing)
    case resultado of
        FormSuccess c -> do
            _ <- runDB $ insert c
            setMessage $ toHtml ("Cliente \"" <> clienteNome c <> "\" cadastrado.")
            redirect ClientesR
        _ -> defaultLayout $ paginaFormulario "Novo cliente" ClienteNovoR ClientesR campos enctype

-- GET /clientes/#ClienteId/editar
getClienteEditarR :: ClienteId -> Handler Html
getClienteEditarR clienteId = do
    c <- runDB $ get404 clienteId
    (campos, enctype) <- generateFormPost (clienteForm (Just clienteId) (Just c))
    defaultLayout $
        paginaFormulario "Editar cliente" (ClienteEditarR clienteId) ClientesR campos enctype

-- POST /clientes/#ClienteId/editar
postClienteEditarR :: ClienteId -> Handler Html
postClienteEditarR clienteId = do
    c <- runDB $ get404 clienteId
    ((resultado, campos), enctype) <- runFormPost (clienteForm (Just clienteId) (Just c))
    case resultado of
        FormSuccess novo -> do
            runDB $ replace clienteId novo
            setMessage $ toHtml ("Cliente \"" <> clienteNome novo <> "\" atualizado.")
            redirect ClientesR
        _ -> defaultLayout $
            paginaFormulario "Editar cliente" (ClienteEditarR clienteId) ClientesR campos enctype

-- POST /clientes/#ClienteId/excluir
-- Cliente com vendas (mesmo canceladas) nao pode ser excluido.
postClienteExcluirR :: ClienteId -> Handler Html
postClienteExcluirR clienteId = do
    c <- runDB $ get404 clienteId
    nVendas <- runDB $ count [VendaClienteId ==. clienteId]
    if nVendas > 0
        then setMessage $ toHtml ("Não é possível excluir \"" <> clienteNome c <> "\": há "
                <> T.pack (show nVendas) <> " venda(s) registradas para esse cliente.")
        else do
            runDB $ delete clienteId
            setMessage $ toHtml ("Cliente \"" <> clienteNome c <> "\" excluído.")
    redirect ClientesR
