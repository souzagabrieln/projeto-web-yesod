{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Vendas: registrar venda com varios albuns, dar baixa no estoque,
-- listar, ver detalhes e cancelar (devolvendo o estoque).
module Handler.Venda where

import Foundation
import Yesod
import Control.Monad (forM, forM_)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime, getCurrentTimeZone)
import Model.Formato (nomeFormato)
import Model.Movimentacao (TipoMovimentacao (..))
import Util.Formatacao
import Util.Cpf (formatarCpf)

-- Quantas linhas de item o formulario de venda mostra.
linhasVenda :: Int
linhasVenda = 5

data DadosVenda = DadosVenda
    { dvCliente :: ClienteId
    , dvItens   :: [(AlbumId, Int)]
    }

-- Numero da venda exibido na tela ("nº 12").
numeroVenda :: VendaId -> Text
numeroVenda = toPathPiece

------------------------------------------------------------------------
-- Formulario de venda (formulario monadico)
------------------------------------------------------------------------

-- O formulario tem um cliente e 5 linhas de item (album + quantidade).
-- Linhas em branco sao ignoradas. Como o numero de campos e o layout
-- sao personalizados, ele e um formulario monadico (mreq/mopt), em que
-- cada campo devolve o resultado e o "FieldView" para montar o HTML.
vendaForm :: Html -> MForm Handler (FormResult DadosVenda, Widget)
vendaForm extra = do
    (clienteRes, clienteView) <- mreq (selectField clientes) "Cliente" Nothing
    linhas <- forM [1 .. linhasVenda] $ \_ -> do
        (albumRes, albumView) <- mopt (selectField opcoesAlbunsEmEstoque) "Álbum" Nothing
        (qtdRes, qtdView)     <- mopt (check quantidadeValida intField) "Quantidade" Nothing
        pure ((,) <$> albumRes <*> qtdRes, (albumView, qtdView))
    let resultadosLinhas = map fst linhas
        views = zip [1 :: Int ..] (map snd linhas)
        -- Erros que envolvem a linha inteira (album sem quantidade etc.)
        (itensRes, errosLinhas) = case sequenceA resultadosLinhas of
            FormSuccess ls -> case validarItens ls of
                Right itens -> (FormSuccess itens, [])
                Left erros  -> (FormFailure erros, erros)
            FormFailure e -> (FormFailure e, [])
            FormMissing   -> (FormMissing, [])
        resultado = DadosVenda <$> clienteRes <*> itensRes
        widget = [whamlet|
            #{extra}
            <div .required>
                <label for=#{fvId clienteView}>Cliente
                ^{fvInput clienteView}
                $maybe erro <- fvErrors clienteView
                    <div .errors>#{erro}
            <table .itens-venda>
                <thead>
                    <tr>
                        <th>#
                        <th>Álbum
                        <th>Quantidade
                <tbody>
                    $forall (n, (albumView, qtdView)) <- views
                        <tr>
                            <td>#{n}
                            <td>
                                ^{fvInput albumView}
                                $maybe erro <- fvErrors albumView
                                    <div .errors>#{erro}
                            <td>
                                ^{fvInput qtdView}
                                $maybe erro <- fvErrors qtdView
                                    <div .errors>#{erro}
            $forall erro <- errosLinhas
                <p .errors>#{erro}
        |]
    pure (resultado, widget)
  where
    clientes = optionsPersistKey [] [Asc ClienteNome] clienteNome

-- Confere as linhas preenchidas. Albuns repetidos em linhas diferentes
-- sao somados (Map.fromListWith (+)).
validarItens :: [(Maybe AlbumId, Maybe Int)] -> Either [Text] [(AlbumId, Int)]
validarItens linhas
    | not (null erros) = Left erros
    | null itens       = Left ["Escolha pelo menos um álbum."]
    | otherwise        = Right (Map.toList (Map.fromListWith (+) itens))
  where
    numeradas = zip [1 :: Int ..] linhas
    erros = [ "Linha " <> T.pack (show n) <> ": informe a quantidade."
            | (n, (Just _, Nothing)) <- numeradas ]
         ++ [ "Linha " <> T.pack (show n) <> ": escolha o álbum."
            | (n, (Nothing, Just _)) <- numeradas ]
    itens = [ (a, q) | (_, (Just a, Just q)) <- numeradas ]

quantidadeValida :: Int -> Either Text Int
quantidadeValida q
    | q <= 0    = Left "Mínimo 1."
    | q > 100   = Left "Máximo 100 por linha."
    | otherwise = Right q

-- So aparecem albuns com estoque: "Titulo - Artista (Formato) | R$ 59,90 | 3 em estoque"
opcoesAlbunsEmEstoque :: Handler (OptionList AlbumId)
opcoesAlbunsEmEstoque = do
    albuns   <- runDB $ selectList [AlbumEstoque >. 0] [Asc AlbumTitulo]
    artistas <- runDB $ selectList [] []
    let nomes = Map.fromList [ (k, artistaNome a) | Entity k a <- artistas ]
        rotulo a = albumTitulo a <> " - " <> Map.findWithDefault "?" (albumArtistaId a) nomes
            <> " (" <> nomeFormato (albumFormato a) <> ") | " <> formatarPreco (albumPreco a)
            <> " | " <> T.pack (show (albumEstoque a)) <> " em estoque"
    optionsPairs [ (rotulo a, k) | Entity k a <- albuns ]

------------------------------------------------------------------------
-- Registrar venda
------------------------------------------------------------------------

-- problemas: avisos de estoque insuficiente, vindos do POST.
paginaNovaVenda :: [Text] -> Widget -> Enctype -> Widget
paginaNovaVenda problemas campos enctype = do
    setTitle "Nova venda"
    [whamlet|
        <h1>Nova venda
        <p>Escolha o cliente e até #{linhasVenda} álbuns. Linhas em branco são ignoradas.
        $if not (null problemas)
            <div .erro>
                <strong>A venda não foi registrada:
                <ul>
                    $forall p <- problemas
                        <li>#{p}
        <form .formulario.formulario-largo method=post action=@{VendaNovaR} enctype=#{enctype}>
            ^{campos}
            <input type=submit value="Registrar venda">
            <a .botao.secundario href=@{VendasR}>Cancelar
    |]

-- GET /vendas/nova
getVendaNovaR :: Handler Html
getVendaNovaR = do
    nClientes <- runDB $ count ([] :: [Filter Cliente])
    nAlbuns   <- runDB $ count [AlbumEstoque >. 0]
    if nClientes == 0 || nAlbuns == 0
        then defaultLayout [whamlet|
            <h1>Nova venda
            <p .vazio>
                Para vender é preciso ter pelo menos um cliente e um álbum com estoque.
                <br>
                <a href=@{ClienteNovoR}>Cadastrar cliente
                \ · #
                <a href=@{EntradaEstoqueR}>Registrar entrada de estoque
        |]
        else do
            (campos, enctype) <- generateFormPost vendaForm
            defaultLayout (paginaNovaVenda [] campos enctype)

-- POST /vendas/nova
-- Tudo acontece dentro de um unico runDB (uma transacao):
--   1. le cada album e confere se ha estoque suficiente;
--   2. se faltar algum, nao grava nada e devolve a lista de problemas;
--   3. senao grava a venda, os itens, baixa o estoque e registra a
--      movimentacao de saida de cada album.
postVendaNovaR :: Handler Html
postVendaNovaR = do
    ((resultado, campos), enctype) <- runFormPost vendaForm
    case resultado of
        FormSuccess dados -> do
            agora <- liftIO getCurrentTime
            gravacao <- runDB $ do
                albuns <- forM (dvItens dados) $ \(albumId, qtd) -> do
                    mAlbum <- get albumId
                    pure (albumId, qtd, mAlbum)
                let problemas = mapMaybe faltaEstoque albuns
                    itens = [ (k, q, a) | (k, q, Just a) <- albuns ]
                    total = sum [ fromIntegral q * albumPreco a | (_, q, a) <- itens ]
                if not (null problemas)
                    then pure (Left problemas)
                    else do
                        vendaId <- insert (Venda (dvCliente dados) agora total False)
                        forM_ itens $ \(albumId, qtd, album) -> do
                            insert_ (ItemVenda vendaId albumId qtd (albumPreco album))
                            update albumId [AlbumEstoque -=. qtd]
                            insert_ MovimentacaoEstoque
                                { movimentacaoEstoqueAlbumId       = albumId
                                , movimentacaoEstoqueTipo          = Saida
                                , movimentacaoEstoqueQuantidade    = qtd
                                , movimentacaoEstoqueCustoUnitario = Nothing
                                , movimentacaoEstoqueFornecedorId  = Nothing
                                , movimentacaoEstoqueDataHora      = agora
                                , movimentacaoEstoqueObservacao    = Just ("Venda nº " <> numeroVenda vendaId)
                                }
                        pure (Right (vendaId, total))
            case gravacao of
                Right (vendaId, total) -> do
                    setMessage $ toHtml ("Venda nº " <> numeroVenda vendaId
                        <> " registrada. Total: " <> formatarPreco total <> ".")
                    redirect (VendaR vendaId)
                Left problemas ->
                    defaultLayout (paginaNovaVenda problemas campos enctype)
        _ -> defaultLayout (paginaNovaVenda [] campos enctype)
  where
    faltaEstoque (_, _, Nothing) = Just "Um dos álbuns escolhidos foi removido do catálogo."
    faltaEstoque (_, qtd, Just a)
        | albumEstoque a < qtd = Just ("\"" <> albumTitulo a <> "\": pedido de "
            <> T.pack (show qtd) <> ", mas só há " <> T.pack (show (albumEstoque a)) <> " em estoque.")
        | otherwise = Nothing

------------------------------------------------------------------------
-- Listagem, detalhes e cancelamento
------------------------------------------------------------------------

-- GET /vendas
getVendasR :: Handler Html
getVendasR = do
    fuso <- liftIO getCurrentTimeZone
    (vendas, clientes, itens) <- runDB $ do
        vendas   <- selectList [] [Desc VendaDataHora, Desc VendaId]
        clientes <- selectList [] []
        itens    <- selectList [] []
        pure (vendas, clientes, itens)
    let nomeCliente = Map.fromList [ (k, clienteNome c) | Entity k c <- clientes ]
        -- unidades vendidas em cada venda
        unidades = Map.fromListWith (+) [ (itemVendaVendaId i, itemVendaQuantidade i) | Entity _ i <- itens ]
        validas = [ v | Entity _ v <- vendas, not (vendaCancelada v) ]
        faturamento = sum (map vendaTotal validas)
    defaultLayout $ do
        setTitle "Vendas"
        [whamlet|
            <div .cabecalho-pagina>
                <h1>Vendas
                <a .botao href=@{VendaNovaR}>Nova venda
            <div .cartoes>
                <div .cartao>
                    <div .numero>#{length validas}
                    <p>vendas concluídas
                <div .cartao>
                    <div .numero>#{formatarPreco faturamento}
                    <p>faturamento (sem as canceladas)
            $if null vendas
                <p .vazio>Nenhuma venda registrada ainda.
            $else
                <table>
                    <thead>
                        <tr>
                            <th>Nº
                            <th>Data
                            <th>Cliente
                            <th>Unidades
                            <th>Total
                            <th>Situação
                    <tbody>
                        $forall Entity vendaId v <- vendas
                            <tr :vendaCancelada v:.cancelada>
                                <td>
                                    <a href=@{VendaR vendaId}>#{numeroVenda vendaId}
                                <td .nowrap>#{formatarDataHora fuso (vendaDataHora v)}
                                <td>#{Map.findWithDefault "-" (vendaClienteId v) nomeCliente}
                                <td>#{Map.findWithDefault 0 vendaId unidades}
                                <td .nowrap>#{formatarPreco (vendaTotal v)}
                                <td>
                                    $if vendaCancelada v
                                        <span .tag.zerado>Cancelada
                                    $else
                                        <span .tag.ok>Concluída
        |]

-- GET /vendas/#VendaId
getVendaR :: VendaId -> Handler Html
getVendaR vendaId = do
    fuso <- liftIO getCurrentTimeZone
    (venda, cliente, itens) <- runDB $ do
        venda   <- get404 vendaId
        cliente <- get404 (vendaClienteId venda)
        itens   <- selectList [ItemVendaVendaId ==. vendaId] []
        -- junta cada item com o album correspondente
        comAlbum <- forM itens $ \(Entity _ i) -> do
            album <- get (itemVendaAlbumId i)
            pure (i, album)
        pure (venda, cliente, comAlbum)
    let subtotal i = fromIntegral (itemVendaQuantidade i) * itemVendaPrecoUnitario i
    defaultLayout $ do
        setTitle (toHtml ("Venda nº " <> numeroVenda vendaId))
        [whamlet|
            <div .cabecalho-pagina>
                <h1>Venda nº #{numeroVenda vendaId}
                $if vendaCancelada venda
                    <span .tag.zerado>Cancelada
                $else
                    <span .tag.ok>Concluída
            <dl .dados-venda>
                <dt>Cliente
                <dd>#{clienteNome cliente} (CPF #{formatarCpf (clienteCpf cliente)})
                <dt>Data
                <dd>#{formatarDataHora fuso (vendaDataHora venda)}
            <table>
                <thead>
                    <tr>
                        <th>Álbum
                        <th>Qtd.
                        <th>Preço unit.
                        <th>Subtotal
                <tbody>
                    $forall (item, mAlbum) <- itens
                        <tr>
                            <td>
                                $maybe album <- mAlbum
                                    <a href=@{AlbumR (itemVendaAlbumId item)}>#{albumTitulo album}
                                    \ (#{nomeFormato (albumFormato album)})
                                $nothing
                                    (álbum removido)
                            <td>#{itemVendaQuantidade item}
                            <td .nowrap>#{formatarPreco (itemVendaPrecoUnitario item)}
                            <td .nowrap>#{formatarPreco (subtotal item)}
                <tfoot>
                    <tr>
                        <td colspan=3>Total
                        <td .nowrap>#{formatarPreco (vendaTotal venda)}
            <div .botoes>
                <a .botao.secundario href=@{VendasR}>Voltar às vendas
                $if not (vendaCancelada venda)
                    <form .inline method=post action=@{VendaCancelarR vendaId} onsubmit="return confirm('Cancelar esta venda e devolver os itens ao estoque?');">
                        <button .perigo type=submit>Cancelar venda
        |]

-- POST /vendas/#VendaId/cancelar
-- Marca a venda como cancelada e devolve cada item ao estoque, com uma
-- movimentacao de entrada explicando o motivo. Tudo na mesma transacao.
postVendaCancelarR :: VendaId -> Handler Html
postVendaCancelarR vendaId = do
    venda <- runDB $ get404 vendaId
    if vendaCancelada venda
        then setMessage "Esta venda já estava cancelada."
        else do
            agora <- liftIO getCurrentTime
            runDB $ do
                itens <- selectList [ItemVendaVendaId ==. vendaId] []
                forM_ itens $ \(Entity _ i) -> do
                    update (itemVendaAlbumId i) [AlbumEstoque +=. itemVendaQuantidade i]
                    insert_ MovimentacaoEstoque
                        { movimentacaoEstoqueAlbumId       = itemVendaAlbumId i
                        , movimentacaoEstoqueTipo          = Entrada
                        , movimentacaoEstoqueQuantidade    = itemVendaQuantidade i
                        , movimentacaoEstoqueCustoUnitario = Nothing
                        , movimentacaoEstoqueFornecedorId  = Nothing
                        , movimentacaoEstoqueDataHora      = agora
                        , movimentacaoEstoqueObservacao    = Just ("Cancelamento da venda nº " <> numeroVenda vendaId)
                        }
                update vendaId [VendaCancelada =. True]
            setMessage $ toHtml ("Venda nº " <> numeroVenda vendaId <> " cancelada. Os itens voltaram ao estoque.")
    redirect (VendaR vendaId)
