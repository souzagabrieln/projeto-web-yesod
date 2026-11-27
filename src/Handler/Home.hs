{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Pagina inicial: painel com os indicadores da loja (vendas do mes,
-- estoque, mais vendidos, estoque baixo e ultimas vendas).
module Handler.Home where

import Foundation
import Yesod
import Database.Persist.Sql (rawSql, Single (..))
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Time
import Model.Formato (nomeFormato)
import Util.Formatacao
import Handler.Estoque (estoqueMinimo, situacao)

-- Nome do mes em portugues (1 = janeiro).
nomeMes :: Int -> Text
nomeMes m = case drop (m - 1) meses of
    (nome : _) -> nome
    []         -> ""
  where
    meses = [ "janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho"
            , "agosto", "setembro", "outubro", "novembro", "dezembro" ]

-- Inicio do mes atual (dia 1, 00:00 no fuso do computador), em UTC,
-- que e como as datas ficam gravadas no banco.
inicioDoMes :: TimeZone -> UTCTime -> (UTCTime, Int)
inicioDoMes fuso agora = (localTimeToUTC fuso (LocalTime primeiroDia midnight), mes)
  where
    (ano, mes, _) = toGregorian (localDay (utcToLocalTime fuso agora))
    primeiroDia   = fromGregorian ano mes 1

-- Os 5 albuns mais vendidos (sem contar vendas canceladas), com a
-- quantidade de unidades e o valor vendido.
--
-- Esta consulta usa SQL direto (rawSql) porque precisa de JOIN entre tres
-- tabelas, SUM e GROUP BY, que o selectList nao faz. O "??" e trocado pelo
-- Persistent pelas colunas da tabela album, e o "?" recebe o parametro
-- (assim o valor nunca e colado no texto do SQL).
maisVendidos :: Handler [(Entity Album, Single Int, Single Double)]
maisVendidos = runDB $ rawSql
    "SELECT ??, SUM(item_venda.quantidade), \
    \       SUM(item_venda.quantidade * item_venda.preco_unitario) \
    \FROM album \
    \JOIN item_venda ON item_venda.album_id = album.id \
    \JOIN venda      ON venda.id = item_venda.venda_id \
    \WHERE venda.cancelada = ? \
    \GROUP BY album.id \
    \ORDER BY SUM(item_venda.quantidade) DESC, album.titulo \
    \LIMIT 5"
    [PersistBool False]

getHomeR :: Handler Html
getHomeR = do
    fuso  <- liftIO getCurrentTimeZone
    agora <- liftIO getCurrentTime
    let (inicioMes, mes) = inicioDoMes fuso agora

    -- Vendas do mes: filtro por data (>=.) e so as nao canceladas.
    vendasMes <- runDB $ selectList [VendaDataHora >=. inicioMes, VendaCancelada ==. False] []
    albuns    <- runDB $ selectList [] []
    artistas  <- runDB $ selectList [] []
    -- Albuns com estoque abaixo do minimo, os mais criticos primeiro.
    baixo     <- runDB $ selectList [AlbumEstoque <. estoqueMinimo]
                                    [Asc AlbumEstoque, Asc AlbumTitulo, LimitTo 6]
    nBaixo    <- runDB $ count [AlbumEstoque <. estoqueMinimo]
    ranking   <- maisVendidos
    ultimas   <- runDB $ selectList [] [Desc VendaDataHora, Desc VendaId, LimitTo 5]
    clientes  <- runDB $ selectList [ClienteId <-. map (vendaClienteId . entityVal) ultimas] []
    nArtistas <- runDB $ count ([] :: [Filter Artista])
    nClientes <- runDB $ count ([] :: [Filter Cliente])

    let faturamentoMes = sum [ vendaTotal v | Entity _ v <- vendasMes ]
        qtdVendasMes   = length vendasMes
        ticketMedio    = if qtdVendasMes == 0 then 0
                         else faturamentoMes / fromIntegral qtdVendasMes
        unidades       = sum [ albumEstoque a | Entity _ a <- albuns ]
        valorEstoque   = sum [ fromIntegral (albumEstoque a) * albumPreco a | Entity _ a <- albuns ]
        nomeArtista    = Map.fromList [ (k, artistaNome a) | Entity k a <- artistas ]
        nomeCliente    = Map.fromList [ (k, clienteNome c) | Entity k c <- clientes ]
        artistaDe a    = Map.findWithDefault "-" (albumArtistaId a) nomeArtista
        -- (posicao, id, album, unidades vendidas, valor vendido)
        linhasRanking  = [ (n, k, a, u, r) | (n, (Entity k a, Single u, Single r)) <- zip [1 :: Int ..] ranking ]
        catalogoVazio  = null albuns

    defaultLayout $ do
        setTitle "Painel"
        [whamlet|
            <div .cabecalho-pagina>
                <h1>Painel da loja
                <div .botoes-topo>
                    <a .botao href=@{VendaNovaR}>Nova venda
                    <a .botao.secundario href=@{EntradaEstoqueR}>Registrar entrada
                    <a .botao.secundario href=@{BuscaMusicBrainzR}>Importar álbum

            $if catalogoVazio
                <div .vazio.primeiros-passos>
                    <h2>Primeiros passos
                    <ol>
                        <li>
                            <a href=@{GeneroNovoR}>Cadastre os gêneros
                            \ que a loja trabalha.
                        <li>
                            <a href=@{BuscaMusicBrainzR}>Importe álbuns do MusicBrainz
                            \ (ou cadastre à mão).
                        <li>
                            <a href=@{FornecedorNovoR}>Cadastre um fornecedor
                            \ e registre a entrada de mercadoria.
                        <li>
                            <a href=@{ClienteNovoR}>Cadastre clientes
                            \ e registre as vendas.

            <div .cartoes>
                <div .cartao>
                    <div .numero>#{qtdVendasMes}
                    <p>vendas em #{nomeMes mes}
                    <p .detalhe>Ticket médio: #{formatarPreco ticketMedio}
                <div .cartao>
                    <div .numero>#{formatarPreco faturamentoMes}
                    <p>faturamento em #{nomeMes mes}
                <div .cartao>
                    <div .numero>#{unidades}
                    <p>unidades em estoque
                    <p .detalhe>Valor de venda: #{formatarPreco valorEstoque}
                <div .cartao>
                    <div .numero>#{nBaixo}
                    <p>álbuns com menos de #{estoqueMinimo} unidades
                    <p .detalhe>
                        <a href=@?{(AlbunsR, [("estoque", "baixo")])}>Ver estoque baixo
                        \ · #
                        <a href=@?{(AlbunsR, [("estoque", "zerado")])}>sem estoque

            <div .colunas>
                <section>
                    <h2>Mais vendidos
                    $if null linhasRanking
                        <p .vazio>Nenhuma venda registrada ainda.
                    $else
                        <table>
                            <thead>
                                <tr>
                                    <th>#
                                    <th>Álbum
                                    <th>Unid.
                                    <th>Vendido
                            <tbody>
                                $forall (n, albumId, album, qtd, valor) <- linhasRanking
                                    <tr>
                                        <td>#{n}
                                        <td>
                                            <a href=@{AlbumR albumId}>#{albumTitulo album}
                                            <div .detalhe>#{artistaDe album} · #{nomeFormato (albumFormato album)}
                                        <td>#{qtd}
                                        <td .nowrap>#{formatarPreco valor}

                <section>
                    <h2>Estoque baixo
                    $if null baixo
                        <p .vazio>Todos os álbuns têm pelo menos #{estoqueMinimo} unidades.
                    $else
                        <table>
                            <thead>
                                <tr>
                                    <th>Álbum
                                    <th>Estoque
                                    <th>
                            <tbody>
                                $forall Entity albumId album <- baixo
                                    $with (classe, texto) <- situacao (albumEstoque album)
                                        <tr>
                                            <td>
                                                <a href=@{AlbumR albumId}>#{albumTitulo album}
                                                <div .detalhe>#{artistaDe album} · #{nomeFormato (albumFormato album)}
                                            <td>
                                                <span .tag class=#{classe}>#{albumEstoque album} · #{texto}
                                            <td>
                                                <a .botao.secundario href=@?{(EntradaEstoqueR, [("album", toPathPiece albumId)])}>Entrada

            <h2>Últimas vendas
            $if null ultimas
                <p .vazio>Nenhuma venda registrada ainda.
            $else
                <table>
                    <thead>
                        <tr>
                            <th>Nº
                            <th>Data
                            <th>Cliente
                            <th>Total
                            <th>Situação
                    <tbody>
                        $forall Entity vendaId v <- ultimas
                            <tr :vendaCancelada v:.cancelada>
                                <td>
                                    <a href=@{VendaR vendaId}>#{toPathPiece vendaId}
                                <td .nowrap>#{formatarDataHora fuso (vendaDataHora v)}
                                <td>#{Map.findWithDefault "-" (vendaClienteId v) nomeCliente}
                                <td .nowrap>#{formatarPreco (vendaTotal v)}
                                <td>
                                    $if vendaCancelada v
                                        <span .tag.zerado>Cancelada
                                    $else
                                        <span .tag.ok>Concluída

            <p .resumo-catalogo>
                Catálogo: #{length albuns} álbum(ns), #{nArtistas} artista(s) e #{nClientes} cliente(s).
        |]
