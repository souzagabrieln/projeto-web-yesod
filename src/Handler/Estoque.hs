{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Controle de estoque: situacao atual, historico de movimentacoes
-- e registro de entrada de mercadoria comprada de fornecedores.
module Handler.Estoque where

import Foundation
import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime, getCurrentTimeZone)
import Model.Formato (nomeFormato)
import Model.Movimentacao
import Util.Validacao
import Util.Formatacao

-- Abaixo desta quantidade o album aparece como "estoque baixo".
estoqueMinimo :: Int
estoqueMinimo = 3

-- Situacao de um album conforme a quantidade em estoque.
situacao :: Int -> (Text, Text)      -- (classe CSS, texto)
situacao n
    | n <= 0            = ("zerado", "Sem estoque")
    | n < estoqueMinimo = ("baixo",  "Estoque baixo")
    | otherwise         = ("ok",     "OK")

------------------------------------------------------------------------
-- Entrada de mercadoria
------------------------------------------------------------------------

data DadosEntrada = DadosEntrada
    { deAlbum      :: AlbumId
    , deFornecedor :: FornecedorId
    , deQuantidade :: Int
    , deCusto      :: Double
    , deObservacao :: Maybe Text
    }

-- mAlbum: album ja selecionado quando a pagina e aberta a partir
-- da pagina de um album (/estoque/entrada?album=5).
entradaForm :: Maybe AlbumId -> Form DadosEntrada
entradaForm mAlbum = renderDivs $ DadosEntrada
    <$> areq (selectField opcoesAlbuns) "Álbum" mAlbum
    <*> areq (selectField fornecedores) "Fornecedor" Nothing
    <*> areq (check quantidadeValida intField) "Quantidade recebida" Nothing
    <*> areq (checkMMap (pure . lerPreco) (const "") textField) "Custo unitário pago (R$)" Nothing
    <*> (textoOpcional <$> aopt textField "Observação (ex.: número da nota fiscal)" Nothing)
  where
    fornecedores = optionsPersistKey [] [Asc FornecedorNome] fornecedorNome

quantidadeValida :: Int -> Either Text Int
quantidadeValida q
    | q <= 0    = Left "A quantidade deve ser maior que zero."
    | q > 10000 = Left "Quantidade alta demais para uma entrada. Confira o valor."
    | otherwise = Right q

-- Opcoes do <select> de albuns: "Titulo - Artista (Formato)".
-- O mesmo titulo pode existir em CD e vinil, por isso o formato aparece.
opcoesAlbuns :: Handler (OptionList AlbumId)
opcoesAlbuns = do
    albuns   <- runDB $ selectList [] [Asc AlbumTitulo]
    artistas <- runDB $ selectList [] []
    let nomes = Map.fromList [ (k, artistaNome a) | Entity k a <- artistas ]
        rotulo a = albumTitulo a <> " - " <> Map.findWithDefault "?" (albumArtistaId a) nomes
                   <> " (" <> nomeFormato (albumFormato a) <> ")"
    optionsPairs [ (rotulo a, k) | Entity k a <- albuns ]

-- Para registrar entrada e preciso ter album e fornecedor cadastrados.
verificarCadastros :: Handler (Maybe Widget)
verificarCadastros = do
    nAlbuns <- runDB $ count ([] :: [Filter Album])
    nForn   <- runDB $ count ([] :: [Filter Fornecedor])
    pure $ if nAlbuns > 0 && nForn > 0 then Nothing else Just [whamlet|
        <h1>Entrada de mercadoria
        <p .vazio>
            Para registrar uma entrada é preciso ter pelo menos um álbum e um fornecedor.
            <br>
            <a href=@{AlbunsR}>Cadastrar álbum
            \ · #
            <a href=@{FornecedorNovoR}>Cadastrar fornecedor
    |]

paginaEntrada :: Widget -> Enctype -> Widget
paginaEntrada campos enctype = do
    setTitle "Entrada de mercadoria"
    [whamlet|
        <h1>Entrada de mercadoria
        <p>Registra mercadoria recebida de um fornecedor. O estoque do álbum aumenta na hora.
        <form .formulario method=post action=@{EntradaEstoqueR} enctype=#{enctype}>
            ^{campos}
            <input type=submit value="Registrar entrada">
            <a .botao.secundario href=@{EstoqueR}>Cancelar
    |]

-- GET /estoque/entrada (opcional: ?album=<id>)
getEntradaEstoqueR :: Handler Html
getEntradaEstoqueR = do
    falta <- verificarCadastros
    case falta of
        Just aviso -> defaultLayout aviso
        Nothing -> do
            mAlbum <- (>>= fromPathPiece) <$> lookupGetParam "album"
            (campos, enctype) <- generateFormPost (entradaForm mAlbum)
            defaultLayout (paginaEntrada campos enctype)

-- POST /estoque/entrada
-- Grava a movimentacao e soma a quantidade no estoque do album. As duas
-- operacoes ficam no mesmo runDB, ou seja, na mesma transacao: nao existe
-- estoque alterado sem historico, nem historico sem estoque alterado.
postEntradaEstoqueR :: Handler Html
postEntradaEstoqueR = do
    ((resultado, campos), enctype) <- runFormPost (entradaForm Nothing)
    case resultado of
        FormSuccess d -> do
            agora <- liftIO getCurrentTime
            album <- runDB $ do
                album <- get404 (deAlbum d)
                insert_ MovimentacaoEstoque
                    { movimentacaoEstoqueAlbumId       = deAlbum d
                    , movimentacaoEstoqueTipo          = Entrada
                    , movimentacaoEstoqueQuantidade    = deQuantidade d
                    , movimentacaoEstoqueCustoUnitario = Just (deCusto d)
                    , movimentacaoEstoqueFornecedorId  = Just (deFornecedor d)
                    , movimentacaoEstoqueDataHora      = agora
                    , movimentacaoEstoqueObservacao    = deObservacao d
                    }
                -- +=. faz "estoque = estoque + quantidade" direto no banco
                update (deAlbum d) [AlbumEstoque +=. deQuantidade d]
                pure album
            let saldo = albumEstoque album + deQuantidade d
                alerta = if deCusto d >= albumPreco album
                    then " Atenção: o custo pago é maior ou igual ao preço de venda ("
                         <> formatarPreco (albumPreco album) <> ")."
                    else ""
            setMessage $ toHtml $ "Entrada de " <> T.pack (show (deQuantidade d))
                <> " unidade(s) de \"" <> albumTitulo album <> "\" registrada. Estoque atual: "
                <> T.pack (show saldo) <> "." <> alerta
            redirect (AlbumR (deAlbum d))
        _ -> defaultLayout (paginaEntrada campos enctype)

------------------------------------------------------------------------
-- Situacao do estoque e historico
------------------------------------------------------------------------

-- GET /estoque
getEstoqueR :: Handler Html
getEstoqueR = do
    fuso <- liftIO getCurrentTimeZone
    (albuns, artistas, fornecedores, movimentos) <- runDB $ do
        albuns       <- selectList [] [Asc AlbumEstoque, Asc AlbumTitulo]
        artistas     <- selectList [] []
        fornecedores <- selectList [] []
        movimentos   <- selectList [] [Desc MovimentacaoEstoqueDataHora, LimitTo 30]
        pure (albuns, artistas, fornecedores, movimentos)
    let nomeArtista = Map.fromList [ (k, artistaNome a) | Entity k a <- artistas ]
        nomeForn    = Map.fromList [ (k, fornecedorNome f) | Entity k f <- fornecedores ]
        porAlbum    = Map.fromList [ (k, a) | Entity k a <- albuns ]
        artistaDe a = Map.findWithDefault "-" (albumArtistaId a) nomeArtista
        tituloDe k  = maybe "(álbum removido)" albumTitulo (Map.lookup k porAlbum)
        fornDe m    = maybe "-" (\k -> Map.findWithDefault "-" k nomeForn) m
        totalUnidades = sum [ albumEstoque a | Entity _ a <- albuns ]
        valorVenda    = sum [ fromIntegral (albumEstoque a) * albumPreco a | Entity _ a <- albuns ]
        emAlerta      = length [ () | Entity _ a <- albuns, albumEstoque a < estoqueMinimo ]
    defaultLayout $ do
        setTitle "Estoque"
        [whamlet|
            <div .cabecalho-pagina>
                <h1>Estoque
                <a .botao href=@{EntradaEstoqueR}>Registrar entrada

            <div .cartoes>
                <div .cartao>
                    <div .numero>#{totalUnidades}
                    <p>unidades em estoque
                <div .cartao>
                    <div .numero>#{formatarPreco valorVenda}
                    <p>valor do estoque (preço de venda)
                <div .cartao>
                    <div .numero>#{emAlerta}
                    <p>álbuns com menos de #{estoqueMinimo} unidades

            <h2>Situação por álbum
            $if null albuns
                <p .vazio>Nenhum álbum cadastrado.
            $else
                <table>
                    <thead>
                        <tr>
                            <th>Álbum
                            <th>Artista
                            <th>Formato
                            <th>Estoque
                            <th>Situação
                            <th>
                    <tbody>
                        $forall Entity albumId a <- albuns
                            $with (classe, texto) <- situacao (albumEstoque a)
                                <tr>
                                    <td>
                                        <a href=@{AlbumR albumId}>#{albumTitulo a}
                                    <td>#{artistaDe a}
                                    <td>#{nomeFormato (albumFormato a)}
                                    <td>#{albumEstoque a}
                                    <td>
                                        <span .tag class=#{classe}>#{texto}
                                    <td>
                                        <a .botao.secundario href=@?{(EntradaEstoqueR, [("album", toPathPiece albumId)])}>Entrada

            <h2>Últimas movimentações
            $if null movimentos
                <p .vazio>Nenhuma movimentação registrada.
            $else
                <table>
                    <thead>
                        <tr>
                            <th>Data
                            <th>Álbum
                            <th>Tipo
                            <th>Qtd.
                            <th>Fornecedor
                            <th>Custo unit.
                            <th>Observação
                    <tbody>
                        $forall Entity _ m <- movimentos
                            <tr>
                                <td .nowrap>#{formatarDataHora fuso (movimentacaoEstoqueDataHora m)}
                                <td>
                                    <a href=@{AlbumR (movimentacaoEstoqueAlbumId m)}>#{tituloDe (movimentacaoEstoqueAlbumId m)}
                                <td>#{nomeTipo (movimentacaoEstoqueTipo m)}
                                <td>#{movimentacaoEstoqueQuantidade m}
                                <td>#{fornDe (movimentacaoEstoqueFornecedorId m)}
                                <td .nowrap>#{maybe "-" formatarPreco (movimentacaoEstoqueCustoUnitario m)}
                                <td>#{maybe "" id (movimentacaoEstoqueObservacao m)}
        |]
