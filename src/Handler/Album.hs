{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- CRUD de albuns e pagina de detalhes do album (com as faixas).
module Handler.Album where

import Foundation
import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Model.Formato
import Util.Validacao
import Util.Formatacao
import Util.Pagina
import Handler.Faixa (faixaForm, proximoNumero, avisoNumeroRepetido)
import Data.Time (getCurrentTimeZone)
import Model.Movimentacao (nomeTipo)
import Handler.Estoque (estoqueMinimo, situacao)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- Formulario de album. Serve para cadastro (Nothing) e edicao (Just ...).
-- Artista e genero sao escolhidos em listas carregadas do banco.
-- O estoque nao aparece no formulario: ele so muda por entradas e vendas.
albumForm :: Integer -> Maybe Album -> Form Album
albumForm anoAtual mAlbum = renderDivs $ Album
    <$> areq campoTitulo "Título" (albumTitulo <$> mAlbum)
    <*> areq (selectField artistas) "Artista" (albumArtistaId <$> mAlbum)
    <*> areq (selectField generos) "Gênero" (albumGeneroId <$> mAlbum)
    <*> aopt (check (anoValido anoAtual) intField) "Ano de lançamento" (albumAno <$> mAlbum)
    <*> areq (selectFieldList opcoesFormato) "Formato" (albumFormato <$> mAlbum)
    <*> areq campoPreco "Preço de venda (R$)" (albumPreco <$> mAlbum)
    <*> pure (maybe 0 albumEstoque mAlbum)
    <*> (textoOpcional <$> aopt urlField "Endereço da imagem da capa" (albumCapaUrl <$> mAlbum))
  where
    campoTitulo = check (tituloObrigatorio 150) textField
    -- optionsPersistKey monta as opcoes do <select> a partir de uma tabela
    artistas = optionsPersistKey [] [Asc ArtistaNome] artistaNome
    generos  = optionsPersistKey [] [Asc GeneroNome] generoNome
    -- Campo de texto convertido para Double (aceita virgula)
    campoPreco = checkMMap (pure . lerPreco) mostrarPreco textField
    mostrarPreco :: Double -> Text
    mostrarPreco v = T.replace "." "," (T.pack (printf "%.2f" v))

-- Mensagem para quando ja existe o mesmo titulo, artista e formato.
avisoDuplicado :: Handler ()
avisoDuplicado = setMessage
    "Já existe um álbum com esse título, artista e formato. Altere um deles ou edite o álbum existente."

-- Sem artista ou genero cadastrado nao da para cadastrar album.
-- Devolve uma mensagem explicando o que falta, se for o caso.
faltaCadastroBase :: Handler (Maybe Widget)
faltaCadastroBase = do
    nArtistas <- runDB $ count ([] :: [Filter Artista])
    nGeneros  <- runDB $ count ([] :: [Filter Genero])
    pure $ if nArtistas > 0 && nGeneros > 0
        then Nothing
        else Just [whamlet|
            <h1>Novo álbum
            <p .vazio>
                Para cadastrar um álbum é preciso ter pelo menos um artista e um gênero.
                <br>
                <a href=@{ArtistaNovoR}>Cadastrar artista
                \ · #
                <a href=@{GeneroNovoR}>Cadastrar gênero
        |]

-- Filtros da lista de albuns, lidos dos parametros da URL:
--   /albuns?q=dark&genero=2&formato=Vinil&estoque=baixo&ordem=preco-asc
data FiltroAlbuns = FiltroAlbuns
    { flTexto   :: Text            -- parte do titulo ou do nome do artista
    , flGenero  :: Maybe GeneroId
    , flFormato :: Maybe Formato
    , flEstoque :: Text            -- "", "disponivel", "baixo" ou "zerado"
    , flOrdem   :: Text            -- "titulo", "preco-asc", "preco-desc" ou "estoque"
    }

-- Le os filtros da URL. Valores invalidos sao ignorados (viram "sem filtro").
lerFiltro :: Handler FiltroAlbuns
lerFiltro = do
    texto   <- maybe "" T.strip <$> lookupGetParam "q"
    genero  <- (>>= fromPathPiece) <$> lookupGetParam "genero"
    formato <- (>>= readMaybe . T.unpack) <$> lookupGetParam "formato"
    estoque <- fromMaybe "" <$> lookupGetParam "estoque"
    ordem   <- fromMaybe "titulo" <$> lookupGetParam "ordem"
    pure (FiltroAlbuns texto genero formato estoque ordem)

-- Algum filtro esta ativo? (para mostrar o link "Limpar filtros")
filtroAtivo :: FiltroAlbuns -> Bool
filtroAtivo f = not (T.null (flTexto f)) || flGenero f /= Nothing
    || flFormato f /= Nothing || not (T.null (flEstoque f))

-- Filtro "contem o texto", que vira LIKE '%texto%' no SQL. O Persistent nao
-- tem operador pronto para LIKE, entao o filtro e montado a mao com
-- BackendSpecificFilter. No SQLite o LIKE ignora maiusculas e minusculas
-- (nas letras sem acento). % e _ sao curingas do LIKE, por isso sao removidos.
contem :: EntityField registro Text -> Text -> Filter registro
contem campo texto = Filter campo (FilterValue padrao) (BackendSpecificFilter "LIKE")
  where
    padrao = "%" <> T.filter (`notElem` ("%_" :: String)) texto <> "%"

-- Monta os filtros do Persistent. Filtros na mesma lista sao combinados
-- com AND; o operador ||. combina duas listas com OR.
--   artistasEncontrados: artistas cujo nome contem o texto buscado
filtrosAlbum :: FiltroAlbuns -> [ArtistaId] -> [Filter Album]
filtrosAlbum f artistasEncontrados = porTexto ++ porGenero ++ porFormato ++ porEstoque
  where
    porTexto
        | T.null (flTexto f) = []
        | otherwise = [AlbumTitulo `contem` flTexto f] ||. [AlbumArtistaId <-. artistasEncontrados]
    porGenero  = maybe [] (\g -> [AlbumGeneroId ==. g]) (flGenero f)
    porFormato = maybe [] (\x -> [AlbumFormato ==. x]) (flFormato f)
    porEstoque = case flEstoque f of
        "disponivel" -> [AlbumEstoque >. 0]
        "baixo"      -> [AlbumEstoque >. 0, AlbumEstoque <. estoqueMinimo]
        "zerado"     -> [AlbumEstoque <=. 0]
        _            -> []

ordenacao :: Text -> [SelectOpt Album]
ordenacao "preco-asc"  = [Asc AlbumPreco, Asc AlbumTitulo]
ordenacao "preco-desc" = [Desc AlbumPreco, Asc AlbumTitulo]
ordenacao "estoque"    = [Asc AlbumEstoque, Asc AlbumTitulo]
ordenacao _            = [Asc AlbumTitulo]

-- GET /albuns : tabela de albuns, com busca, filtros e ordenacao.
-- Os nomes de artista e genero sao buscados uma vez so e guardados
-- em um Map (id -> nome), em vez de uma consulta por linha.
getAlbunsR :: Handler Html
getAlbunsR = do
    f <- lerFiltro
    artistasEncontrados <- if T.null (flTexto f)
        then pure []
        else runDB $ selectKeysList [ArtistaNome `contem` flTexto f] []
    albuns   <- runDB $ selectList (filtrosAlbum f artistasEncontrados) (ordenacao (flOrdem f))
    total    <- runDB $ count ([] :: [Filter Album])
    artistas <- runDB $ selectList [] []
    generos  <- runDB $ selectList [] [Asc GeneroNome]
    let nomeArtista = Map.fromList [ (k, artistaNome a) | Entity k a <- artistas ]
        nomeGenero  = Map.fromList [ (k, generoNome g)  | Entity k g <- generos ]
        buscar mapa k = Map.findWithDefault "-" k mapa
        -- Opcoes dos <select> do filtro: (valor, texto, selecionado?)
        opcoesGenero  = [ (toPathPiece k, generoNome g, Just k == flGenero f) | Entity k g <- generos ]
        opcoesFiltroFormato = [ (T.pack (show x), nomeFormato x, Just x == flFormato f) | x <- [minBound .. maxBound] ]
        opcoesEstoque :: [(Text, Text, Bool)]
        opcoesEstoque = [ (v, rotulo, v == flEstoque f)
                        | (v, rotulo) <- [ ("disponivel", "Com estoque")
                                         , ("baixo", "Estoque baixo (menos de " <> T.pack (show estoqueMinimo) <> ")")
                                         , ("zerado", "Sem estoque") ] ]
        opcoesOrdem :: [(Text, Text, Bool)]
        opcoesOrdem   = [ (v, rotulo, v == flOrdem f)
                        | (v, rotulo) <- [ ("titulo", "Ordenar por título")
                                         , ("preco-asc", "Menor preço")
                                         , ("preco-desc", "Maior preço")
                                         , ("estoque", "Menor estoque") ] ]
    defaultLayout $ do
        setTitle "Álbuns"
        [whamlet|
            <div .cabecalho-pagina>
                <h1>Álbuns
                <div>
                    <a .botao href=@{BuscaMusicBrainzR}>Importar do MusicBrainz
                    <a .botao.secundario href=@{AlbumNovoR}>Cadastro manual

            <form .filtros method=get action=@{AlbunsR}>
                <input type=search name=q value=#{flTexto f} placeholder="Título ou artista">
                <select name=genero>
                    <option value="">Todos os gêneros
                    $forall (valor, texto, selecionado) <- opcoesGenero
                        <option value=#{valor} :selecionado:selected>#{texto}
                <select name=formato>
                    <option value="">Todos os formatos
                    $forall (valor, texto, selecionado) <- opcoesFiltroFormato
                        <option value=#{valor} :selecionado:selected>#{texto}
                <select name=estoque>
                    <option value="">Qualquer estoque
                    $forall (valor, texto, selecionado) <- opcoesEstoque
                        <option value=#{valor} :selecionado:selected>#{texto}
                <select name=ordem>
                    $forall (valor, texto, selecionado) <- opcoesOrdem
                        <option value=#{valor} :selecionado:selected>#{texto}
                <input type=submit value="Filtrar">
                $if filtroAtivo f
                    <a href=@{AlbunsR}>Limpar filtros

            $if total == 0
                <p .vazio>Nenhum álbum cadastrado ainda.
            $elseif null albuns
                <p .vazio>Nenhum álbum encontrado com esses filtros.
            $else
                <p .resultado>#{length albuns} de #{total} álbum(ns)
                <table>
                    <thead>
                        <tr>
                            <th>Capa
                            <th>Título
                            <th>Artista
                            <th>Gênero
                            <th>Formato
                            <th>Preço
                            <th>Estoque
                            <th>Ações
                    <tbody>
                        $forall Entity albumId album <- albuns
                            <tr>
                                <td>
                                    $maybe url <- albumCapaUrl album
                                        <img .capa-mini src=#{url} alt="">
                                    $nothing
                                        <div .capa-mini.sem-capa>♪
                                <td>
                                    <a href=@{AlbumR albumId}>#{albumTitulo album}
                                <td>#{buscar nomeArtista (albumArtistaId album)}
                                <td>#{buscar nomeGenero (albumGeneroId album)}
                                <td>#{nomeFormato (albumFormato album)}
                                <td .nowrap>#{formatarPreco (albumPreco album)}
                                <td>
                                    $with (classe, _) <- situacao (albumEstoque album)
                                        <span .tag class=#{classe}>#{albumEstoque album}
                                <td .acoes>
                                    <a .botao.secundario href=@{AlbumEditarR albumId}>Editar
                                    ^{botaoExcluir (AlbumExcluirR albumId) (albumTitulo album)}
        |]

-- GET /albuns/novo
getAlbumNovoR :: Handler Html
getAlbumNovoR = do
    falta <- faltaCadastroBase
    case falta of
        Just aviso -> defaultLayout aviso
        Nothing -> do
            ano <- obterAnoAtual
            (campos, enctype) <- generateFormPost (albumForm ano Nothing)
            defaultLayout $ paginaFormulario "Novo álbum" AlbumNovoR AlbunsR campos enctype

-- POST /albuns/novo : insertUnique devolve Nothing se violar UniqueAlbum.
postAlbumNovoR :: Handler Html
postAlbumNovoR = do
    ano <- obterAnoAtual
    ((resultado, campos), enctype) <- runFormPost (albumForm ano Nothing)
    case resultado of
        FormSuccess album -> do
            inserido <- runDB $ insertUnique album
            case inserido of
                Just albumId -> do
                    setMessage $ toHtml ("Álbum \"" <> albumTitulo album <> "\" cadastrado. Agora cadastre as faixas.")
                    redirect (AlbumR albumId)
                Nothing -> do
                    avisoDuplicado
                    defaultLayout $ paginaFormulario "Novo álbum" AlbumNovoR AlbunsR campos enctype
        _ -> defaultLayout $ paginaFormulario "Novo álbum" AlbumNovoR AlbunsR campos enctype

-- GET /albuns/#AlbumId : detalhes, faixas e formulario de nova faixa.
getAlbumR :: AlbumId -> Handler Html
getAlbumR albumId = do
    numero <- proximoNumero albumId
    (campos, enctype) <- generateFormPost (faixaForm albumId (Just numero) Nothing)
    paginaAlbum albumId campos enctype

-- Monta a pagina de detalhes. Fica separada porque o POST de nova
-- faixa com erro mostra esta mesma pagina, com os erros no formulario.
paginaAlbum :: AlbumId -> Widget -> Enctype -> Handler Html
paginaAlbum albumId camposFaixa enctype = do
    fuso <- liftIO getCurrentTimeZone
    (album, artista, genero, faixas, movimentos, fornecedores) <- runDB $ do
        album   <- get404 albumId
        artista <- get404 (albumArtistaId album)
        genero  <- get404 (albumGeneroId album)
        faixas  <- selectList [FaixaAlbumId ==. albumId] [Asc FaixaNumero]
        movimentos <- selectList [MovimentacaoEstoqueAlbumId ==. albumId]
                                 [Desc MovimentacaoEstoqueDataHora, Desc MovimentacaoEstoqueId]
        fornecedores <- selectList [] []
        pure (album, artista, genero, faixas, movimentos, fornecedores)
    let duracaoTotal = sum [ d | Entity _ f <- faixas, Just d <- [faixaDuracao f] ]
        nomeForn = Map.fromList [ (k, fornecedorNome f) | Entity k f <- fornecedores ]
        fornDe = maybe "-" (\k -> Map.findWithDefault "-" k nomeForn)
    defaultLayout $ do
        setTitle (toHtml (albumTitulo album))
        [whamlet|
            <div .detalhe-album>
                $maybe url <- albumCapaUrl album
                    <img .capa src=#{url} alt="Capa de #{albumTitulo album}">
                $nothing
                    <div .capa.sem-capa>♪
                <div>
                    <h1>#{albumTitulo album}
                    <p .subtitulo>#{artistaNome artista}
                    <dl>
                        <dt>Gênero
                        <dd>#{generoNome genero}
                        <dt>Ano
                        <dd>#{maybe "-" show (albumAno album)}
                        <dt>Formato
                        <dd>#{nomeFormato (albumFormato album)}
                        <dt>Preço
                        <dd>#{formatarPreco (albumPreco album)}
                        <dt>Em estoque
                        <dd>#{albumEstoque album} unidade(s)
                    <a .botao.secundario href=@{AlbumEditarR albumId}>Editar álbum
                    <a .botao.secundario href=@{AlbunsR}>Voltar à lista

            <h2>Faixas
            $if null faixas
                <p .vazio>Nenhuma faixa cadastrada.
            $else
                <table>
                    <thead>
                        <tr>
                            <th>Nº
                            <th>Título
                            <th>Duração
                            <th>Ações
                    <tbody>
                        $forall Entity faixaId faixa <- faixas
                            <tr>
                                <td>#{faixaNumero faixa}
                                <td>#{faixaTitulo faixa}
                                <td>#{maybe "-" formatarDuracao (faixaDuracao faixa)}
                                <td .acoes>
                                    <a .botao.secundario href=@{FaixaEditarR faixaId}>Editar
                                    ^{botaoExcluir (FaixaExcluirR faixaId) (faixaTitulo faixa)}
                    <tfoot>
                        <tr>
                            <td colspan=2>#{length faixas} faixa(s)
                            <td colspan=2>Total: #{formatarDuracao duracaoTotal}

            <h2>Adicionar faixa
            <form .formulario method=post action=@{FaixaNovaR albumId} enctype=#{enctype}>
                ^{camposFaixa}
                <input type=submit value="Adicionar faixa">

            <div .cabecalho-pagina>
                <h2>Movimentações de estoque
                <a .botao href=@?{(EntradaEstoqueR, [("album", toPathPiece albumId)])}>Registrar entrada
            $if null movimentos
                <p .vazio>Nenhuma entrada ou saída registrada para este álbum.
            $else
                <table>
                    <thead>
                        <tr>
                            <th>Data
                            <th>Tipo
                            <th>Qtd.
                            <th>Fornecedor
                            <th>Custo unit.
                            <th>Observação
                    <tbody>
                        $forall Entity _ m <- movimentos
                            <tr>
                                <td .nowrap>#{formatarDataHora fuso (movimentacaoEstoqueDataHora m)}
                                <td>#{nomeTipo (movimentacaoEstoqueTipo m)}
                                <td>#{movimentacaoEstoqueQuantidade m}
                                <td>#{fornDe (movimentacaoEstoqueFornecedorId m)}
                                <td .nowrap>#{maybe "-" formatarPreco (movimentacaoEstoqueCustoUnitario m)}
                                <td>#{maybe "" id (movimentacaoEstoqueObservacao m)}
        |]

-- POST /albuns/#AlbumId/faixas : cadastra uma faixa.
-- Se o formulario tiver erro, mostra de novo a pagina do album
-- com as mensagens ao lado dos campos.
postFaixaNovaR :: AlbumId -> Handler Html
postFaixaNovaR albumId = do
    _ <- runDB $ get404 albumId
    ((resultado, campos), enctype) <- runFormPost (faixaForm albumId Nothing Nothing)
    case resultado of
        FormSuccess faixa -> do
            inserida <- runDB $ insertUnique faixa
            case inserida of
                Just _  -> setMessage "Faixa adicionada."
                Nothing -> avisoNumeroRepetido (faixaNumero faixa)
            redirect (AlbumR albumId)
        _ -> paginaAlbum albumId campos enctype

-- GET /albuns/#AlbumId/editar
getAlbumEditarR :: AlbumId -> Handler Html
getAlbumEditarR albumId = do
    album <- runDB $ get404 albumId
    ano <- obterAnoAtual
    (campos, enctype) <- generateFormPost (albumForm ano (Just album))
    defaultLayout $
        paginaFormulario "Editar álbum" (AlbumEditarR albumId) (AlbumR albumId) campos enctype

-- POST /albuns/#AlbumId/editar : replaceUnique devolve Nothing quando
-- deu certo, ou Just com a restricao que seria violada.
postAlbumEditarR :: AlbumId -> Handler Html
postAlbumEditarR albumId = do
    album <- runDB $ get404 albumId
    ano <- obterAnoAtual
    ((resultado, campos), enctype) <- runFormPost (albumForm ano (Just album))
    let formulario = defaultLayout $
            paginaFormulario "Editar álbum" (AlbumEditarR albumId) (AlbumR albumId) campos enctype
    case resultado of
        FormSuccess novo -> do
            conflito <- runDB $ replaceUnique albumId novo
            case conflito of
                Nothing -> do
                    setMessage $ toHtml ("Álbum \"" <> albumTitulo novo <> "\" atualizado.")
                    redirect (AlbumR albumId)
                Just _ -> avisoDuplicado >> formulario
        _ -> formulario

-- POST /albuns/#AlbumId/excluir : apaga as faixas e depois o album.
-- As duas operacoes estao no mesmo runDB, ou seja, na mesma transacao.
-- Album com movimentacao de estoque nao pode ser excluido, para nao
-- perder o historico de entradas e saidas.
postAlbumExcluirR :: AlbumId -> Handler Html
postAlbumExcluirR albumId = do
    album <- runDB $ get404 albumId
    movimentos <- runDB $ count [MovimentacaoEstoqueAlbumId ==. albumId]
    if movimentos > 0
        then setMessage $ toHtml ("Não é possível excluir \"" <> albumTitulo album
                <> "\": ele já tem movimentações de estoque registradas.")
        else do
            runDB $ do
                deleteWhere [FaixaAlbumId ==. albumId]
                delete albumId
            setMessage $ toHtml ("Álbum \"" <> albumTitulo album <> "\" excluído, junto com suas faixas.")
    redirect AlbunsR
