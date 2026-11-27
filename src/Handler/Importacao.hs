{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Busca de albuns no MusicBrainz e importacao para o catalogo.
--
-- Fluxo:
--   1. /albuns/buscar          : o usuario pesquisa titulo (e artista);
--                                a lista mostra os lancamentos encontrados.
--   2. /albuns/importar/<id>   : mostra os dados do lancamento (faixas, capa)
--                                e um formulario ja preenchido; o usuario
--                                completa genero, formato e preco.
--   3. POST no mesmo endereco  : cria (ou reaproveita) o artista, grava o
--                                album e todas as faixas.
module Handler.Importacao where

import Foundation
import Yesod
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import Api.MusicBrainz
import Model.Formato
import Util.Validacao
import Util.Formatacao

-- O que o usuario confirma ou completa no formulario de importacao.
data DadosImportacao = DadosImportacao
    { diTitulo  :: Text
    , diGenero  :: GeneroId
    , diAno     :: Maybe Int
    , diFormato :: Formato
    , diPreco   :: Double
    , diCapa    :: Maybe Text
    }

-- Formulario ja preenchido com os dados que vieram da API.
--   generoSugerido : genero do catalogo com o mesmo nome de um genero do MusicBrainz
importacaoForm :: Integer -> Lancamento -> Maybe Text -> Maybe GeneroId -> Form DadosImportacao
importacaoForm anoAtual lanc capa generoSugerido = renderDivs $ DadosImportacao
    <$> areq (check (tituloObrigatorio 150) textField) "Título" (Just (lnTitulo lanc))
    <*> areq (selectField generos) "Gênero" generoSugerido
    <*> aopt (check (anoValido anoAtual) intField) "Ano de lançamento" (Just (lnAno lanc))
    <*> areq (selectFieldList opcoesFormato) "Formato" (lnFormato lanc >>= formatoDoMusicBrainz)
    <*> areq campoPreco "Preço de venda (R$)" Nothing
    <*> (textoOpcional <$> aopt urlField "Endereço da imagem da capa" (Just capa))
  where
    generos = optionsPersistKey [] [Asc GeneroNome] generoNome
    campoPreco = checkMMap (pure . lerPreco) mostrarPreco textField
    mostrarPreco :: Double -> Text
    mostrarPreco v = T.replace "." "," (T.pack (printf "%.2f" v))

-- Procura no catalogo um genero com o mesmo nome (sem diferenciar
-- maiusculas) de algum genero que o MusicBrainz informou.
sugerirGenero :: [Text] -> Handler (Maybe GeneroId)
sugerirGenero nomesApi = do
    generos <- runDB $ selectList [] [Asc GeneroNome]
    let apiMinusculo = map T.toLower nomesApi
    pure $ listToMaybe
        [ k | Entity k g <- generos, T.toLower (generoNome g) `elem` apiMinusculo ]

------------------------------------------------------------------------
-- 1. Busca
------------------------------------------------------------------------

-- GET /albuns/buscar?titulo=...&artista=...
getBuscaMusicBrainzR :: Handler Html
getBuscaMusicBrainzR = do
    (mTitulo, mArtista) <- runInputGet $ (,)
        <$> iopt textField "titulo"
        <*> iopt textField "artista"
    let titulo  = maybe "" T.strip mTitulo
        artista = maybe "" T.strip mArtista
    -- So chama a API quando o usuario digitou um titulo.
    resultado <- if T.null titulo
        then pure Nothing
        else Just <$> liftIO (buscarLancamentos titulo artista)
    defaultLayout $ do
        setTitle "Importar do MusicBrainz"
        [whamlet|
            <h1>Importar álbum do MusicBrainz
            <p>
                Pesquise um álbum na base pública do
                <a href="https://musicbrainz.org" target="_blank" rel="noopener">MusicBrainz</a>.
                Ao importar, o artista, o ano, a capa e as faixas são preenchidos automaticamente.
            <form .busca method=get action=@{BuscaMusicBrainzR}>
                <input type=text name=titulo value=#{titulo} placeholder="Título do álbum" required>
                <input type=text name=artista value=#{artista} placeholder="Artista (opcional)">
                <input type=submit value="Buscar">
            $maybe r <- resultado
                $case r
                    $of Left erro
                        <p .erro>#{erro}
                    $of Right []
                        <p .vazio>Nenhum lançamento encontrado. Confira a grafia ou tire o artista da busca.
                    $of Right itens
                        <table>
                            <thead>
                                <tr>
                                    <th>Capa
                                    <th>Título
                                    <th>Artista
                                    <th>Ano
                                    <th>País
                                    <th>Formato
                                    <th>Faixas
                                    <th>
                            <tbody>
                                $forall item <- itens
                                    <tr>
                                        <td>
                                            <img .capa-mini src=#{urlMiniaturaCapa (rbId item)} alt="" loading=lazy onerror="this.style.visibility='hidden'">
                                        <td>#{rbTitulo item}
                                        <td>#{rbArtista item}
                                        <td>#{maybe "-" show (rbAno item)}
                                        <td>#{fromMaybe "-" (rbPais item)}
                                        <td>#{fromMaybe "-" (rbFormato item)}
                                        <td>#{rbFaixas item}
                                        <td>
                                            <a .botao href=@{ImportarR (rbId item)}>Importar
        |]

------------------------------------------------------------------------
-- 2. Conferir e completar os dados
------------------------------------------------------------------------

-- Busca o lancamento na API. Se der erro, volta para a busca com aviso.
carregarLancamento :: Text -> Handler Lancamento
carregarLancamento mbid = do
    if mbidValido mbid then pure () else notFound
    resposta <- liftIO (buscarLancamento mbid)
    case resposta of
        Right lanc -> pure lanc
        Left erro -> do
            setMessage (toHtml erro)
            redirect BuscaMusicBrainzR

-- GET /albuns/importar/<mbid>
getImportarR :: Text -> Handler Html
getImportarR mbid = do
    exigirGenero
    lanc  <- carregarLancamento mbid
    capa  <- liftIO (buscarCapa mbid)
    gen   <- sugerirGenero (lnGeneros lanc)
    ano   <- obterAnoAtual
    (campos, enctype) <- generateFormPost (importacaoForm ano lanc capa gen)
    paginaImportacao mbid lanc capa campos enctype

-- A importacao precisa de pelo menos um genero no catalogo.
exigirGenero :: Handler ()
exigirGenero = do
    n <- runDB $ count ([] :: [Filter Genero])
    if n > 0 then pure () else do
        setMessage "Cadastre pelo menos um gênero antes de importar álbuns."
        redirect GeneroNovoR

paginaImportacao :: Text -> Lancamento -> Maybe Text -> Widget -> Enctype -> Handler Html
paginaImportacao mbid lanc capa campos enctype = do
    let duracaoTotal = sum [ d | f <- lnFaixas lanc, Just d <- [faDuracao f] ]
    defaultLayout $ do
        setTitle "Importar álbum"
        [whamlet|
            <h1>Importar álbum
            <div .detalhe-album>
                $maybe url <- capa
                    <img .capa src=#{url} alt="Capa">
                $nothing
                    <div .capa.sem-capa>♪
                <div>
                    <h2>#{lnTitulo lanc}
                    <p .subtitulo>#{lnArtistaNome lanc}
                    <dl>
                        <dt>Ano
                        <dd>#{maybe "-" show (lnAno lanc)}
                        <dt>Formato original
                        <dd>#{fromMaybe "-" (lnFormato lanc)}
                        <dt>Gêneros no MusicBrainz
                        <dd>
                            $if null (lnGeneros lanc)
                                \-
                            $else
                                #{T.intercalate ", " (lnGeneros lanc)}
                        <dt>Faixas
                        <dd>#{length (lnFaixas lanc)} (#{formatarDuracao duracaoTotal})
                    <p .fonte>
                        <a href="https://musicbrainz.org/release/#{mbid}" target="_blank" rel="noopener">Ver no MusicBrainz

            <div .colunas>
                <div>
                    <h2>Confira e complete
                    <p>Gênero, formato e preço não vêm da API: escolha os valores da loja.
                    <form .formulario method=post action=@{ImportarR mbid} enctype=#{enctype}>
                        ^{campos}
                        <input type=submit value="Importar para o catálogo">
                        <a .botao.secundario href=@{BuscaMusicBrainzR}>Voltar à busca
                <div>
                    <h2>Faixas que serão importadas
                    <ol .lista-faixas>
                        $forall faixa <- lnFaixas lanc
                            <li>
                                #{faTitulo faixa}
                                <span .duracao>#{maybe "" formatarDuracao (faDuracao faixa)}
        |]

------------------------------------------------------------------------
-- 3. Gravar no banco
------------------------------------------------------------------------

-- POST /albuns/importar/<mbid>
-- Os dados do lancamento sao buscados de novo na API, em vez de confiar
-- em campos escondidos no formulario.
postImportarR :: Text -> Handler Html
postImportarR mbid = do
    exigirGenero
    lanc <- carregarLancamento mbid
    ano  <- obterAnoAtual
    ((resultado, campos), enctype) <- runFormPost (importacaoForm ano lanc Nothing Nothing)
    case resultado of
        FormSuccess dados -> do
            artistaId <- obterArtista lanc
            let album = Album
                    { albumTitulo    = diTitulo dados
                    , albumArtistaId = artistaId
                    , albumGeneroId  = diGenero dados
                    , albumAno       = diAno dados
                    , albumFormato   = diFormato dados
                    , albumPreco     = diPreco dados
                    , albumEstoque   = 0
                    , albumCapaUrl   = diCapa dados
                    }
            -- Album e faixas no mesmo runDB (mesma transacao):
            -- ou grava tudo, ou nada.
            inserido <- runDB $ do
                mAlbumId <- insertUnique album
                case mAlbumId of
                    Nothing -> pure Nothing
                    Just albumId -> do
                        insertMany_
                            [ Faixa albumId (faNumero f) (T.take 150 (faTitulo f)) (faDuracao f)
                            | f <- lnFaixas lanc ]
                        pure (Just albumId)
            case inserido of
                Just albumId -> do
                    setMessage $ toHtml ("Álbum \"" <> diTitulo dados <> "\" importado com "
                        <> T.pack (show (length (lnFaixas lanc))) <> " faixa(s).")
                    redirect (AlbumR albumId)
                Nothing -> do
                    setMessage "Esse álbum já está no catálogo nesse formato. Escolha outro formato ou edite o existente."
                    paginaImportacao mbid lanc (diCapa dados) campos enctype
        _ -> do
            capa <- liftIO (buscarCapa mbid)
            paginaImportacao mbid lanc capa campos enctype

-- Usa o artista principal (o primeiro creditado). Reaproveita o
-- cadastro se ja existir um artista com o mesmo nome. Se nao existir,
-- consulta a API do artista para preencher pais e ano de inicio, e cadastra.
obterArtista :: Lancamento -> Handler ArtistaId
obterArtista lanc = do
    let nome = T.take 100 (lnArtistaPrincipal lanc)
    existente <- runDB $ getBy (UniqueArtistaNome nome)
    case existente of
        Just (Entity artistaId _) -> pure artistaId
        Nothing -> do
            info <- case lnArtistaId lanc of
                Just mbidArtista -> do
                    -- O MusicBrainz aceita 1 requisicao por segundo; a busca
                    -- do lancamento acabou de ser feita.
                    liftIO (threadDelay 1100000)
                    either (const Nothing) Just <$> liftIO (buscarArtista mbidArtista)
                Nothing -> pure Nothing
            runDB $ insert Artista
                { artistaNome      = nome
                , artistaPais      = info >>= aaPais
                , artistaAnoInicio = info >>= aaInicio
                }
