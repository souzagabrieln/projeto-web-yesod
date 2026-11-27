{-# LANGUAGE OverloadedStrings #-}

-- Cliente das APIs externas usadas pela loja:
--
--   * MusicBrainz (https://musicbrainz.org/doc/MusicBrainz_API):
--     busca de lancamentos (releases), dados do album com as faixas
--     e dados do artista.
--   * Cover Art Archive (https://coverartarchive.org): capa do album.
--
-- As duas sao gratuitas e nao pedem chave, mas o MusicBrainz exige um
-- User-Agent que identifique a aplicacao e aceita no maximo
-- 1 requisicao por segundo.
module Api.MusicBrainz
    ( ResultadoBusca (..)
    , Lancamento (..)
    , FaixaApi (..)
    , ArtistaApi (..)
    , buscarLancamentos
    , buscarLancamento
    , buscarArtista
    , buscarCapa
    , urlMiniaturaCapa
    , formatoDoMusicBrainz
    , mbidValido
    ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad (forM)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (isHexDigit)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Simple
import Text.Read (readMaybe)
import Model.Formato (Formato (..))

------------------------------------------------------------------------
-- Tipos com os dados que a aplicacao usa
------------------------------------------------------------------------

-- Um item da lista de resultados da busca.
data ResultadoBusca = ResultadoBusca
    { rbId      :: Text         -- MBID do lancamento
    , rbTitulo  :: Text
    , rbArtista :: Text
    , rbAno     :: Maybe Int
    , rbPais    :: Maybe Text
    , rbFormato :: Maybe Text   -- formato como vem da API ("CD", "12\" Vinyl"...)
    , rbFaixas  :: Int
    } deriving Show

-- Dados completos de um lancamento, usados na importacao.
data Lancamento = Lancamento
    { lnId          :: Text
    , lnTitulo      :: Text
    , lnAno         :: Maybe Int
    , lnArtistaId   :: Maybe Text   -- MBID do artista principal
    , lnArtistaPrincipal :: Text    -- primeiro artista creditado
    , lnArtistaNome :: Text         -- creditos completos ("A & B")
    , lnFormato     :: Maybe Text
    , lnFaixas      :: [FaixaApi]
    , lnGeneros     :: [Text]
    } deriving Show

data FaixaApi = FaixaApi
    { faNumero  :: Int
    , faTitulo  :: Text
    , faDuracao :: Maybe Int    -- em segundos
    } deriving Show

data ArtistaApi = ArtistaApi
    { aaPais   :: Maybe Text
    , aaInicio :: Maybe Int
    } deriving Show

------------------------------------------------------------------------
-- Leitura do JSON (FromJSON)
------------------------------------------------------------------------

-- Resposta da busca: {"releases": [ {...}, {...} ]}
newtype RespostaBusca = RespostaBusca [ResultadoBusca]

instance FromJSON RespostaBusca where
    parseJSON = withObject "RespostaBusca" $ \o ->
        RespostaBusca <$> o .:? "releases" .!= []

instance FromJSON ResultadoBusca where
    parseJSON = withObject "ResultadoBusca" $ \o -> do
        mbid    <- o .: "id"
        titulo  <- o .: "title"
        artista <- nomeDosCreditos o
        data_   <- o .:? "date"
        pais    <- o .:? "country"
        midias  <- o .:? "media" .!= []
        formatos <- forM midias (\m -> withObject "media" (.:? "format") m)
        faixas  <- o .:? "track-count" .!= 0
        pure ResultadoBusca
            { rbId      = mbid
            , rbTitulo  = titulo
            , rbArtista = artista
            , rbAno     = data_ >>= anoDaData
            , rbPais    = pais
            , rbFormato = listToMaybe [ f | Just f <- formatos ]
            , rbFaixas  = faixas
            }

-- Resposta de /release/{id}?inc=recordings+artist-credits+genres
instance FromJSON Lancamento where
    parseJSON = withObject "Lancamento" $ \o -> do
        mbid     <- o .: "id"
        titulo   <- o .: "title"
        data_    <- o .:? "date"
        artista  <- nomeDosCreditos o
        creditos <- o .:? "artist-credit" .!= []
        (artId, principal) <- case creditos of
            (c : _) -> withObject "credito" (\c' -> do
                           nome <- c' .: "name"
                           a <- c' .:? "artist"
                           mbidArt <- maybe (pure Nothing) (withObject "artist" (.:? "id")) a
                           pure (mbidArt, nome)) c
            []      -> pure (Nothing, artista)
        midias   <- o .:? "media" .!= []
        (formatos, listas) <- unzip <$> forM midias lerMidia
        generos  <- o .:? "genres" .!= []
        nomesGen <- forM generos (withObject "genre" (.: "name"))
        -- Albuns com mais de um disco: as faixas sao numeradas em sequencia.
        let faixas = zipWith numerar [1 ..] (concat listas)
            numerar n (t, d) = FaixaApi n t d
        pure Lancamento
            { lnId          = mbid
            , lnTitulo      = titulo
            , lnAno         = data_ >>= anoDaData
            , lnArtistaId   = artId
            , lnArtistaPrincipal = principal
            , lnArtistaNome = artista
            , lnFormato     = listToMaybe [ f | Just f <- formatos ]
            , lnFaixas      = faixas
            , lnGeneros     = nomesGen
            }
      where
        lerMidia :: Value -> Parser (Maybe Text, [(Text, Maybe Int)])
        lerMidia = withObject "media" $ \m -> do
            formato <- m .:? "format"
            tracks  <- m .:? "tracks" .!= []
            faixas  <- forM tracks $ withObject "track" $ \t -> do
                nome    <- t .: "title"
                duracao <- t .:? "length"          -- milissegundos ou null
                pure (nome, fmap (`div` 1000) duracao)
            pure (formato, faixas)

-- Resposta de /artist/{id}: pais e ano de inicio ("life-span").
instance FromJSON ArtistaApi where
    parseJSON = withObject "ArtistaApi" $ \o -> do
        pais   <- o .:? "country"
        vida   <- o .:? "life-span"
        inicio <- case vida of
            Just v  -> withObject "life-span" (.:? "begin") v
            Nothing -> pure Nothing
        pure (ArtistaApi pais (inicio >>= anoDaData))

-- Resposta do Cover Art Archive: {"images": [{"front": true, "thumbnails": {...}}]}
newtype RespostaCapa = RespostaCapa (Maybe Text)

instance FromJSON RespostaCapa where
    parseJSON = withObject "RespostaCapa" $ \o -> do
        imagens <- o .:? "images" .!= []
        urls <- forM imagens $ withObject "image" $ \i -> do
            frente <- i .:? "front" .!= False
            miniaturas <- i .:? "thumbnails" .!= mempty
            grande  <- miniaturas .:? "500"
            original <- i .:? "image"
            -- miniatura de 500px; se nao houver, a imagem original
            pure (if frente then grande <|> original else Nothing)
        pure (RespostaCapa (listToMaybe [ u | Just u <- urls ]))

-- "artist-credit" e uma lista: [{"name": "A", "joinphrase": " & "}, {"name": "B"}]
-- O nome exibido e a juncao de todos ("A & B").
nomeDosCreditos :: Object -> Parser Text
nomeDosCreditos o = do
    creditos <- o .:? "artist-credit" .!= []
    partes <- forM creditos $ withObject "credito" $ \c -> do
        nome  <- c .: "name"
        juncao <- c .:? "joinphrase" .!= ""
        pure (nome <> juncao)
    pure (if null partes then "Artista desconhecido" else T.concat partes)

-- "1973-03-24", "1973-03" ou "1973" -> Just 1973
anoDaData :: Text -> Maybe Int
anoDaData = readMaybe . T.unpack . T.take 4

------------------------------------------------------------------------
-- Requisicoes HTTP
------------------------------------------------------------------------

urlMusicBrainz :: String
urlMusicBrainz = "https://musicbrainz.org/ws/2"

urlCoverArt :: String
urlCoverArt = "https://coverartarchive.org"

-- Identificacao exigida pelo MusicBrainz.
userAgent :: Text
userAgent = "LojaDeAlbuns/1.0 ( https://github.com/souzagabrieln/projeto-web-yesod )"

-- Faz um GET e decodifica o JSON. Qualquer falha (sem internet,
-- tempo esgotado, status diferente de 200, JSON inesperado) vira
-- Left com uma mensagem para mostrar ao usuario.
--
-- Quando o MusicBrainz responde 503 (limite de 1 requisicao por segundo
-- ou servidor sobrecarregado), espera um pouco e tenta de novo:
-- ate 3 tentativas, esperando 1,5 s e depois 3 s.
requisitarJSON :: FromJSON a => String -> Query -> IO (Either Text a)
requisitarJSON url parametros = do
    base <- parseRequest url
    let req = setRequestHeader "User-Agent" [TE.encodeUtf8 userAgent]
            $ setRequestHeader "Accept" ["application/json"]
            $ setRequestQueryString parametros base
    tentar req 1
  where
    maxTentativas = 3 :: Int

    tentar req tentativa = do
        resposta <- try (httpLBS req)
        case resposta of
            Left erro -> pure (Left (mensagemDeErro erro))
            Right r -> case getResponseStatusCode r of
                200 -> pure $ case eitherDecode (getResponseBody r) of
                    Right valor -> Right valor
                    Left _      -> Left "A resposta da API veio num formato inesperado."
                404 -> pure (Left "Não encontrado na API.")
                503 | tentativa < maxTentativas -> do
                        threadDelay (tentativa * 1500000)   -- 1,5 s, depois 3 s
                        tentar req (tentativa + 1)
                    | otherwise ->
                        pure (Left "A API está ocupada (limite de requisições). Tente de novo em alguns segundos.")
                s   -> pure (Left ("A API respondeu com erro " <> T.pack (show s) <> "."))

    mensagemDeErro :: HttpException -> Text
    mensagemDeErro _ = "Não foi possível acessar a API. Verifique a conexão com a internet."

-- Busca lancamentos oficiais pelo titulo e, opcionalmente, pelo artista.
-- A busca usa a sintaxe do MusicBrainz: release:"..." AND artist:"..."
buscarLancamentos :: Text -> Text -> IO (Either Text [ResultadoBusca])
buscarLancamentos titulo artista = do
    resultado <- requisitarJSON (urlMusicBrainz <> "/release/")
        [ ("query", Just (TE.encodeUtf8 consulta))
        , ("fmt",   Just "json")
        , ("limit", Just "15")
        ]
    pure (fmap (\(RespostaBusca rs) -> rs) resultado)
  where
    -- Aspas e barras digitadas pelo usuario quebrariam a consulta.
    limpar = T.strip . T.filter (`notElem` ("\"\\" :: String))
    consulta = T.intercalate " AND " $
        [ "release:\"" <> limpar titulo <> "\"" ]
        ++ [ "artist:\"" <> limpar artista <> "\"" | not (T.null (limpar artista)) ]
        ++ [ "status:official" ]

-- Dados completos de um lancamento, com faixas, artista e generos.
buscarLancamento :: Text -> IO (Either Text Lancamento)
buscarLancamento mbid =
    requisitarJSON (urlMusicBrainz <> "/release/" <> T.unpack mbid)
        [ ("inc", Just "recordings+artist-credits+genres")
        , ("fmt", Just "json")
        ]

-- Pais e ano de inicio do artista.
buscarArtista :: Text -> IO (Either Text ArtistaApi)
buscarArtista mbid =
    requisitarJSON (urlMusicBrainz <> "/artist/" <> T.unpack mbid) [("fmt", Just "json")]

-- URL da capa (frente) no Cover Art Archive, ou Nothing se nao houver.
buscarCapa :: Text -> IO (Maybe Text)
buscarCapa mbid = do
    resultado <- requisitarJSON (urlCoverArt <> "/release/" <> T.unpack mbid) []
    pure $ case resultado of
        Right (RespostaCapa url) -> fmap paraHttps url
        Left _                   -> Nothing
  where
    paraHttps url = fromMaybe url (("https://" <>) <$> T.stripPrefix "http://" url)

-- Miniatura usada direto na tag <img> dos resultados da busca
-- (o navegador carrega a imagem; nao passa pelo servidor).
urlMiniaturaCapa :: Text -> Text
urlMiniaturaCapa mbid = T.pack urlCoverArt <> "/release/" <> mbid <> "/front-250"

-- Converte o formato do MusicBrainz para o formato da loja.
formatoDoMusicBrainz :: Text -> Maybe Formato
formatoDoMusicBrainz texto
    | "vinyl"    `T.isInfixOf` t = Just Vinil
    | "cassette" `T.isInfixOf` t = Just Cassete
    | "cd"       `T.isInfixOf` t = Just CD
    | otherwise                  = Nothing
  where
    t = T.toLower texto

-- MBID e um UUID: 36 caracteres, hexadecimais e hifens.
-- Evita mandar para a API qualquer texto digitado na URL.
mbidValido :: Text -> Bool
mbidValido t = T.length t == 36 && T.all (\c -> isHexDigit c || c == '-') t
