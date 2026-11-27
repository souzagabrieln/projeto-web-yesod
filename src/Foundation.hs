{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-} -- exigido pelo persistent >= 2.14
{-# LANGUAGE TypeOperators              #-}

module Foundation where

import Yesod
import Yesod.Static
import Yesod.Form.I18n.Portuguese (portugueseFormMessage)
import Database.Persist.Sqlite
import Data.Text (Text)
import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFile)

-- Gera identificadores para os arquivos da pasta static
-- (ex.: static/img/logo.svg vira img_logo_svg).
staticFiles "static"

-- Modelo do banco de dados. Cada bloco vira uma tabela no SQLite,
-- um tipo em Haskell (Artista, Genero) e uma chave (ArtistaId, GeneroId).
-- As linhas "Unique..." criam restricoes de unicidade no banco.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Artista
    nome      Text
    pais      Text Maybe
    anoInicio Int  Maybe
    UniqueArtistaNome nome
    deriving Show

Genero
    nome      Text
    descricao Textarea Maybe
    UniqueGeneroNome nome
    deriving Show
|]

-- Estado global da aplicacao: o pool de conexoes com o banco
-- e os arquivos estaticos.
data App = App
    { connPool  :: ConnectionPool
    , getStatic :: Static
    }

mkYesodData "App" $(parseRoutesFile "routes.yesodroutes")

-- Itens do menu de navegacao: texto exibido e rota de destino.
menuItens :: [(Text, Route App)]
menuItens =
    [ ("Início",   HomeR)
    , ("Artistas", ArtistasR)
    , ("Gêneros",  GenerosR)
    ]

instance Yesod App where
    -- Layout usado por todas as paginas: cabecalho com o menu,
    -- mensagem de aviso (setMessage) e o conteudo da pagina.
    defaultLayout conteudo = do
        mensagem <- getMessage
        rotaAtual <- getCurrentRoute
        pagina <- widgetToPageContent $ do
            toWidget $(luciusFile "templates/layout.lucius")
            conteudo
        withUrlRenderer $(hamletFile "templates/layout.hamlet")

    -- Pagina mostrada para rotas inexistentes ou registros apagados (get404).
    errorHandler NotFound = fmap toTypedContent $ defaultLayout $ do
        setTitle "Página não encontrada"
        [whamlet|
            <h1>Página não encontrada
            <p>O endereço acessado não existe ou o registro foi removido.
            <a .botao href=@{HomeR}>Voltar ao início
        |]
    errorHandler outro = defaultErrorHandler outro

-- Mensagens de validacao dos formularios em portugues.
instance RenderMessage App FormMessage where
    renderMessage _ _ = portugueseFormMessage

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB acao = do
        app <- getYesod
        runSqlPool acao (connPool app)

-- Atalho para os formularios da aplicacao.
type Form a = Html -> MForm Handler (FormResult a, Widget)
