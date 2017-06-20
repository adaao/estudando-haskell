{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

-- Modulos necessarios
import Yesod.Core
import Yesod
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

-- Definição do App
data App = App {getStatic :: Static, connPool :: ConnectionPool }

-- Criacao das tabelas
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuario
    nome Text
    email Text
    senha Text
    UniqueEmail email
|]

mkYesodData "App" $(parseRoutesFile "routes")

-- Determina o nome da pasta com os arquivos de estilo e js 
staticFiles "static"

--instance Yesod App


