{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

module Main where
import Application () -- for YesodDispatch instance
import Yesod
import Yesod.Static
import Foundation
-- import Application
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

connStr = "dbname=d8ul494tou53ak host=ec2-54-83-49-44.compute-1.amazonaws.com user=hfzumdslxbwsqk password=bfb84ba3961db31fd587bdfdce3b83d4993fbb485241bd816f7ce57dab5ac953"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       t@(Static settings) <- static "static"
       warp 3000 (Sitio t pool)