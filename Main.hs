{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
import Foundation
import Application () -- for YesodDispatch instance
import Yesod
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql

connStr :: ConnectionString
connStr = "dbname=d5j1fjv8sgepv8 host=ec2-107-22-238-96.compute-1.amazonaws.com user=fcazhsohpfwvvm password=vg9J-PW_A_euhMYWO4mqaa0Sam port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       warp 8080 (App pool)