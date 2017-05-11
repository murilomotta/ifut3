{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable, EmptyDataDecls,
             GeneralizedNewtypeDeriving, ViewPatterns, FlexibleInstances #-}
module Foundation where
import Yesod
import Yesod.Static
import Data.String
import Data.ByteString
import Data.Text
import Crypto.PasswordStore
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool )

data App = App {connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    fistname Text
    lastname Text
    discriminador Text
    username Text
    password ByteString
    deriving Show

Course
    name Text
    description Text
    personId PersonId
    totalClass Int
    deriving Show
    
Enrollment
    studentId PersonId
    courseId CourseId
    deriving Show
    
Frequency
    courseId CourseId
    enrollmentId EnrollmentId
    deriving Show
|]

mkYesodData "App" $(parseRoutesFile "routes")

instance YesodPersist App where
   type YesodPersistBackend App = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod App where
    authRoute _ = Just LoginR
    
    isAuthorized LoginR _ = return Authorized
    isAuthorized _ _ = isAuthenticated

isAuthenticated :: Handler AuthResult
isAuthenticated = do
    msu <- lookupSession "_ID"
    case msu of
        Just _ -> return Authorized
        Nothing -> return AuthenticationRequired

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage