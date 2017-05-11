{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

module Handlers.Login where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text as T
import Text.Lucius
import Database.Persist.Postgresql
import Data.ByteString.Base64 (decode)
import Data.Bits (xor)
import Data.ByteString as B
import Data.ByteString.Char8 as C
import Data.String.Conversions (cs)

getLoginR :: Handler Html
getLoginR = defaultLayout
    [whamlet|
        <head>
            <title>Sistema Chamada
            <meta charset="utf-8">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
            <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
            <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
        <body>
            <form method=post action=@{LoginR}>
                <h5>Nome de Usuário: 
                <input type=text name=username>
                <h5>Senha: 
                <input type=password name=password>
                <input class="btn btn-success" type=submit value="Entrar">
    |]
    
postLoginR :: Handler Html
postLoginR = do
                userName <- runInputPost $ ireq textField "username"
                password <- runInputPost $  ireq textField "password"
                let Right key = decode "kTSFoLQRrR+hWJlLjAwXqOH5Z3ZLDWray5mBgNK7lLuHdTwab8m/v96y"
                let encrypt = B.pack . B.zipWith xor key
                let pw = cs password :: ByteString
                let hash = encrypt pw
                user <- runDB $ selectFirst [PersonUsername ==. userName, PersonPassword ==. hash] []
                case user of
                    Just (Entity pid person) -> do
                        setSession "_ID" (T.pack $ show $ fromSqlKey pid)
                        redirect (HomeR)
                    Nothing -> redirect LoginR
                    
getLogoutR :: Handler Html
getLogoutR = do
    deleteSession "_ID"
    redirect HomeR