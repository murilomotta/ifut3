{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.User where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Class
import Database.Persist.Postgresql
import Data.ByteString.Base64 (decode)
import Data.Bits (xor)
import Data.ByteString         as B
import Data.ByteString.Char8   as C
import Data.String.Conversions (cs)
                          
getCadastroR :: Handler Html
getCadastroR = defaultLayout $ do
    [whamlet|
        <head>
            <title>Sistema Chamada
            <meta charset="utf-8">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
            <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
            <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
        <body>
            <form method=post action=@{CadastroR}>
                <h4>Primeiro Nome: <input type=text name=fistName>
                <h4>Sobrenome: <input type=text name=lastName>
                <h4>Nome de usuario: <input type=text name=username>
                <h4> Senha: <input type=password name=password>
                <h4>Tipo de usuario: <select name=discriminator></br>
                    <option value="1">Admin
                    <option value="2">Professor
                    <option value="3">Aluno
                <a><input class="btn btn-success" type=submit value="Cadastrar">
                <a class="btn btn-warning" href="@{HomeR}">Voltar</a>
    |]

postCadastroR :: Handler Html
postCadastroR = do
                    firstName <- runInputPost $ ireq textField "fistName"
                    lastName <- runInputPost $ ireq textField "lastName"
                    userame <- runInputPost $ ireq textField "username"
                    password <- runInputPost $ ireq textField "password"
                    let Right key = decode "kTSFoLQRrR+hWJlLjAwXqOH5Z3ZLDWray5mBgNK7lLuHdTwab8m/v96y"
                    let encrypt = B.pack . B.zipWith xor key
                    let pw = cs password :: ByteString
                    let hash = encrypt pw
                    disc <- runInputPost $ ireq textField "discriminator"
                    runDB $ insert $ Person firstName lastName disc userame hash
                    defaultLayout [whamlet|
                        <head>
                            <title>Sistema Chamada
                            <meta charset="utf-8">
                            <meta name="viewport" content="width=device-width, initial-scale=1">
                            <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
                            <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
                            <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
                        <body>
                            <h1>Cadastro realizado com sucesso.
                    |]
                    
getListPessoaR :: Handler Html
getListPessoaR = do
            persons <- runDB $ selectList [] []
            defaultLayout $ do
                [whamlet|
                <head>
                    <title>Sistema Chamada
                    <meta charset="utf-8">
                    <meta name="viewport" content="width=device-width, initial-scale=1">
                    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
                    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
                    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
                <body>
                    <h3>Usuario:
                    <table class="table table-striped">
                        <thead>
                            <tr>
                                <th>Nome
                                <th>Visualizar
                        <tbody>
                        $forall Entity pid person <- persons
                            <tr>
                                <td>#{personFistname person} #{personLastname person} 
                                <td><a class="btn btn-success" href=@{PessoaR pid}>Ver detalhes</a>
                    <a class="btn btn-warning" href="@{HomeR}">Voltar</a>
                |]
                    
getPessoaR :: PersonId -> Handler Html
getPessoaR pid = do
             person <- runDB $ get404 pid
             defaultLayout [whamlet|
                <head>
                    <title>Sistema Chamada
                    <meta charset="utf-8">
                    <meta name="viewport" content="width=device-width, initial-scale=1">
                    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
                    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
                    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
                <body>
                    <div style="margin:10px;" >
                        <h3>Informações do usuario:
                        <h5>Primeiro nome: #{personFistname person}
                        <h5>Sobrenome: #{personLastname person}
                        <h5>Username: #{personUsername person}
                        <a class="btn btn-warning" href="@{ListPessoaR}">Voltar</a>
             |]