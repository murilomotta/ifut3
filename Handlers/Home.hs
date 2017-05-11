{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
module Handlers.Home where

import Foundation
import Yesod
import Data.Int
import Data.Text as T
import Text.Read
import Database.Persist.Postgresql as PSQL

getHomeR :: Handler Html
getHomeR = do
    defaultLayout [whamlet|
        <head>
            <title>Sistema Chamada
            <meta charset="utf-8">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
            <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
            <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
        <body>
            <div style="text-align:center; width:15px; margin:50px;">
                <div style="text-align:center; width:50px; margin:10px;">
                    <img src="http://22sense.com.br/wp-content/uploads/2016/11/Logo2-e1480448435997.png">
                <div class="btn-group-vertical" style="margin:10px;" role="group">
                    <a class="btn btn-success" role="group" href=@{CadastroR}>Cadastrar usuário
                    <a class="btn btn-success" role="group" href=@{ListPessoaR}>Listar usuários
                    <a class="btn btn-success" role="group" href=@{ListCourseR}>Cursos
                    <a class="btn btn-success" role="group" href=@{SubscribeR}>Inscrever aluno
                    <a class="btn btn-warning" role="group" href=@{LogoutR}>Sair
    |]