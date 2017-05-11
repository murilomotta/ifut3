{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

module Handlers.Frequency where

import Yesod
import Foundation
import Data.Text
import Database.Persist.Postgresql

getChamadaR :: CourseId -> Handler Html
getChamadaR cid = do
    let crsid = (fromSqlKey cid)
    enrolls <- runDB $ selectList [EnrollmentCourseId ==. (toSqlKey crsid)] []
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
            <h3>Lista de Presença
            <table class="table table-striped">
                <thead>
                    <tr>
                        <th>Numero
                        <th>Status
                <tbody>
                $forall (Entity eid enrollment) <- enrolls
                    <tr>
                        <td>#{fromSqlKey eid}
                        <td><a class="btn btn-success" href=@{SalvarChamadaR cid eid}> Presente</a>
            <a class="btn btn-warning" href="@{ListCourseR}">Voltar</a>
    |]
    
getSalvarChamadaR :: CourseId -> EnrollmentId -> Handler Html
getSalvarChamadaR cid eid = do
    runDB $ insert $ Frequency cid eid
    redirect (ChamadaR cid)
