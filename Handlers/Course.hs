{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

module Handlers.Course where

import Yesod
import Foundation
import Data.Text
import Database.Persist.Postgresql

formCourse :: Form Course
formCourse = renderDivs $ Course
    <$> areq textField "Nome do Curso: " Nothing
    <*> areq textField "Descrição do Curso: " Nothing
    <*> areq (selectField profList) "Professor:  " Nothing
    <*> areq intField "Quantidade de aulas: " Nothing
    where
        profList :: Handler (OptionList PersonId)
        profList = do
            profs <- runDB $ selectList [PersonDiscriminador ==. "2"] [Asc PersonFistname]
            optionsPairs $ Prelude.map (\cur -> (personFistname $ entityVal cur, entityKey cur)) profs
    
getCadastroCourseR :: Handler Html
getCadastroCourseR = do
            (widget, enctype) <- generateFormPost formCourse
            defaultLayout [whamlet|
              <form method=post action=@{CadastroCourseR} enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar Curso">
            |]
            
postCadastroCourseR :: Handler Html
postCadastroCourseR = do
            ((result, _), _) <- runFormPost formCourse
            case (result) of
                FormSuccess course -> do
                    runDB $ insert course
                    defaultLayout [whamlet|
                        Curso cadastrado com sucesso!
                    |]
                _ -> redirect HomeR
                
getListCourseR :: Handler Html
getListCourseR = do 
    course <- runDB $ rawSql
        "SELECT ??, ?? FROM course INNER JOIN  person ON course.person_id = person.id" []
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
                <h3>Lista de Cursos
                <a class="btn btn-warning" href=@{CadastroCourseR}>Novo curso
                <table class="table table-striped">
                    <thead>
                        <tr>
                            <th> Nome do curso
                            <th> Descrição
                            <th> Professor
                            <th> Total de aulas
                    <tbody>
                    $forall (Entity cid course, Entity _ person) <- course
                        <tr>
                            <td> #{courseName course}
                            <td> #{courseDescription course}
                            <td> #{personFistname person}
                            <td> #{courseTotalClass course}
                            <td> <a class="btn btn-success" href=@{ChamadaR cid}>Fazer chamada
                <a class="btn btn-warning" href="@{HomeR}">Voltar</a>
        |]