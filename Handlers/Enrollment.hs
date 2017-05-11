{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

module Handlers.Enrollment where

import Yesod
import Foundation
import Data.Text
import Database.Persist.Postgresql

formEnroll :: Form Enrollment
formEnroll = renderDivs $ Enrollment
    <$> areq (selectField studentList) "Aluno:  " Nothing
    <*> areq (selectField courseList) "Curso:  " Nothing
    where
        studentList :: Handler (OptionList PersonId)
        studentList = do
            studs <- runDB $ selectList [PersonDiscriminador ==. "3"] [Asc PersonFistname]
            optionsPairs $ Prelude.map (\cur -> (personFistname $ entityVal cur, entityKey cur)) studs
            
        courseList :: Handler (OptionList CourseId)
        courseList = do
            courses <- runDB $ selectList [] [Asc CourseName]
            optionsPairs $ Prelude.map (\cur -> (courseName $ entityVal cur, entityKey cur)) courses
    
getSubscribeR :: Handler Html
getSubscribeR = do
            (widget, enctype) <- generateFormPost formEnroll
            defaultLayout [whamlet|
              <form method=post action=@{SubscribeR} enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Inscrever Aluno">
            |]
            
postSubscribeR :: Handler Html
postSubscribeR = do
            ((result, _), _) <- runFormPost formEnroll
            case (result) of
                FormSuccess enroll -> do
                    runDB $ insert enroll
                    defaultLayout [whamlet|
                        Aluno inscrito no curso com sucesso!
                    |]
                _ -> redirect HomeR
                
getListEnrollmentR :: Handler Html
getListEnrollmentR = do
    course <- runDB $ rawSql
        "SELECT ??, ?? FROM person, course, Enrollment WHERE Enrollment.student_id = person.id" []
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
                <h3> Lista de Inscrições
                <table class="table table-striped">
                    <thead>
                        <tr>
                            <th> Nome do aluno
                            <th> Nome do curso
                    <tbody>
                    $forall (Entity cid course, Entity _ person) <- course
                        <tr>
                            <td> #{courseName course}
                            <td> #{personFistname person}
                <a class="btn btn-warning" href="@{HomeR}">Voltar</a>
  
        |]
        
        

getListChamadaR :: Handler Html
getListChamadaR = do
    course <- runDB $ rawSql
        "SELECT ??, ?? FROM person left JOIN course ON person.id = course.person_id JOIN enrollment ON enrollment.course_id = course.id JOIN frequency ON (frequency.enrollment_id = enrollment.id)" []
    defaultLayout $ do
        [whamlet|

            <h1> Listagem de Inscrições
             
            <table>
                    <tr>
                    <td> Nome do Curso
                    <td> Nome do Professor
                $forall (Entity cid course, Entity _ person) <- course
                    <tr>
                        <td> #{courseName course}
                        <td> #{personFistname person}
            
            
            <a href=@{ListEnrollmentR}>Atualizar
            <a href=@{HomeR}>Voltar
            
            
        |]
 