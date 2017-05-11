{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Yesod

import Handlers.Home
import Handlers.User
import Handlers.Login
import Handlers.Course
import Handlers.Enrollment
import Handlers.Frequency

mkYesodDispatch "App" resourcesApp
