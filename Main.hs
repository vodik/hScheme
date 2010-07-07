module Main where

import System
import System.Environment
import Control.Monad
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.SourceView

import Lisp

newSourceView :: IO SourceBuffer
newSourceView = do
    manager  <- sourceLanguageManagerNew
    language <- sourceLanguageManagerGetLanguage manager "scheme"
    case language of
         Nothing     -> exitFailure
         (Just lang) -> sourceBufferNewWithLanguage lang >>= return

startGUI :: IO ()
startGUI = do
    initGUI
    builder <- builderNew
    builderAddFromFile builder "scheme.ui"

    window <- builderGetObject builder castToWindow "window1"
    window `onDestroy` mainQuit

    scrwin     <- builderGetObject builder castToScrolledWindow "scrolledwindow1"
    sourceView <- newSourceView >>= sourceViewNewWithBuffer
    srcfont    <- fontDescriptionFromString "Monospace 10"

    widgetModifyFont sourceView (Just srcfont)
    set sourceView [sourceViewHighlightCurrentLine := True,
                    sourceViewShowLineNumbers := True,
                    sourceViewAutoIndent := True]
    scrolledWindowAddWithViewport scrwin sourceView

    widgetShowAll window
    mainGUI

main :: IO ()
main = do args <- getArgs
          if null args
             then startGUI
             else runOne $ args
