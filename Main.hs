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

runCode :: SourceView -> IO ()
runCode sourceView = do
    textBuffer <- textViewGetBuffer sourceView
    sI <- textBufferGetStartIter textBuffer
    eI <- textBufferGetEndIter textBuffer
    textBufferGetText textBuffer sI eI True >>= runScript

startGUI :: IO ()
startGUI = do
    initGUI
    builder <- builderNew
    builderAddFromFile builder "scheme.ui"

    window     <- builderGetObject builder castToWindow "window1"
    scrwin     <- builderGetObject builder castToScrolledWindow "scrolledwindow1"
    sourceView <- newSourceView >>= sourceViewNewWithBuffer
    srcfont    <- fontDescriptionFromString "Monospace 10"

    widgetModifyFont sourceView (Just srcfont)
    set sourceView [sourceViewHighlightCurrentLine := True,
                    sourceViewShowLineNumbers := True,
                    sourceViewAutoIndent := True,
                    sourceViewTabWidth := 4,
                    sourceViewInsertSpacesInsteadOfTabs := True]
    scrolledWindowAddWithViewport scrwin sourceView

    exia <- builderGetObject builder castToAction "EXIA"
    exia `onActionActivate` widgetDestroy window

    exca <- builderGetObject builder castToAction "EXCA"
    exca `onActionActivate` runCode sourceView

    window `onDestroy` mainQuit
    widgetShowAll window
    mainGUI

main :: IO ()
main = do args <- getArgs
          if null args
             then startGUI
             else runOne $ args
