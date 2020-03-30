module Main where

import Prelude hiding (div)

-- import Affjax (get) as Affjax
-- import Affjax.ResponseFormat (json) as ResponseFormat
-- import Data.Argonaut.Core (Json)
-- import Data.Argonaut.Core (toArray, toObject, toString) as JSON
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.NonEmpty as NonEmpty
import Data.Traversable (traverse)
-- import Debug.Trace (spy)
import Effect (Effect, foreachE)
-- import Effect.Aff (Aff, error, launchAff_, throwError, try)
import Effect.Class (liftEffect)
import Effect.Console (error, log) as Console
-- import Foreign.Object (lookup) as Object
import Math (asin)
import Web.DOM (Document) as DOM
import Web.DOM.Document (createElement, toNonElementParentNode) as Web.DOM.Document
import Web.DOM.Element (Element, setAttribute, setId, setClassName, toEventTarget, toNode) as Web.DOM.Element
import Web.DOM.Node (appendChild, parentNode, removeChild, setTextContent) as Web.DOM.Node
import Web.DOM.NonElementParentNode (getElementById) as Web.DOM.NonElementParentNode
import Web.Event.Event (Event) as Event
import Web.Event.EventTarget (addEventListener, eventListener) as Event
import Web.HTML (window) as Web.HTML
import Web.HTML.Event.EventTypes (click) as Event
import Web.HTML.HTMLDocument (toDocument,body, toDocument,HTMLDocument) as Web.HTML.HTMLDocument
import Web.HTML.HTMLElement (toNode,HTMLElement) as Web.HTML.HTMLElement
import Web.HTML.HTMLInputElement (fromElement, value) as HTMLInput
import Web.HTML.Window (document,Window) as Web.HTML.Window
import Web.DOM.Internal.Types(Node) as Web.DOM.Internal.Types

foreign import setStyleProp :: String -> String -> Web.DOM.Element.Element -> Effect Boolean
createBoxElement :: String -> DOM.Document -> Effect Web.DOM.Element.Element
createBoxElement id document = do
    boxEl <-  Web.DOM.Document.createElement "div" document -- 
    Web.DOM.Element.setId id boxEl
    Web.DOM.Element.setClassName "box" boxEl
    _ <- setStyleProp "position" "relative" boxEl
    _ <- setStyleProp "width" "5em" boxEl
    _ <- setStyleProp "height" "5em" boxEl
    _ <- setStyleProp "background" "#ff4242" boxEl
    pure boxEl


label :: String -> DOM.Document -> Effect Web.DOM.Element.Element
label text document = do
  elem <- Web.DOM.Document.createElement "label" document
  Web.DOM.Node.setTextContent text (Web.DOM.Element.toNode elem)
  pure elem 



main :: Effect Unit
main = do
  Console.log "prova"
  window        <- Web.HTML.window :: Effect Web.HTML.Window.Window
  htmlDocument  <- Web.HTML.Window.document window :: Effect Web.HTML.HTMLDocument.HTMLDocument
  let document =  Web.HTML.HTMLDocument.toDocument htmlDocument :: DOM.Document 
  maybeBody     <- Web.HTML.HTMLDocument.body htmlDocument :: Effect (Maybe Web.HTML.HTMLElement.HTMLElement)
  case maybeBody of
    Nothing   -> Console.error "no body element found!"
    Just (body::  Web.HTML.HTMLElement.HTMLElement) -> do
      let bodyNode = (Web.HTML.HTMLElement.toNode :: Web.HTML.HTMLElement.HTMLElement -> Web.DOM.Internal.Types.Node)(body::  Web.HTML.HTMLElement.HTMLElement) 

      defaultElem <- (Web.DOM.Document.createElement "span" document)
      boxEl <- createBoxElement "the-box"  document
      newBody <- Web.DOM.Node.appendChild (Web.DOM.Element.toNode boxEl) bodyNode

      container   <- Web.DOM.Document.createElement "section" document
      repoLabel   <- label "Subreddit1" document
      let containerNode = Web.DOM.Element.toNode container
      Web.DOM.Node.appendChild (Web.DOM.Element.toNode repoLabel) containerNode # void
      Web.DOM.Node.appendChild containerNode bodyNode # void
      

