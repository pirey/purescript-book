module Main
  ( main
  ) where

import Prelude
import Data.AddressBook (PhoneNumber, examplePerson)
import Data.AddressBook.Validation (Errors, Field(..), ValidationError(..), validatePerson')
import Data.Array (length, mapWithIndex, updateAt)
import Data.Array as List
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (ReactComponent, element, reactComponent, useState)
import React.Basic.Hooks as R
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

renderError :: Maybe ValidationError -> R.JSX
renderError (Nothing) = R.empty

renderError (Just (ValidationError err _)) = D.div { className: "alert alert-danger", children: [ D.text err ] }

getFieldError :: Errors -> Field -> Maybe ValidationError
getFieldError errors field = List.find (\(ValidationError _ vfield) -> vfield == field) errors

-- Helper function to render a single form field with an
-- event handler to update
formField :: String -> String -> String -> Maybe ValidationError -> (String -> Effect Unit) -> R.JSX
formField name placeholder value maybeValidation setValue =
  D.div
    { className: "form-group row"
    , children:
        [ D.label
            { className: "col-sm col-form-label"
            , htmlFor: name
            , children: [ D.text name ]
            }
        , D.div
            { className: "col-sm"
            , children:
                [ D.input
                    { className: "form-control"
                    , id: name
                    , placeholder
                    , value
                    , onChange: handler targetValue $ traverse_ setValue
                    }
                ]
            }
        , D.div
            { className: "col-sm"
            , children: [ renderError maybeValidation ]
            }
        ]
    }

mkAddressBookApp :: Effect (ReactComponent {})
mkAddressBookApp =
  -- incoming \props are unused
  reactComponent "AddressBookApp" \_ -> R.do
    -- `useState` takes a default initial value and returns the
    -- current value and a way to update the value.
    -- Consult react-hooks docs for a more detailed explanation of `useState`.
    Tuple person setPerson <- useState examplePerson
    let
      errors = case validatePerson' person of
        Left e -> e
        Right _ -> []

      -- helper-function to return array unchanged instead of Nothing if index is out of bounds
      updateAt' :: forall a. Int -> a -> Array a -> Array a
      updateAt' i x xs = fromMaybe xs (updateAt i x xs)

      -- helper-function to render a single phone number at a given index
      renderPhoneNumber :: Int -> PhoneNumber -> R.JSX
      renderPhoneNumber index phone =
        formField
          (show phone."type")
          "XXX-XXX-XXXX"
          phone.number
          (getFieldError errors (PhoneField phone.type))
          (\s -> setPerson _ { phones = updateAt' index phone { number = s } person.phones })

      -- helper-function to render all phone numbers
      renderPhoneNumbers :: Array R.JSX
      renderPhoneNumbers = mapWithIndex renderPhoneNumber person.phones

      submitButton =
        D.button
          { disabled: length errors > 0
          , className: "btn btn-primary"
          , children: [ D.text "Submit" ]
          }
    pure
      $ D.div
          { className: "container"
          , children:
              [ D.div
                  { className: "row"
                  , key: "person-form"
                  , children:
                      [ D.form
                          { className: "w-100"
                          , children:
                              [ D.h3_ [ D.text "Basic Information" ]
                              , formField "First Name" "First Name" person.firstName (getFieldError errors FirstNameField) \s ->
                                  setPerson _ { firstName = s }
                              , formField "Last Name" "Last Name" person.lastName (getFieldError errors LastNameField) \s ->
                                  setPerson _ { lastName = s }
                              , D.h3_ [ D.text "Address" ]
                              , formField "Street" "Street" person.homeAddress.street (getFieldError errors StreetField) \s ->
                                  setPerson _ { homeAddress { street = s } }
                              , formField "City" "City" person.homeAddress.city (getFieldError errors CityField) \s ->
                                  setPerson _ { homeAddress { city = s } }
                              , formField "State" "State" person.homeAddress.state (getFieldError errors StateField) \s ->
                                  setPerson _ { homeAddress { state = s } }
                              , D.h3_ [ D.text "Contact Information" ]
                              ]
                                <> renderPhoneNumbers
                                <> [ submitButton ]
                          }
                      ]
                  }
              ]
          }

main :: Effect Unit
main = do
  log "Rendering address book component"
  -- Get window object
  w <- window
  -- Get window's HTML document
  doc <- document w
  -- Get "container" element in HTML
  ctr <- getElementById "container" $ toNonElementParentNode doc
  case ctr of
    Nothing -> throw "Container element not found."
    Just c -> do
      -- Create AddressBook react component
      addressBookApp <- mkAddressBookApp
      let
        -- Create JSX node from react component. Pass-in empty props
        app = element addressBookApp {}
      -- Render AddressBook JSX node in DOM "container" element
      D.render app c
