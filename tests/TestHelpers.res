open Jest
open Expect
open! Operators
open Webapi
open ReactTestingLibrary

let renderApp = (
  ~onBlur as onInputBlur=?,
  ~onChange as onInputChange=?,
  ~onFocus as onInputFocus=?,
  (),
) => render(<App ?onInputBlur ?onInputChange ?onInputFocus />)

let submitButton = getByTestId(~matcher=#Str("submit"))

let resetButton = getByTestId(~matcher=#Str("reset"))

module Handler = {
  let make = f => {
    let spy = JestJs.inferred_fn()
    let fn = MockJs.fn(spy)
    let handler = arg => ignore(fn(. f(arg)))

    (spy, handler)
  }

  let expectToMatchSnapshot = spy => spy->MockJs.calls->expect->toMatchSnapshot
}

module Input = {
  module AssertionResult = {
    type t = {
      value: string,
      focusState: string,
      status: string,
    }
  }

  type t = {
    expectToMatchSnapshot: renderResult => assertion,
    focusState: renderResult => Dom.Element.t,
    input: renderResult => Dom.Element.t,
    status: renderResult => Dom.Element.t,
  }

  let make = name => {
    let input = getByTestId(~matcher=#Str(name))

    let focusState = result => result->input->Dom.Element.nextElementSibling->Belt.Option.getExn

    let status = result => result->focusState->Dom.Element.nextElementSibling->Belt.Option.getExn

    let expectToMatchSnapshot = app =>
      {
        AssertionResult.focusState: app->focusState->Dom.Element.textContent,
        status: app->status->Dom.Element.textContent,
        value: app->input->Dom.Element.asHtmlElement->Belt.Option.getExn->Dom.HtmlElement.value,
      }
      ->expect
      ->toMatchSnapshot

    {
      expectToMatchSnapshot: expectToMatchSnapshot,
      focusState: focusState,
      input: input,
      status: status,
    }
  }
}
