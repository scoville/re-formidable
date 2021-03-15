open Jest
open Expect
open! Operators
open ReactTestingLibrary
open TestHelpers

describe("Formidable", () => {
  test("Component renders", () => {
    renderApp()->container->expect->toMatchSnapshot
  })

  describe("Field (example: email)", () => {
    let existingEmail = "already@exists.com"

    let wrongEmail = "foobar.com"

    let validEmail = "foo@bar.com"

    let email = Input.make("email")

    test("Pristine status", () => {
      let app = renderApp()

      email.expectToMatchSnapshot(app)
    })

    test("Change focus status - focus", () => {
      let app = renderApp()

      act(() => app->email.input->FireEvent.focusIn)

      email.expectToMatchSnapshot(app)
    })

    test(
      "Change focus status and trigger only displays loading/validating message - focus, then blur",
      () => {
        let app = renderApp()

        act(() => app->email.input->FireEvent.focusIn)

        act(() => app->email.input->FireEvent.focusOut)

        email.expectToMatchSnapshot(app)
      },
    )

    testAsync(
      "Change focus status and trigger already exists error - focus, type email address, then blur",
      done => {
        let app = renderApp()

        act(() => app->email.input->FireEvent.focusIn)

        act(() =>
          app->email.input->FireEvent.input(~eventInit={"target": {"value": existingEmail}})
        )

        act(() => app->email.input->FireEvent.focusOut)

        Js.Global.setTimeout(() => done(email.expectToMatchSnapshot(app)), 300)->ignore
      },
    )

    testAsync("Change focus status and trigger required error - focus then blur", done => {
      let app = renderApp()

      act(() => app->email.input->FireEvent.focusIn)

      act(() => app->email.input->FireEvent.focusOut)

      Js.Global.setTimeout(() => done(email.expectToMatchSnapshot(app)), 300)->ignore
    })

    test("Email format error", () => {
      let app = renderApp()

      app
      ->email.input
      ->FireEvent.input(
        ~eventInit={
          "target": {
            "value": wrongEmail,
          },
        },
      )

      email.expectToMatchSnapshot(app)
    })

    test("Updates value", () => {
      let app = renderApp()

      act(() =>
        app
        ->email.input
        ->FireEvent.input(
          ~eventInit={
            "target": {
              "value": validEmail,
            },
          },
        )
      )

      email.expectToMatchSnapshot(app)
    })

    test("Submit errors", () => {
      let app = renderApp()

      act(() => app->submitButton->FireEvent.click)

      email.expectToMatchSnapshot(app)
    })

    test("Submit and reset", () => {
      let app = renderApp()

      act(() => app->submitButton->FireEvent.click)

      act(() => app->resetButton->FireEvent.click)

      email.expectToMatchSnapshot(app)
    })

    test("Submit with errors", () => {
      let app = renderApp()

      act(() =>
        app
        ->email.input
        ->FireEvent.input(
          ~eventInit={
            "target": {
              "value": wrongEmail,
            },
          },
        )
      )

      act(() => app->submitButton->FireEvent.click)

      email.expectToMatchSnapshot(app)
    })

    test("On focus listener - no listener", () => {
      let (spy, _) = Handler.make(_ => "focused")

      let app = renderApp()

      act(() => app->email.input->FireEvent.focusIn)

      Handler.expectToMatchSnapshot(spy)
    })

    test("On focus listener - no event", () => {
      let (spy, onFocus) = Handler.make(_ => "focused")

      ignore(renderApp(~onFocus, ()))

      Handler.expectToMatchSnapshot(spy)
    })

    test("On focus listener - wrong event", () => {
      let (spy, onFocus) = Handler.make(_ => "focused")

      let app = renderApp(~onFocus, ())

      act(() => app->email.input->FireEvent.focusOut)

      Handler.expectToMatchSnapshot(spy)
    })

    test("On focus listener", () => {
      let (spy, onFocus) = Handler.make(_ => "focused")

      let app = renderApp(~onFocus, ())

      act(() => app->email.input->FireEvent.focusIn)

      Handler.expectToMatchSnapshot(spy)
    })

    test("On blur listener - no listener", () => {
      let (spy, _) = Handler.make(_ => "blurred")

      let app = renderApp()

      act(() => app->email.input->FireEvent.focusOut)

      Handler.expectToMatchSnapshot(spy)
    })

    test("On blur listener - no event", () => {
      let (spy, onBlur) = Handler.make(_ => "blurred")

      ignore(renderApp(~onBlur, ()))

      Handler.expectToMatchSnapshot(spy)
    })

    test("On blur listener - wrong event", () => {
      let (spy, onBlur) = Handler.make(_ => "blurred")

      let app = renderApp(~onBlur, ())

      act(() => app->email.input->FireEvent.focusIn)

      Handler.expectToMatchSnapshot(spy)
    })

    test("On blur listener", () => {
      let (spy, onBlur) = Handler.make(_ => "blurred")

      let app = renderApp(~onBlur, ())

      act(() => app->email.input->FireEvent.focusOut)

      Handler.expectToMatchSnapshot(spy)
    })

    test("On change listener", () => {
      let (spy, onChange) = Handler.make(value => `changed: ${value}`)

      let app = renderApp(~onChange, ())

      act(() =>
        app
        ->email.input
        ->FireEvent.input(
          ~eventInit={
            "target": {
              "value": "foo@",
            },
          },
        )
      )

      act(() =>
        app
        ->email.input
        ->FireEvent.input(
          ~eventInit={
            "target": {
              "value": "bar.",
            },
          },
        )
      )

      act(() =>
        app
        ->email.input
        ->FireEvent.input(
          ~eventInit={
            "target": {
              "value": "com",
            },
          },
        )
      )

      Handler.expectToMatchSnapshot(spy)
    })
  })
})
