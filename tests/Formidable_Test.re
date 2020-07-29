open Relude.Globals;
open Jest;
open Expect;
open! Operators;
open ReactTestingLibrary;
open TestHelpers;

describe("Formidable", () => {
  test("Component renders", () => {
    renderApp() |> container |> expect |> toMatchSnapshot
  });

  describe("Field (example: email)", () => {
    let wrongEmail = "foobar.com";

    let validEmail = "foo@bar.com";

    let email = Input.make("email");

    test("Pristine status", () => {
      let app = renderApp();

      email.expectToMatchSnapshot(app);
    });

    test("Change focus status - focus", () => {
      let app = renderApp();

      app |> email.input |> FireEvent.focus;

      email.expectToMatchSnapshot(app);
    });

    test(
      "Change focus status and trigger required error - focus then blur", () => {
      let app = renderApp();

      app |> email.input |> FireEvent.focus;

      app |> email.input |> FireEvent.blur;

      email.expectToMatchSnapshot(app);
    });

    test("Email format error", () => {
      let app = renderApp();

      app
      |> email.input
      |> FireEvent.input(~eventInit={
                           "target": {
                             "value": wrongEmail,
                           },
                         });

      email.expectToMatchSnapshot(app);
    });

    test("Updates value", () => {
      let app = renderApp();

      app
      |> email.input
      |> FireEvent.input(~eventInit={
                           "target": {
                             "value": validEmail,
                           },
                         });

      email.expectToMatchSnapshot(app);
    });

    test("Submit errors", () => {
      let app = renderApp();

      app |> submitButton |> FireEvent.click;

      email.expectToMatchSnapshot(app);
    });

    test("Submit and reset", () => {
      let app = renderApp();

      app |> submitButton |> FireEvent.click;

      app |> resetButton |> FireEvent.click;

      email.expectToMatchSnapshot(app);
    });

    test("Submit with errors", () => {
      let app = renderApp();

      app
      |> email.input
      |> FireEvent.input(~eventInit={
                           "target": {
                             "value": wrongEmail,
                           },
                         });

      app |> submitButton |> FireEvent.click;

      email.expectToMatchSnapshot(app);
    });

    test("On focus listener - no listener", () => {
      let (spy, _) = Handler.make(const("focused"));

      let app = renderApp();

      app |> email.input |> FireEvent.focus;

      Handler.expectToMatchSnapshot(spy);
    });

    test("On focus listener - no event", () => {
      let (spy, onFocus) = Handler.make(const("focused"));

      ignore(renderApp(~onFocus, ()));

      Handler.expectToMatchSnapshot(spy);
    });

    test("On focus listener - wrong event", () => {
      let (spy, onFocus) = Handler.make(const("focused"));

      let app = renderApp(~onFocus, ());

      app |> email.input |> FireEvent.blur;

      Handler.expectToMatchSnapshot(spy);
    });

    test("On focus listener", () => {
      let (spy, onFocus) = Handler.make(const("focused"));

      let app = renderApp(~onFocus, ());

      app |> email.input |> FireEvent.focus;

      Handler.expectToMatchSnapshot(spy);
    });

    test("On blur listener - no listener", () => {
      let (spy, _) = Handler.make(const("blurred"));

      let app = renderApp();

      app |> email.input |> FireEvent.blur;

      Handler.expectToMatchSnapshot(spy);
    });

    test("On blur listener - no event", () => {
      let (spy, onBlur) = Handler.make(const("blurred"));

      ignore(renderApp(~onBlur, ()));

      Handler.expectToMatchSnapshot(spy);
    });

    test("On blur listener - wrong event", () => {
      let (spy, onBlur) = Handler.make(const("blurred"));

      let app = renderApp(~onBlur, ());

      app |> email.input |> FireEvent.focus;

      Handler.expectToMatchSnapshot(spy);
    });

    test("On blur listener", () => {
      let (spy, onBlur) = Handler.make(const("blurred"));

      let app = renderApp(~onBlur, ());

      app |> email.input |> FireEvent.blur;

      Handler.expectToMatchSnapshot(spy);
    });

    test("On change listener", () => {
      let (spy, onChange) = Handler.make(value => "changed: " ++ value);

      let app = renderApp(~onChange, ());

      app
      |> email.input
      |> FireEvent.input(~eventInit={
                           "target": {
                             "value": "foo@",
                           },
                         });

      app
      |> email.input
      |> FireEvent.input(~eventInit={
                           "target": {
                             "value": "bar.",
                           },
                         });

      app
      |> email.input
      |> FireEvent.input(~eventInit={
                           "target": {
                             "value": "com",
                           },
                         });

      Handler.expectToMatchSnapshot(spy);
    });
  });
});
