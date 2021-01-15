open Components

module Values = {
  @lenses
  type t = {
    age: string,
    name: string,
    password: string,
    passwordConfirm: string,
    email: string,
  }

  let init = {
    age: "",
    name: "",
    password: "",
    passwordConfirm: "",
    email: "",
  }
}

// In this example we define our form module at the top level using a Functor,
// but in some cases you can simply use the hook provided by Formidable
// or define your form at runtime using the make function
// All the above solutions come with pros and cons
module Form = Formidable.Make(Values, I18n.Error)

open Validations

// Validations can be defined in an other module, and re-used easily
let requiredValidations = [(#onChange, required)]

let emailValidations = [(#onChange, required->compose(email))]

let passwordConfirmValidations = [(#onChange, equals(Values.password))]

module Child = {
  @react.component
  let make = (~onInputBlur=?, ~onInputChange=?, ~onInputFocus=?) => {
    let {Formidable.Hook.reset: reset, state: {values: {Values.email: email}}, submit} = Form.use(
      ~onSuccess=values => Js.log2("Success: ", values),
      ~onError=(values, errors) => Js.log3("Error: ", errors, values),
      (),
    )

    <Form preventDefault=true onSubmit=submit>
      <div> {("Email address: " ++ email)->React.string} </div>
      <div> {"Form"->React.string} </div>
      <Form.Field
        name="email"
        onBlur=?onInputBlur
        onChange=?onInputChange
        onFocus=?onInputFocus
        label="Email"
        lens=Values.email
        validations=emailValidations>
        {field => <TextInput field />}
      </Form.Field>
      <Form.Field
        name="password"
        onBlur=?onInputBlur
        onChange=?onInputChange
        onFocus=?onInputFocus
        label="Password"
        lens=Values.password
        validations=requiredValidations>
        {field => <TextInput field />}
      </Form.Field>
      <Form.Field
        name="password-confirm"
        onBlur=?onInputBlur
        onChange=?onInputChange
        onFocus=?onInputFocus
        label="Password confirm"
        lens=Values.passwordConfirm
        validations=passwordConfirmValidations>
        {field => <TextInput field />}
      </Form.Field>
      <Form.Field
        name="name"
        onBlur=?onInputBlur
        onChange=?onInputChange
        onFocus=?onInputFocus
        label="Name"
        lens=Values.name
        validations=requiredValidations>
        {field => <TextInput field />}
      </Form.Field>
      <Form.Field
        name="age"
        onBlur=?onInputBlur
        onChange=?onInputChange
        onFocus=?onInputFocus
        label="Age"
        lens=Values.age>
        {field => <TextInput field />}
      </Form.Field>
      <Test id="submit"> <button type_="submit"> {"Submit"->React.string} </button> </Test>
      <Test id="reset">
        <button type_="button" onClick={Formidable.Events.handle(reset)}>
          {"Reset"->React.string}
        </button>
      </Test>
    </Form>
  }
}

@react.component
let make = (~onInputBlur=?, ~onInputChange=?, ~onInputFocus=?) =>
  <Form.Provider>
    <Form.Consumer>
      {({values}) =>
        ("Email address' length: " ++ Int.toString(String.length(values.email)))->React.string}
    </Form.Consumer>
    <Child ?onInputBlur ?onInputChange ?onInputFocus />
  </Form.Provider>
