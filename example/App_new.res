// open Components

// module Values = {
//   @lenses
//   type t = {
//     age: string,
//     name: string,
//     password: string,
//     passwordConfirm: string,
//     email: string,
//   }

//   let init = {
//     age: "",
//     name: "",
//     password: "",
//     passwordConfirm: "",
//     email: "",
//   }
// }

// // It's pretty easy to have a generic form maker
// // for when a the same value set and errors are used in different context
// let makeForm = Formidable.make(~values=module(Values), ~error=module(I18n.Error))

// // In this example, we'll use the hook contained in the module so we need to define it at the top level
// // In many cases, it's not needed, especially thanks to the Consumer component
// module Form = (
//   val makeForm(~onSubmit=_values => (), ~onSubmitError=(_values, _errors) => (), ())
// );

// open Validations
// // Validations can be defined in an other module, and re-used easily
// let requiredValidations = list{(#onChange, required)}

// let emailValidations = list{(#onChange, required->compose(email))}

// let passwordConfirmValidations = list{(#onChange, equals(Values.password))}

// module Child = {
//   @react.component
//   let make = (~onInputBlur=?, ~onInputChange=?, ~onInputFocus=?) => {
//     // Most of the time, the hook will be useless
//     let {Formidable.Hook.reset: reset, state: {values: {Values.email: email}}} = Form.use()

//     <Form preventDefault=true>
//       <div> {("Email address: " ++ email)->React.string} </div>
//       <div> {"Form"->React.string} </div>
//       <Form.Field
//         name="email"
//         onBlur=?onInputBlur
//         onChange=?onInputChange
//         onFocus=?onInputFocus
//         label="Email"
//         lens=Values.email
//         validations=emailValidations>
//         {field => <TextInput field />}
//       </Form.Field>
//       <Form.Field
//         name="password"
//         onBlur=?onInputBlur
//         onChange=?onInputChange
//         onFocus=?onInputFocus
//         label="Password"
//         lens=Values.password
//         validations=requiredValidations>
//         {field => <TextInput field />}
//       </Form.Field>
//       <Form.Field
//         name="password-confirm"
//         onBlur=?onInputBlur
//         onChange=?onInputChange
//         onFocus=?onInputFocus
//         label="Password confirm"
//         lens=Values.passwordConfirm
//         validations=passwordConfirmValidations>
//         {field => <TextInput field />}
//       </Form.Field>
//       <Form.Field
//         name="name"
//         onBlur=?onInputBlur
//         onChange=?onInputChange
//         onFocus=?onInputFocus
//         label="Name"
//         lens=Values.name
//         validations=requiredValidations>
//         {field => <TextInput field />}
//       </Form.Field>
//       <Form.Field
//         name="age"
//         onBlur=?onInputBlur
//         onChange=?onInputChange
//         onFocus=?onInputFocus
//         label="Age"
//         lens=Values.age>
//         {field => <TextInput field />}
//       </Form.Field>
//       <Test id="submit"> <button type_="submit"> {"Submit"->React.string} </button> </Test>
//       <Test id="reset">
//         <button type_="button" onClick={Formidable.Events.handle'(reset)}>
//           {"Reset"->React.string}
//         </button>
//       </Test>
//     </Form>
//   }
// }

// @react.component
// let make = (~onInputBlur=?, ~onInputChange=?, ~onInputFocus=?) => {
//   <Form.Provider>
//     <Form.Consumer>
//       {({state: {values}}) =>
//         ("Email address' length: " ++ Int.toString(String.length(values.email)))->React.string}
//     </Form.Consumer>
//     <Child ?onInputBlur ?onInputChange ?onInputFocus />
//   </Form.Provider>
// }

