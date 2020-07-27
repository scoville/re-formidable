switch (ReactDOM.querySelector("#root")) {
| Some(root) => ReactDOM.render(<App />, root)
| None => Js.log("Couldn't find root element")
};
