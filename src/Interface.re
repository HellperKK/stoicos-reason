/* State declaration */
type content = {
  code: string,
  text: string,
};

/* Action declaration */
type action =
  | Update(string)
  | Click
  | Clear;

/* Component template declaration.
   Needs to be **after** state and action declarations! */
let component = ReasonReact.reducerComponent("Example");

/* greeting and children are props. `children` isn't used, therefore ignored.
   We ignore it by prepending it with an underscore */
let make = (_children) => {
  /* spread the other default fields of component here and override a few */
  ...component,

  initialState: () => {code: "", text:""},

  /* State transitions */
  reducer: (action, state) =>
    switch (action) {
    | Update(txt) => ReasonReact.Update({...state, code:txt })
    | Click => ReasonReact.Update({...state, text: state.text ++ Main.interpete(state.code)})
    | Clear => ReasonReact.Update({...state, text:""})
    },

  render: self => {
    <div>
      <textarea onChange=(_event => self.send(Update(ReactEvent.Form.target(_event)##value)))>
      </textarea>
      <br/>
      <button onClick=(_event => self.send(Click))>
        (ReasonReact.string("Interpret"))
      </button>
      <button onClick=(_event => self.send(Clear))>
        (ReasonReact.string("Clear"))
      </button>
      <br/>
      <p>(ReasonReact.string(self.state.text))</p>
    </div>;
  },
};
