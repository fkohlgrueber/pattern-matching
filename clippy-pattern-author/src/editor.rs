use yew::{html, Callback, Component, ComponentLink, Html, Renderable, ShouldRender};

pub struct Editor {
    oninput: Option<Callback<(String)>>,
    value: String,
}

pub enum Msg {
    GotInput(String),
}

#[derive(PartialEq, Clone)]
pub struct Props {
    pub value: String,
    pub oninput: Option<Callback<(String)>>,
}

impl Default for Props {
    fn default() -> Self {
        Props {
            value: "".to_string(),
            oninput: None
        }
    }
}

impl Component for Editor {
    type Message = Msg;
    type Properties = Props;

    fn create(props: Self::Properties, _: ComponentLink<Self>) -> Self {
        Editor {
            oninput: props.oninput,
            value: props.value,
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::GotInput(s) => {
                if let Some(ref mut callback) = self.oninput {
                    callback.emit(s);
                }
            }
        }
        false
    }

    fn change(&mut self, props: Self::Properties) -> ShouldRender {
        self.oninput = props.oninput;
        self.value = props.value;
        true
    }
}

impl Renderable<Editor> for Editor {
    fn view(&self) -> Html<Self> {
        html! {
            <textarea rows=25,
                    oninput=|e| Msg::GotInput(e.value),
                    style="width:45%;", 
                    /*placeholder="output",*/ >
                { &self.value }
            </textarea>
        }
    }
}
