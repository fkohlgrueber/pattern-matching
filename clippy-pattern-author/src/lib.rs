use yew::{html, Component, ComponentLink, Html, Renderable, ShouldRender};

mod clippy_pattern_author;
mod editor;
use editor::Editor;

pub struct Model {
    result: String,
}

pub enum Msg {
    GotInput(String),
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, _: ComponentLink<Self>) -> Self {
        Model {
            result: "".to_string(),
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::GotInput(s) => {
                self.result = clippy_pattern_author::calc_pattern_for_syntax(&s);
            }
        }
        true
    }
}

impl Renderable<Model> for Model {
    fn view(&self) -> Html<Self> {
        html! {
            <div>
                <Editor: oninput=|e| Msg::GotInput(e), />
                <Editor: value={ &self.result }, />
            </div>
        }
    }
}