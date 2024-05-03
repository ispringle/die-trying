import { html } from "/js/yhtml.js";

class ShadowElement extends HTMLElement {
  constructor() {
    super();
  }

  template() {
    this.shadow = this.attachShadow({ mode: "open" });
    const template = document.createElement("template");
    template.innerHTML = this.render();
    this.shadowRoot.appendChild(template.content.cloneNode(true));
  }
  render() {}
}

export { html, ShadowElement };
