/*
 * Proyecto: xaWeb framework
 * Fichero: CmpButtonSpinner.js
 * Descripción: button + spinner component
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Referred to https://www.youtube.com/@dcode-software
 */

export class CmpButtonSpinner extends HTMLElement {
   constructor() {
   super();
   this.attachShadow({mode: "open"});
   }

  setLoading(state) {
    const btn = this.shadowRoot.querySelector(".button");
    if (typeof state === "undefined") {
      btn.classList.toggle("button--loading");
    } else {
      if (state) {
        btn.classList.add("button--loading");
      } else {
        btn.classList.remove("button--loading");
      }
    }
  }

   connectedCallback() {
      const type = this.getAttribute("type");
      const spin = this.classList.contains("button--loading");

      this.shadowRoot.innerHTML = `
         <style>${css}</style>
         <button class="button">
            <span class="button__text">
               <slot></slot>
            </span>
         </button>
      `;

      this.setLoading(spin);

      if (type) {
         this.shadowRoot.querySelector(".button")[ "type" ] = "${type}";
      }

   }
}

const css = `
.button {
  display: inline-block;
  font-size: 1rem;
  position: relative;
  padding: 0.6em 1em;
  margin: 0.5em 0em;
  background: var(--primary-color, #333333);
  color: var(--primary-text-color, #ffffff);
  border: 0.125em solid transparent;
  border-radius: 0.5em;
  cursor: pointer;
  outline: none;
  transition: 0.3s;

}

.button:hover {
   opacity: 0.6;
   transition: 0.3s;
}

.button:active {
  transform: scale(0.98);
}

.button__text {
  color: var(--primary-text-color, #ffffff);
  transition: all 0.2s;
}

.button--loading .button__text {
  visibility: hidden;
  opacity: 0;
}

.button--loading::after {
  content: "";
  position: absolute;
  width: 16px;
  height: 16px;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  margin: auto;
  border: 4px solid transparent;
  border-top-color: var(--primary-text-color, #ffffff);;
  border-radius: 50%;
  animation: button-loading-spinner 1s ease infinite;
}

@keyframes button-loading-spinner {
  from {
    transform: rotate(0turn);
  }

  to {
    transform: rotate(1turn);
  }
}
`;