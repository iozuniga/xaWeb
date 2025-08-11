/*
 * Proyecto: xaWeb framework
 * Fichero: xa_CmpNumericKeypad.js
 * Descripción: Numeric keypad
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

export class CmpNumericKeypad extends HTMLElement {
   constructor() {
   super();
   this.attachShadow({mode: "open"});
   }

   connectedCallback() {
      const dot = this.getAttribute("data-dot");
      const edi = document.getElementById(this.getAttribute("data-edit"));
      const zero = "key" + (dot ? "" : " key--zero");

      this.shadowRoot.innerHTML = `
         <style>${css}</style>
         <div class="container">
            <button class="key">7</button>
            <button class="key">8</button>
            <button class="key">9</button>
            <button class="key">4</button>
            <button class="key">5</button>
            <button class="key">6</button>
            <button class="key">1</button>
            <button class="key">2</button>
            <button class="key">3</button>
            ${
               (dot && `<button class="key">.</button>`) || ""
            }
            <button class="${zero}">0</button>
            <button class="key key--del">
               <ion-icon name="backspace" style="vertical-align:text-bottom;" ></ion-icon>
            </button>
         </div>
      `;

      if (edi) {
         this.shadowRoot.querySelectorAll(".key").forEach( btn => {
            btn.addEventListener("click", (e) => {
               if (e.currentTarget.classList.contains("key--del")) {
                  edi.value = edi.value.substring(0, edi.value.length -1);
               } else {
                  edi.value += e.currentTarget.textContent;
               }
            });
         });
      }
   }
}

const css = `
   .container {
      --gap: 1em;
      border: var(--gap) solid transparent;
      color: white;
      display: grid;
      grid-template-columns: auto auto auto;
      grid-gap: var(--gap);
      font-size: 2vmin;
      align-items: center;
   }

   .key {
      border-radius: 0.2em;
      border: none;
      font-size: 5em;
      text-align: center;
      background-color: var(--primary-color, #166df7);
      color: var(--primary-text-color, #ffffff);
      padding: 0.2em 0.2em;
      opacity: 1.0;
      transition: 0.3s;
   }

  .key:active {
      transform: scale(0.98);
   }

   .key:hover {
      opacity: 0.6;
      transition: 0.3s;
   }

   .key--zero {
      grid-row-start: 4;
      grid-row-end: 4;
      grid-column-start: 1;
      grid-column-end:3;
   }

   .key--del {
      padding: 0.2em 0em;
   }

`;