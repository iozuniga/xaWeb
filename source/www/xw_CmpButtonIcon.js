/*
 * Proyect: XailerWeb framework
 * File: CmpButtonIcon.js
 * Description: button + icon component
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 * Copyright https://www.youtube.com/@dcode-software
 */

export class CmpButtonIcon extends HTMLElement {
   constructor() {
   super();
   this.attachShadow({mode: "open"});
   }

   connectedCallback() {
      const icon = this.getAttribute("data-icon");
      const variant = this.getAttribute("data-variant");
      const type = this.getAttribute("type");

      this.shadowRoot.innerHTML = `
         <style>${css}</style>
         <button class="button">
            ${
               (icon && `<ion-icon name="${icon}" style="vertical-align: text-bottom;"> </ion-icon>`) || ""
            }
            <span class="label">
               <slot></slot>
            </span>
         </button>
      `;

      if (variant) {
         this.shadowRoot.querySelector(".button").classList.add(`variant-${variant}`);
      }
      if (type) {
         this.shadowRoot.querySelector(".button")[ "type" ] = "${type}";
      }
   }
}

const css = `
   .button {
      font-size: 1rem;
      display: inline-block;
      align-items: center;
      justify-content: center;
      gap: 8px;
      padding: 0.6em 1em;
      margin: 0.5em 0em;
      background-color: var(--primary-color, #166df7);
      border-radius: 0.5em;
      border: 0.125em solid transparent;
      color: var(--primary-text-color, #ffffff);
      outline: none;
      cursor: pointer;
      opacity: 1.0;
      transition: 0.3s;
   }


  .button:active {
      transform: scale(0.98);
   }

   .button:hover {
      opacity: 0.6;
      transition: 0.3s;
   }

   .variant-success {
      background-color: #3cba02;
      color: #ffffff;
   }

   .variant-error {
      background-color: #dd050c;
      color: #ffffff;
   }

`;