// Deploy Cloud: curl -T  <MODULE_FILENAME> -u "name:password" ftp://reg.Oz Software/cgi/xailerweb/js/<MODULE_NAME>
// Deploy WSL: /var/www/html/js/<MODULE_NAME>

export class Button extends HTMLElement {
   constructor() {
   super();
   this.attachShadow({mode: "open"});
   }

   connectedCallback() {
      const icon = this.getAttribute("data-icon");
      const variant = this.getAttribute("data-variant");

      this.shadowRoot.innerHTML = `
         <style>${css}</style>
         <button class="button">
            ${
               (icon && `<ion-icon name="${icon}"></ion-icon>`) || ""
            }
            <span class="label">
               <slot></slot>
            </span>
         </button>
      `;

      if (variant) {
         this.shadowRoot.querySelector(".button").classList.add(`variant-${variant}`);
      }
   }
}

const css = `
   .button {
      display: inline-block;
      align-items: center;
      justify-content: center;
      padding: 8px 16px;
      gap: 8px;
      margin: 2px 0px;

      background-color: #166df7;
      border-radius: 5px;
      color: #ffffff;
      border: none;
      outline: none;
      cursor: pointer;

      font-family: 'Lexend', sans-serif;

      opacity: 1.0;
      transition: 0.3s;
   }

   .button:hover {
      opacity: 0.5;
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