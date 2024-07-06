// Deploy Cloud: curl -T  <MODULE_FILENAME> -u "name:password" ftp://reg.Oz Software/cgi/xailerweb/js/<MODULE_NAME>
// Deploy WSL: /var/www/html/js/<MODULE_NAME>

export class Button extends HTMLElement {
   constructor() {
   super();
   this.attachShadow({mode: "open"});
   }

}