/*
 * Proyecto: xaWeb framework
 * Fichero: xa_backpack.js
 * Descripción: JavaScript core functions for xaWeb
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Note: This source code can only be used on xaWeb projects
*/

function xa_baseUri() {
   return window.location.href.split('?')[0];
}

function xa_paramsUri() {
   return window.location.href.split('?')[1];
}

function xa_idVisible( cId, lShow ) {
   const ele = document.getElementById(cId);
   if (ele) {
      if (typeof lShow === "undefined") {
         lShow = ele.hidden;
         }
      ele.hidden = !lShow;
   }
}

function xa_getCookie(name) {
   let matches = document.cookie.match(new RegExp(
   "(?:^|; )" + name.replace(/([\.$?*|{}\(\)\[\]\\\/\+^])/g, '\\$1') + "=([^;]*)"));
   if (matches) {
      return decodeURIComponent(matches[1]);
   } else {
      console.warn( "Cookie not found: " + name );
      return undefined;
   }
}

function xa_setCookie(name, value, options = {}) {
  if (value === undefined) {
    console.warn( "Cookie value undefined: " + name );
    return false;
  }

  if (value.length > 1024) {
    console.warn( "excessive size for cookie: " + name );
    return false;
  }

  if (options.expires instanceof Date) {
    options.expires = options.expires.toUTCString();
  }

  let updatedCookie = encodeURIComponent(name) + "=" + encodeURIComponent(value);

  for (let optionKey in options) {
    updatedCookie += "; " + optionKey;
    let optionValue = options[optionKey];
    if (optionValue !== true) {
      updatedCookie += "=" + optionValue;
    }
  }
  document.cookie = updatedCookie;
  return true;
}

function xa_deleteCookie(name) {
   xa_setCookie(name, "", {'max-age': -1});
}

function xa_saveState() {
   let s = {};
   for (const [id, obj] of xa_mapProp) {
      for(const[key, val] of Object.entries(obj)) {
         const ele = document.getElementById( id );
         const nam = key.substring(1);
         const typ = typeof ele[nam];
         if (typ !== "undefined") {
           if (typ === "string" && ele[nam].length > 512) {
             obj[key] = null;
           } else {
             obj[key] = ele[nam];
           }
         }
      }
      s[id] = obj;
   }
   s['lastLocation'] = xa_paramsUri();
   xa_setCookie("state", JSON.stringify(s), {'max-age': 60});
}

function xa_saveSession() {
   let s = {};
   for (const [id, obj] of xa_mapProp) {
      let nob = {};
      let ele = document.getElementById(id);
      for(const[key, val] of Object.entries(obj)) {
         if (val == null) {
            nob[key] = ele[key];
         }
      }
      if (Object.keys(nob).length > 0) {
         s[id] = nob;
      }
   }
   if (Object.keys(s).length > 0) {
      sessionStorage.setItem( encodeURIComponent(window.location), JSON.stringify(s) );
   }
   else {
      sessionStorage.removeItem( encodeURIComponent(window.location));
   }
}

function xa_stateValue(id, name) {
   return xa_mapProp.get(id)[name];
}

function xa_restoreSession() {
   const j = sessionStorage.getItem( encodeURIComponent(window.location) );
   if (j) {
      let s = JSON.parse(j);
      for (const [id, obj] of s) {
         const ele = document.getElementById( id );
         if (ele) {
            for (const[key, val] of Object.entries(obj)) {
               ele[key] = val;
            }
         }
      }
   }
}

function xa_b64toUnicode(str) {
    return decodeURIComponent(atob(str).split('').map(function(c) {
        return '%' + ('00' + c.charCodeAt(0).toString(16)).slice(-2);
    }).join(''));
}

function xa_filterTableByColumn() {
   document.querySelectorAll(".table-search-input").forEach(inputField => {
      const tableRows = inputField.closest("table").querySelectorAll("tbody > tr");
      const headerCell = inputField.closest("td");
      const otherHeaderCells = headerCell.closest("tr").children;
      const columnIndex = Array.from(otherHeaderCells).indexOf(headerCell);
      const searchableCells = Array.from(tableRows).map(
         (row) => row.querySelectorAll("td")[columnIndex]
      );
      inputField.value = "";

      inputField.addEventListener("input", () => {
         const searchQuery = inputField.value.toLowerCase();
         const searchableCells = Array.from(tableRows).map(
         (row) => row.querySelectorAll("td")[columnIndex]
         );

         for (const tableCell of searchableCells) {
            const row = tableCell.closest("tr");
            const value = tableCell.textContent.toLowerCase().replace(",", "");
            row.style.visibility = null;
            if (value.search(searchQuery) === -1) {
               row.style.visibility = "collapse";
            }
         }
      });
   });
}

function xa_sortTableByColumn(table, column, asc = true) {
   const dirModifier = asc ? 1 : -1;
   const tBody = table.tBodies[0];
   const rows = Array.from(tBody.querySelectorAll("tr"));

   const sortedRows = rows.sort((a, b) => {
      const aColText = a.querySelector(`td:nth-child(${column + 1})`)?.textContent.trim() ?? "";
      const bColText = b.querySelector(`td:nth-child(${column + 1})`)?.textContent.trim() ?? "";

      return aColText > bColText ? (1 * dirModifier) : (-1 * dirModifier);
   });

   while (tBody.firstChild) {
      tBody.removeChild(tBody.firstChild);
   }

   tBody.append(...sortedRows);

   table.querySelectorAll("th").forEach(th => th.classList.remove("th-sort-asc", "th-sort-desc"));
   table.querySelector(`th:nth-child(${column + 1})`).classList.toggle("th-sort-asc", asc);
   table.querySelector(`th:nth-child(${column + 1})`).classList.toggle("th-sort-desc", !asc);
}

function xa_sortTableListen() {
   document.querySelectorAll(".table-sortable th").forEach(headerCell => {
      headerCell.addEventListener("click", () => {
         const tableElement = headerCell.parentElement.parentElement.parentElement;
         const headerIndex = Array.prototype.indexOf.call(headerCell.parentElement.children, headerCell);
         const currentIsAscending = headerCell.classList.contains("th-sort-asc");

         xa_sortTableByColumn(tableElement, headerIndex, !currentIsAscending);
      });
   });
}

function xa_disableTableControl(btn, value) {
   if (!btn.dataset.syncro) {
      btn = btn.querySelector('a');
   }
   if (btn) {
      if (btn.nodeName == "BUTTON") {
         btn.disabled = value;
      } else {
         btn.classList.remove("disabled");
      }
      const syncro = document.getElementById(btn.dataset.syncro);
      if (syncro) {
         syncro.classList.remove("disabled");
      }
   }
}

function xa_rowClickOnTableListen() {
   document.querySelectorAll(".table-recno tbody").forEach(tbody => {
      tbody.addEventListener("click", function(e) {
         const row = e.target.parentElement;
         if (row.matches("tr")) {
            const table = row.parentElement.parentElement;
            const cells = [];
            tbody.querySelectorAll("tr").forEach(tr => tr.classList.remove("row-selected"));
            row.classList.add("row-selected");
            xa_disableTableControl(document.getElementById(table.dataset.btnEdit), false);
            xa_disableTableControl(document.getElementById(table.dataset.btnDel), false);
            Array.from(row.children).forEach((td) => {
               cells.push(td.textContent);
            });

            table.dataset.recno = cells[0];
            table.dataset.fields = cells;

            const evValue = table.getAttribute( "onrowclick" );
            if (evValue && evValue.startsWith('xa_fetch')) {
               eval(`${evValue}`);
            } else {
               table.dispatchEvent(new CustomEvent("rowclick",{
                  detail: { recno: cells[0], fields: cells}
                  }));
            }
         }
      });
   });
}

function xa_loadTable(table, rows) {
   const tableBody = table.querySelector("tbody");
   tableBody.innerHTML = "";

   rows.forEach(row => {
      const rowElement = document.createElement("tr");

      row.forEach(cellText => {
         const cellElement = document.createElement("td");
         cellElement.textContent = cellText;
         rowElement.appendChild(cellElement);
         });
      tableBody.appendChild(rowElement);
   });
   xa_filterTableByColumn();
   table.querySelectorAll("th").forEach(th => th.classList.remove("th-sort-asc", "th-sort-desc"));
   xa_disableTableControl(document.getElementById(table.dataset.btnEdit), true);
   xa_disableTableControl(document.getElementById(table.dataset.btnDel), true);
}

function xa_getSelectedRecord(table) {
   const row = table.querySelector(".row-selected");
   if (row) {
      const flds = [];
      const record = {};
      let n = 0;
      Array.from(table.querySelectorAll("th")).forEach((th) => {
         flds.push(th.dataset.field);
      });
      Array.from(row.children).forEach((td) => {
         record[flds[n++]] = td.textContent;
      });
      return record;
   }
}

function xa_fetchTableSelectedRow( targetId, sourceId, cb, url, options) {
   const ele = document.getElementById(targetId);
   const rec = xa_getSelectedRecord(ele);
   if (rec) {
      xa_fetch(targetId, sourceId, cb, url, options, rec);
   }
}

function xa_addRowTable(table, data) {
   const row = document.createElement("tr");
   const flds = [];
   let n = 0;
   if (Object.keys(data).length > 0) {
      Array.from(table.querySelectorAll("th")).forEach((th) => {
         flds.push(th.dataset.field);
      });
      flds.forEach( field => {
         const cell = document.createElement("td");
         cell.textContent = data[field];
         row.appendChild(cell);
      });
      const newRow = table.querySelector("tbody").appendChild(row);
      table.querySelectorAll("tr").forEach(tr => tr.classList.remove("row-selected"));
      xa_disableTableControl(document.getElementById(table.dataset.btnEdit), false);
      xa_disableTableControl(document.getElementById(table.dataset.btnDel), false);
      if (table.classList.contains( "table-filterable" )) {
         xa_filterTableByColumn();
      }
      newRow.classList.add("row-selected");
      newRow.scrollIntoView({
         behavior: 'smooth',
         block: 'end',
         inline: 'nearest'
      });
   }
}

function xa_saveRowTable(table, data) {
   const row = table.querySelector(".row-selected");
   if (row && (Object.keys(data).length > 0)) {
      const flds = [];
      let n = 0;
      Array.from(table.querySelectorAll("th")).forEach((th) => {
         flds.push(th.dataset.field);
      });
      Array.from(row.children).forEach((td) => {
         if (data.hasOwnProperty(flds[n])) {
            td.textContent = data[flds[n]];
         }
         n++;
      });
   }
}

function xa_postEditTable(table, data) {
   if (Object.keys(data).length > 0) {
      if (data.hasOwnProperty("error")) {
         if (data.hasOwnProperty("errorFunction")) {
            eval(`${data.errorFunction}`)(data.error);
         } else {
            alert(data.error);
         }
      } else {
         if (data.append_mode === "true") {
            xa_addRowTable(table, data)
         } else {
            xa_saveRowTable(table, data)
         }
      }
   }
}

function xa_delRowTable(table, result) {
   if (result.pass) {
      const row = table.querySelector(".row-selected");
      let next = row.nextElementSibling;
      if (!next )
         next = row.previousElementSibling;
      if (row) {
         table.deleteRow(row.rowIndex);
      }
      if (next) {
         next.classList.add("row-selected");
      } else {
         xa_disableTableControl(document.getElementById(table.dataset.btnEdit), true);
         xa_disableTableControl(document.getElementById(table.dataset.btnDel), true);
      }
   }
}

function xa_eventToObject(ev) {
   const e = {"target": ev.target.id || ev.srcElement.id,
              "isTrusted":ev.isTrusted || true,
              "altKey":ev.altKey || false,
              "ctrlKey":ev.ctrlKey || false,
              "clientX":ev.clientX || 0,
              "clientY":ev.clientY || 0};
   if (ev.detail) {
      for(const[nam, val] of Object.entries(ev.detail)) {
         e["detail-"+nam] = val;
      }
   }
   return e;
}

async function xa_fetch(targetId, sourceId, cb, url, options, cargo, params) {
   const ev  = xa_eventToObject(window.event);
   const el = document.getElementById(targetId);
   const or = document.getElementById(sourceId);
   let tp = options.headers['Content-type'];

   let dt = null;
   let re = null;
   if (cb) {
      try {
         if (typeof eval(cb) !== "function") {
            console.warn(`function ${cb} not found.`);
            return {};
         }
      } catch (exception) {
         console.warn(`Function ${cb} not found.`);
         return {};
      }
   }
   if (or) {
      or.disabled = true;
      if (or.setLoading) {
         or.setLoading(true);
      }
   }
   if (cargo) {
      xa_setCookie("cargo", JSON.stringify(cargo), {'max-age': 60});
   }
   xa_setCookie("event", JSON.stringify(ev), {'max-age': 60});
   try {
      if (params) {
         let urlParams = "";
         for(const[key, val] of Object.entries(params)) {
            const el = document.getElementById(key);
            if (el) {
               if (el.getAttribute(val)) {
                  urlParams += "&" + key + "_" + val + "=" + el[val];
               } else {
                  urlParams += "&detail-" + val + "=" + el.dataset[val];
               }
            }
         }
         if ((urlParams.length > 0) && (url.indexOf("?") < 0)) {
            urlParams = "?" + urlParams.slice(1);
         }
         url = url + urlParams;
      }

      re = await fetch(url, options);
      if (re.ok) {
         if (re.status != 200) {
            tp = "application/javascript";
         }
         try {
            switch(tp) {
            case "application/json":
               dt = await re.json();
               break;
            case "text/plain":
            case "application/javascript":
               dt = await re.text();
               break;
            case "multipart/form-data":
            case "application/x-www-form-urlencoded":
               dt = await re.formData();
               break;
            case "application/octet-stream":
               dt = await re.blob();
               break;
            default:
               dt = await re.text();
            }
         } catch (exception) {
            console.warn(exception);
            dt = null;
         }
      }
   } catch (exception) {
      console.warn(exception);
      re = null;
   }
   if (or) {
      or.disabled = false;
      if (or.setLoading) {
         or.setLoading(false);
      }
   }
   if (dt) {
      if ( tp === "application/javascript" ) {
         if (re.status === 200) {
            let s = document.createElement('script');
            s.text = dt;
            const node = document.getElementsByTagName('head')[0].appendChild(s);
            node.remove();
         } else {
            console.warn( dt );
         }
      } else {
         if (cb) {
            eval(cb)(el,dt,ev);
         } else {
            return dt;
           //console.warn('Call-back function empty');
         }
      }
   }
}

function xa_setHbData(element, hbData, value, post = false) {
   if (post) {
      if (xa_mapPost.has(element.id)) {
         xa_mapPost.get(element.id)[hbData.toLowerCase()] = value;
      } else {
         xa_mapPost.set(element.id, {});
         xa_mapPost.get(element.id)[hbData.toLowerCase()] = value;
      }
   } else {
      xa_mapProp.get(element.id)[hbData.toLowerCase()] = value;
   }
}

async function xa_getHbData(service, doc, type) {
   if (typeof doc === "undefined") {
      doc = xa_docName;
   }
   if (typeof type === "undefined") {
      type = "application/json";
   }
   const url = xa_baseUri() + "?service=" + doc + "-" + service;
   const opt = {method: "GET", mode: "cors", cache: "no-cache",
                credentials: "same-origin",
                headers: { "Content-type": type},
                redirect: "follow",
                referrerPolicy: "no-referrer",
               };
   return await xa_fetch( null, null, null, url, opt );
}

function xa_savePost(id, url) {
   if (xa_mapPost.size === 0) {
      return false;
   }
   let form = document.getElementById(id);
   if (form) {
      let input;
      for (const [id, obj] of xa_mapPost) {
         for(const[key, val] of Object.entries(obj)) {
            if (val !== null) {
               input = document.createElement('input');
               input.setAttribute('name', id + '--' + key);
               input.setAttribute('value', val);
               input.setAttribute('type', 'hidden');
               form.appendChild(input);
            }
         }
      }
   } else {
      form = document.createElement('form');
      form.method = "POST";
      let input;
      for (const [id, obj] of xa_mapPost) {
         for(const[key, val] of Object.entries(obj)) {
            if (val !== null) {
               input = document.createElement('input');
               input.setAttribute('name', id + '@' + key);
               input.setAttribute('value', val);
               input.setAttribute('type', 'hidden');
               form.appendChild(input);
            }
         }
      }
      document.body.appendChild(form);

      if (url) {
         form.action = url;
      } else {
         form.addEventListener("submit", function(e){e.preventDefault();});
      }
      form.submit();
   }
   return true;
}

function xa_runAction(element, funcName, ev) {
   if (!ev) {
      ev = window.event;
   }
   const e = xa_eventToObject(ev);
   const d = ((element && element.dataset.document) ? element.dataset.document : xa_docName);
   const p = xa_baseUri() + "?action=" + d + "-" + funcName;

   xa_setCookie("event", JSON.stringify(e), {'max-age': 60});
   xa_setCookie("session", xa_getCookie("session"), {'max-age': 60});
   xa_saveState();
   xa_saveSession();
   if (!xa_savePost(null,p)) {
      window.location = p;
   }
}

function xa_runJS(element, funcName, ev) {
   eval(`${funcName}`)(ev, element);
}

function xa_triggerEvent(name, targetId) {
   const ev = window.event;
   const el = ev.target;
   const ta = document.getElementById(targetId);
   if (ta) {
      ev.preventDefault();
      ta.dispatchEvent( new CustomEvent(name,{
                  detail: { value: el.innerText} }));
   }
}

function xa_processEvent( element, eventName, eventValue ) {
   if (eventValue.startsWith('@')) {
      element.addEventListener(eventName.substring(2), function(ev) {
         xa_runAction( element, eventValue.substring(1), ev );
      });
   } else {
      try {
         const evName = eventName.substring(2);
         if (typeof eval(`${eventValue}`) === "function") {
            element.addEventListener(eventName.substring(2), function(ev) {
               xa_runJS( element, eventValue, ev );
            });
         }
      } catch (exception) {
         console.warn(exception);
      }
   }
}

function xa_submit(e) {
   let ele = document.getElementById( e.target.name );
   if (ele) {
      xa_setCookie("session", xa_getCookie("session"), {'max-age': 60});
      xa_saveState();
      xa_saveSession();
      xa_savePost(e.target.name);
   }
   return true;
}

class xa_FormValidator {
   constructor(selector) {
      this.form = document.querySelector(selector);
      this.inputsWithErrors = new Set();
      this.inputs = new Set();
      this.onShowEvent;

      this.form['validator'] = this;

      this.form.addEventListener("submit", e => {
         e.preventDefault();
         if (!this.hasErrors) {
            if (this.form.getAttribute("action")) {
               this.form.submit();
            } else {
               const para = eval( this.form.getAttribute( "onsubmit") );
               const data = new FormData(this.form);
               const sp = new URLSearchParams();

               for (const pair of data) {
                  sp.append(pair[0], pair[1]);
               }

               if (this.form.hasAttribute("data-append")) {
                  sp.append("append_mode", this.form.dataset.append);
                }

               const doc = this.form.dataset.document;
               const opt = {method: "POST", mode: "cors", cache: "no-cache",
                            credentials: "same-origin",
                            headers: {"Content-type": para.content},
                            redirect: "follow",
                            referrerPolicy: "no-referrer",
                            body: sp,
                           };

               const url = xa_baseUri() + "?service=" + doc + "-" + para.service;
               const table = this.form.dataset.table;
               if (table) {
                  const rec = xa_getSelectedRecord(document.getElementById(table));
                  if (rec) {
                     for(const[key, val] of Object.entries(rec)) {
                        sp.append("old_"+key, val);
                     }
                  }
                  xa_fetch( table, null, "xa_postEditTable", url, opt );
               } else {
                  xa_fetch( null, null, null, url, opt, this.inputValues() );
               }

               const section = this.form.closest("x-doc-section");
               if (section) {
                  xa_showSection(section.id, false, this.form.id);
               }
            }
         } else {
            this.inputsWithErrors.forEach( (input) => {
               const errorElement = input.closest(".xa-input").querySelector(".xa-input__error");
               if (errorElement)
                  xa_fieldSetError( errorElement, input.dataset.error );
            });
         }
      });
   }

   get hasErrors() {
      return this.inputsWithErrors.size > 0;
   }

   inputValues() {
      const values = {};
      Array.from(this.form.elements).forEach(inp => {
         if (((inp.tagName == "INPUT") || (inp.type == "textarea")) && inp.id) {
            values[inp.id] = inp.value;
         }
      });
      return values;
   }

   async onShow() {
      let values = this.inputValues();
      if (Object.keys(values).length > 0) {
         values = await xa_formOnShow(values, this.onShowEvent, this.form);
         for(const[key, val] of Object.entries(values)) {
            const ele = document.getElementById(key);
            if (ele) {
               ele.value = val;
               if (xa_isMaterialize() && (ele.type == "textarea")) {
                  M.Forms.textareaAutoResize(ele);
               }
            }
         }
      }
   }

   register(selector, func) {
      const fv = this;
      const inputField = this.form.querySelector(selector);
      const errorElement = inputField.closest(".xa-input").querySelector(".xa-input__error");
      let isJsFunc;
      let execute;
      try {
         isJsFunc = (typeof eval(`${func}`) === "function");
      } catch (exception) {
         isJsFunc = false;
      }
      this.inputs.add(inputField);

      if (isJsFunc) {
         execute = async function(hideErrors = false) {
            const { pass, error } = eval(`${func}`)(inputField.value, inputField);

            inputField.dataset['error'] = error;

            if (!hideErrors) {
               xa_fieldSetError( errorElement, error );
            }

            if (!pass) {
               fv.inputsWithErrors.add(inputField);
            } else {
               fv.inputsWithErrors.delete(inputField);
            }
         };
      } else {
         execute = async function(hideErrors = false) {
            const { pass, error } = await xa_fieldValidator(inputField, func, fv.form);

            inputField.dataset['error'] = error;

            if (!hideErrors) {
               xa_fieldSetError( errorElement, error );
            } else {
               xa_fieldSetError( errorElement, "" );
            }

            if (!pass) {
               fv.inputsWithErrors.add(inputField);
            } else {
               fv.inputsWithErrors.delete(inputField);
            }
         }
      };

      inputField.addEventListener("change", () => execute());
      inputField.validate = execute;
   }

   validate(hideErrors) {
      this.inputs.forEach( (input) => {
         input.validate(hideErrors);
      });
   };

   clean() {
      this.inputs.forEach( (input) => {
         input.value = "";
         xa_fieldSetError( input, "" );
      });
   };
}

function xa_fieldSetError( element, error ) {
   element.textContent = error || "";
   let isJsFunc;
   try {
      isJsFunc = (typeof eval("xa_fieldError") === "function");
   } catch (exception) {
      isJsFunc = false;
   }
   if (isJsFunc)
      xa_fieldError(element, error);
}

async function xa_formOnShow(values, hbFunc, frm) {
   const doc = frm.dataset.document;
   let re;
   let dt;
   const url = xa_baseUri() + "?service=" + doc.toLowerCase() + "-" +
               hbFunc.toLowerCase();
   const opt  = {method: "GET", mode: "cors", cache: "no-cache",
                  credentials: "same-origin",
                  headers: { "Content-type": "application/json"},
                  redirect: "follow",
                  referrerPolicy: "no-referrer",
                };
   xa_setCookie("cargo", JSON.stringify(values), {'max-age': 60});
   try {
      re = await fetch(url, opt);
      if (re.ok) {
         try {
            if (re.status != 200) {
               dt = await re.text();
               console.warn( dt );
               return {error: "Run-time error on " + hbFunc + ". Check the console."};
            } else {
               dt = await re.json();
            }
            return dt;
         } catch (exception) {
            console.warn(exception);
            return {error: "Invalid JSON return on " + hbFunc};
         }
      } else {
         console.warn(exception);
         return {error: "Invalid URL on " + hbFunc + " [" + url + "]"};
      }
   } catch (exception) {
      console.warn(exception);
      return {error: "Invalid URL on " + hbFunc + " [" + url + "]"};
   }

   return {};
}

async function xa_fieldValidator(inputField, hbFunc, frm) {
   const doc = frm.dataset.document;
   let re;
   let dt;
   let url  = xa_baseUri() + "?service=" + doc.toLowerCase() + "-" +
              hbFunc.toLowerCase() +
              '&value=' + encodeURIComponent(inputField.value) +
              '&id=' + inputField.id;
   if (frm.hasAttribute("data-append")) {
      url = url + "&append_mode=" + frm.dataset.append;
   } else {
      url = url + "&append_mode=false";
    }
   const opt  = {method: "GET", mode: "cors", cache: "no-cache",
                  credentials: "same-origin",
                  headers: { "Content-type": "application/json"},
                  redirect: "follow",
                  referrerPolicy: "no-referrer",
                };

   try {
      re = await fetch(url, opt);
      if (re.ok) {
         try {
            if (re.status != 200) {
               dt = await re.text();
               console.warn( dt );
               return {pass: false, error: "Run-time error on " + hbFunc + ". Check the console."};
            } else {
               dt = await re.json();
            }
            return dt;
         } catch (exception) {
            console.warn(exception);
            return {pass: false, error: "Invalid JSON return on " + hbFunc};
         }
      } else {
         console.warn(exception);
         return {pass: false, error: "Invalid URL on " + hbFunc + " [" + url + "]"};
      }
   } catch (exception) {
      console.warn(exception);
      return {pass: false, error: "Invalid URL on " + hbFunc + " [" + url + "]"};
   }

   return { pass: true };
}

function xa_showSection(idSection, show, idForm, idTable, append) {
   const ele = document.getElementById( idSection );
   const frm = document.getElementById(idForm);

   if (ele) {
      if (show) {
         if (frm) {
            if (idTable) {
               const tbl = document.getElementById(idTable);
               if (tbl) {
                  const rec = xa_getSelectedRecord(tbl);
                  frm.dataset['append'] = append;
                  frm.querySelectorAll('input[data-field]').forEach( inp => {
                     if (rec && !append) {
                        inp.value = rec[inp.dataset.field];
                        if (inp.dataset.disabledOnEdit) {
                           inp.readOnly = true;
                        }
                     } else {
                        inp.value = "";
                        inp.readOnly = false;
                     }
                  });
               }
            }
            frm.validator.validate(true);
         }
         ele.style.display="block";
      } else {
         if (frm) {
            frm.validator.clean();
         }
         ele.style.display="none";
      }
   }
}

const xa_observer = new IntersectionObserver((entries, observer) => {
  entries.forEach((entry) => {
    const fv = entry.target.validator;
    if (fv && fv.onShowEvent) {
      fv.onShow();
      }
  });
  },{rootMargin: "0px", threshold: 1.0,});

function xa_isMaterialize() {
   return !(typeof M === 'undefined' || M === null);
}