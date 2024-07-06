/*
 * Proyect: XailerWeb framework
 * File: xw_backpack.js
 * Description: JavaScript core functions for XailerWeb
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 * Note: This source code can only be used on XailerWeb projects
*/

function xw_baseUri() {
   return window.location.href.split('.cgi')[0] + '.cgi';
}

function xw_idVisible( cId, lShow ) {
   const ele = document.getElementById(cId);
   if (ele) {
      if (typeof lShow === "undefined") {
         lShow = ele.hidden;
         }
      ele.hidden = !lShow;
   }
}

function xw_getCookie(name) {
   let matches = document.cookie.match(new RegExp(
   "(?:^|; )" + name.replace(/([\.$?*|{}\(\)\[\]\\\/\+^])/g, '\\$1') + "=([^;]*)"));
   if (matches) {
      return decodeURIComponent(matches[1]);
   } else {
      console.warn( "Cookie not found: " + name );
      return undefined;
   }
}

function xw_setCookie(name, value, options = {}) {
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

function xw_deleteCookie(name) {
   xw_setCookie(name, "", {'max-age': -1});
}

function xw_saveState() {
   let s = {};
   for (const [id, obj] of xw_mapProp) {
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
   if (Object.keys(s).length > 0) {
      xw_setCookie("state", JSON.stringify(s), {'max-age': 60});
   }
}

function xw_saveSession() {
   let s = {};
   for (const [id, obj] of xw_mapProp) {
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

function xw_stateValue(id, name) {
   return xw_mapProp.get(id)[name];
}

function xw_restoreSession() {
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

function xw_b64toUnicode(str) {
    return decodeURIComponent(atob(str).split('').map(function(c) {
        return '%' + ('00' + c.charCodeAt(0).toString(16)).slice(-2);
    }).join(''));
}

function xw_filterTableByColumn() {
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

function xw_sortTableByColumn(table, column, asc = true) {
   const dirModifier = asc ? 1 : -1;
   const tBody = table.tBodies[0];
   const rows = Array.from(tBody.querySelectorAll("tr"));

   const sortedRows = rows.sort((a, b) => {
      const aColText = a.querySelector(`td:nth-child(${column + 1})`).textContent.trim();
      const bColText = b.querySelector(`td:nth-child(${column + 1})`).textContent.trim();

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

function xw_sortTableListen() {
   document.querySelectorAll(".table-sortable th").forEach(headerCell => {
      headerCell.addEventListener("click", () => {
         const tableElement = headerCell.parentElement.parentElement.parentElement;
         const headerIndex = Array.prototype.indexOf.call(headerCell.parentElement.children, headerCell);
         const currentIsAscending = headerCell.classList.contains("th-sort-asc");

         xw_sortTableByColumn(tableElement, headerIndex, !currentIsAscending);
      });
   });
}

function xw_rowClickOnTableListen() {
   document.querySelectorAll(".table-recno tbody").forEach(tbody => {
      tbody.addEventListener("click", function(e) {
         const row = e.target.parentElement;
         if (row.matches("tr")) {
            const table = row.parentElement.parentElement;
            const cells = [];
            tbody.querySelectorAll("tr").forEach(tr => tr.classList.remove("row-selected"));
            row.classList.add("row-selected");
            if (table.dataset.btnEdit) {
               const btnEdit = document.getElementById(table.dataset.btnEdit);
               if (btnEdit) {
                  btnEdit.disabled = false;
               }
            }
            if (table.dataset.btnDel) {
               const btnDel = document.getElementById(table.dataset.btnDel);
               if (btnDel) {
                  btnDel.disabled = false;
               }
            }
            Array.from(row.children).forEach((td) => {
               cells.push(td.textContent);
            });

            table.dataset.recno = cells[0];
            table.dataset.fields = cells;

            const evValue = table.getAttribute( "onrowclick" );
            if (evValue.startsWith('xw_fetch')) {
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

function xw_loadTable(table, rows) {
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
   xw_filterTableByColumn();
   table.querySelectorAll("th").forEach(th => th.classList.remove("th-sort-asc", "th-sort-desc"));
   if (table.dataset.btnEdit) {
      const btnEdit = document.getElementById(table.dataset.btnEdit);
      if (btnEdit) {
         btnEdit.disabled = true;
      }
   }
   if (table.dataset.btnDel) {
      const btnDel = document.getElementById(table.dataset.btnDel);
      if (btnDel) {
         btnDel.disabled = true;
      }
   }
}

function xw_getSelectedRecord(table) {
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

function xw_fetchTableSelectedRow( targetId, sourceId, cb, url, options) {
   const ele = document.getElementById(targetId);
   const rec = xw_getSelectedRecord(ele);
   if (rec) {
      xw_fetch(targetId, sourceId, cb, url, options, rec);
   }
}

function xw_addRowTable(table, data) {
   const row = document.createElement("tr");
   const flds = [];
   let n = 0;
   Array.from(table.querySelectorAll("th")).forEach((th) => {
      flds.push(th.dataset.field);
   });
   flds.forEach( field => {
      const cell = document.createElement("td");
      cell.textContent = data[field];
      row.appendChild(cell);
   });
   table.querySelector("tbody").appendChild(row);
}

function xw_saveRowTable(table, data) {
   const row = table.querySelector(".row-selected");
   if (row) {
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

function xw_postEditTable(table, data) {
   if (Object.keys(data).length > 0) {
      if (data.hasOwnProperty("error")) {
         if (data.hasOwnProperty("errorFunction")) {
            eval(`${data.errorFunction}`)(data.error);
         } else {
            alert(data.error);
         }
      } else {
         if (data.append_mode === "true") {
            xw_addRowTable(table, data)
         } else {
            xw_saveRowTable(table, data)
         }
      }
   }
}

function xw_delRowTable(table, result) {
   if (result.pass) {
      const row = table.querySelector(".row-selected");
      if (row) {
         table.deleteRow(row.rowIndex);
         if (table.dataset.btnEdit) {
            const btnEdit = document.getElementById(table.dataset.btnEdit);
            if (btnEdit) {
               btnEdit.disabled = true;
            }
         }
         if (table.dataset.btnDel) {
            const btnDel = document.getElementById(table.dataset.btnDel);
            if (btnDel) {
               btnDel.disabled = true;
            }
         }
      }
   }
}


function xw_eventToObject(ev) {
   const e = {"target": ev.target || ev.srcElement,
              "isTrusted":ev.isTrusted,
              "altKey":ev.altKey,"ctrlKey":ev.ctrlKey,
              "clientX":ev.clientX,"clientY":ev.clientY};
   if (ev.detail) {
      for(const[nam, val] of Object.entries(ev.detail)) {
         e["detail-"+nam] = val;
      }
   }
   return e;
}

async function xw_fetch( targetId, sourceId, cb, url, options, cargo ) {
   const ev  = xw_eventToObject(window.event);
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
      xw_setCookie("cargo", JSON.stringify(cargo), {'max-age': 60});
   }
   xw_setCookie("event", JSON.stringify(ev), {'max-age': 60});
   try {
      const pos = url.search("&");
      if (pos > 0 ) {
         let tParams = "";
         url.substring(pos+1).split("&").forEach(token => {
            const tId  = token.split("=")[0];
            const tFld = token.split("=")[1];
            const tEl = document.getElementById(tId);
            if (tEl) {
               if (tEl.getAttribute(tFld)) {
                  tParams += "&" + tId + "_" + tFld + "=" + tEl[tFld];
               } else {
                  tParams += "&detail-" + tFld + "=" + tEl.dataset[tFld];
               }
            }
            });
         url = url.substring(0, pos)+tParams;
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
         if (cb ) {
            eval(cb)(el,dt,ev);
         } else {
            return dt;
           //console.warn('Call-back function empty');
         }
      }
   }
}

function xw_setHbData(element, hbData, value, post = false) {
   if (post) {
      if (xw_mapPost.has(element.id)) {
         xw_mapPost.get(element.id)[hbData.toLowerCase()] = value;
      } else {
         xw_mapPost.set(element.id, {});
         xw_mapPost.get(element.id)[hbData.toLowerCase()] = value;
      }
   } else {
      xw_mapProp.get(element.id)[hbData.toLowerCase()] = value;
   }
}

async function xw_getHbData(service, doc, type) {
   if (typeof doc === "undefined") {
      doc = xw_docName;
   }
   if (typeof type === "undefined") {
      type = "application/json";
   }
   const url = xw_baseUri() + "?service=" + doc + "-" + service;
   const opt = {method: "GET", mode: "cors", cache: "no-cache",
                credentials: "same-origin",
                headers: { "Content-type": type},
                redirect: "follow",
                referrerPolicy: "no-referrer",
               };
   return await xw_fetch( null, null, null, url, opt );
}

function xw_savePost(id, url) {
   if (xw_mapPost.size === 0) {
      return false;
   }
   let form = document.getElementById(id);
   if (form) {
      let input;
      for (const [id, obj] of xw_mapPost) {
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
      for (const [id, obj] of xw_mapPost) {
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

function xw_submit(e) {
   let ele = document.getElementById( e.target.name );
   if (ele) {
      xw_setCookie("session", xw_getCookie("session"), {'max-age': 60});
      xw_saveState();
      xw_saveSession();
      xw_savePost(e.target.name);
   }
   return true;
}

function xw_runAction(element, funcName, ev) {
   const e = xw_eventToObject(ev);
   const d = ((element && element.dataset.document) ? element.dataset.document : xw_docName);
   const p = xw_baseUri() + "?action=" + d + "-" + funcName;

   xw_setCookie("event", JSON.stringify(e), {'max-age': 60});
   xw_setCookie("session", xw_getCookie("session"), {'max-age': 60});
   xw_saveState();
   xw_saveSession();
   if (!xw_savePost(null,p)) {
      window.location = p;
   }
}

function xw_processEvent( element, eventName, eventValue ) {
   if (eventValue.startsWith('@')) {
      element.addEventListener(eventName.substring(2), function(ev) {
         xw_runAction( element, eventValue.substring(1), ev );
      });
   } else {
      try {
         const evName = eventName.substring(2);
         if (typeof eval(`${eventValue}`) === "function") {
            element.addEventListener(eventName.substring(2), eval(`${eventValue}`));
         }
      } catch (exception) {
         console.warn(exception);
      }
   }
}

class xw_FormValidator {
   constructor(selector) {
      this.form = document.querySelector(selector);
      this.inputsWithErrors = new Set();
      this.inputs = new Set();
      this.validated = false;

      this.form.addEventListener("submit", e => {
         e.preventDefault();
         if (!this.validated) {
            this.validate();
         }
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

               const url = xw_baseUri() + "?service=" + doc + "-" + para.service;
               const table = this.form.dataset.table;
               if (table) {
                  const rec = xw_getSelectedRecord(document.getElementById(table));
                  if (rec) {
                     for(const[key, val] of Object.entries(rec)) {
                        sp.append("old_"+key, val);
                     }
                  }
                  xw_fetch( table, null, "xw_postEditTable", url, opt );
               } else {
                  xw_fetch( null, null, null, url, opt );
               }

               const page = this.form.closest("x-page");
               if (page) {
                  xw_showPage(page.id, false);
               }
            }
         } else {
            this.inputsWithErrors.forEach( (input) => {
               const errorElement = input.closest(".xw-input").querySelector(".xw-input__error");
               errorElement.textContent = input.dataset.error;
            });
         }
      });
   }

   get hasErrors() {
      return this.inputsWithErrors.size > 0;
   }

   register(selector, func) {
      const fv = this;
      const inputField = this.form.querySelector(selector);
      const errorElement = inputField.closest(".xw-input").querySelector(".xw-input__error");
      let isJsFunc;
      let execute;
      try {
         isJsFunc = (typeof eval(`${func}`) === "function");
      } catch (exception) {
         isJsFunc = false;
      }
      this.inputs.add(inputField);

      if (isJsFunc) {
         execute = async function(hideErrors) {
            const { pass, error } = eval(`${func}`)(inputField.value, inputField);

            inputField.dataset['error'] = error;

            if (!hideErrors) {
               errorElement.textContent = error || "";
            }

            if (!pass) {
               fv.inputsWithErrors.add(inputField);
            } else {
               fv.inputsWithErrors.delete(inputField);
            }
         };
      } else {
         execute = async function(hideErrors) {
            const { pass, error } = await xw_fieldValidator(inputField, func, fv.form);

            inputField.dataset['error'] = error;

            if (!hideErrors) {
               errorElement.textContent = error || "";
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

   async validate() {
      this.inputs.forEach( async (input) => {await input.validate();});
      this.validated = true;
   }
}

async function xw_fieldValidator(inputField, hbFunc, frm) {
   const doc = frm.dataset.document;
   let re;
   let dt;
   let url  = xw_baseUri() + "?service=" + doc.toLowerCase() + "-" +
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

function xw_showPage(idPage, show, idForm, idTable, append) {
   const ele = document.getElementById( idPage );
   if (ele) {
      if (show) {
         ele.style.display="block";
         if ( idForm && idTable) {
            const frm = document.getElementById(idForm);
            const tbl = document.getElementById(idTable);
            if (frm) {
               frm.querySelectorAll(".xw-input__error").forEach( lbl => {
                  lbl.textContent = "";
               });
            }
            if (frm && tbl) {
               const rec = xw_getSelectedRecord(tbl);
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
      } else {
         ele.style.display="none";
      }
   }
}
