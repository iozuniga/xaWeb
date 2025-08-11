/*
 * Proyecto: xaWeb framework
 * Fichero: ZSyntaxHilite.prg
 * Descripción: Hilite syntax package
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * More at: https://highlightjs.org/
 *          https://github.com/highlightjs/highlight.js
 *          https://github.com/highlightjs/highlight.js/issues/3652
 */

#include "xaWeb.ch"

#define HLJS_BASE_URL "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/highlight.min.js"
#define HLJS_LANG_URL "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/languages/"

CLASS ZSyntaxHilite FROM WPackage
PUBLISHED:
   METHOD AddLang( cLang ) INLINE AAdd( ::aLangs, cLang )

RESERVED:
   DATA cName        INIT "xa_SyntaxHilite" READONLY
   DATA aLangs       INIT {}
   DATA lAutoCreated INIT .F.

   METHOD Render()
   METHOD CssBase()
   METHOD CssColors()
   METHOD PreProcess()  VIRTUAL

ENDCLASS

//------------------------------------------------------------------------------

METHOD Render() CLASS ZSyntaxHilite

   LOCAL cLang

   WITH OBJECT ::AddCSS( ::CssColors(), ::cName )
      :AddCode( ::CssBase(), ::cName )
   END WITH

   ::AddScript( HLJS_BASE_URL )

   FOR EACH cLang IN ::aLangs
      ::AddScript( HLJS_LANG_URL + cLang + ".min.js" )
   NEXT

   ::AddScript( "hljs.highlightAll();",,.F. ):lBottom := .T.

RETURN ::Super:Render()

//------------------------------------------------------------------------------

METHOD CssColors() CLASS ZSyntaxHilite

   LOCAL cCss

   TEXT INTO cCss
   [theme='light'] {
    --highlightjs-base00: #f9f9f9;
    --highlightjs-base01: #e7eaec;
    --highlightjs-base02: #cceae7;
    --highlightjs-base03: #9f9f9f;
    --highlightjs-base04: #8796b0;
    --highlightjs-base05: #404040;
    --highlightjs-base06: #404040;
    --highlightjs-base07: #ffffff;
    --highlightjs-base08: #ff5370;
    --highlightjs-base09: #f76d47;
    --highlightjs-base0A: #ff914d;
    --highlightjs-base0B: #91b859;
    --highlightjs-base0C: #39adb5;
    --highlightjs-base0D: #6182b8;
    --highlightjs-base0E: #7c4dff;
    --highlightjs-base0F: #e53935;
   }

   [theme='dark'] {
    --highlightjs-base00: #232323;
    --highlightjs-base01: #303030;
    --highlightjs-base02: #353535;
    --highlightjs-base03: #a9a9a9;
    --highlightjs-base04: #b2ccd6;
    --highlightjs-base05: #e3e3e3;
    --highlightjs-base06: #e3e3e3;
    --highlightjs-base07: #ffffff;
    --highlightjs-base08: #f07178;
    --highlightjs-base09: #f78c6c;
    --highlightjs-base0A: #ffcb6b;
    --highlightjs-base0B: #c3e88d;
    --highlightjs-base0C: #89ddff;
    --highlightjs-base0D: #82aaff;
    --highlightjs-base0E: #c792ea;
    --highlightjs-base0F: #ff5370;
   }
   ENDTEXT

RETURN cCss

//------------------------------------------------------------------------------

METHOD CssBase() CLASS ZSyntaxHilite

   LOCAL cCss

   TEXT INTO cCss
   pre code.hljs {
     display: block;
     overflow-x: auto;
     padding: 1em;
   }

   code.hljs {
     padding: 3px 5px;
   }

   .hljs {
     color: var(--highlightjs-base05);
     background: var(--highlightjs-base00);
   }

   .hljs::selection,
   .hljs ::selection {
     background-color: var(--highlightjs-base02);
     color: var(--highlightjs-base05);
   }

   .hljs-formula,
   .hljs-params,
   .hljs-property {}

   .hljs-comment {
     color: var(--highlightjs-base03);
   }

   .hljs-tag {
     color: var(--highlightjs-base04);
   }

   .hljs-subst,
   .hljs-punctuation,
   .hljs-operator {
     color: var(--highlightjs-base05);
   }

   .hljs-operator {
     opacity: 0.7;
   }

   .hljs-bullet,
   .hljs-variable,
   .hljs-template-variable,
   .hljs-selector-tag,
   .hljs-name,
   .hljs-deletion {
     color: var(--highlightjs-base08);
   }

   .hljs-symbol,
   .hljs-number,
   .hljs-link,
   .hljs-attr,
   .hljs-variable.constant_,
   .hljs-literal {
     color: var(--highlightjs-base09);
   }

   .hljs-title,
   .hljs-class .hljs-title,
   .hljs-title.class_ {
     color: var(--highlightjs-base0A);
   }

   .hljs-strong {
     font-weight: bold;
     color: var(--highlightjs-base0A);
   }

   .hljs-code,
   .hljs-addition,
   .hljs-title.class_.inherited__,
   .hljs-string {
     color: var(--highlightjs-base0B);
   }

   .hljs-built_in,
   .hljs-doctag,
   .hljs-quote,
   .hljs-keyword.hljs-atrule,
   .hljs-regexp {
     color: var(--highlightjs-base0C);
   }

   .hljs-function .hljs-title,
   .hljs-attribute,
   .ruby .hljs-property,
   .hljs-title.function_,
   .hljs-section {
     color: var(--highlightjs-base0D);
   }

   .hljs-type,
   .hljs-template-tag,
   .diff .hljs-meta,
   .hljs-keyword {
     color: var(--highlightjs-base0E);
   }

   .hljs-emphasis {
     color: var(--highlightjs-base0E);
     font-style: italic;
   }

   .hljs-meta,
   .hljs-meta .hljs-keyword,
   .hljs-meta .hljs-string {
     color: var(--highlightjs-base0F);
   }

   .hljs-meta .hljs-keyword,
   .hljs-meta-keyword {
     font-weight: bold;
   }
   ENDTEXT

RETURN cCss

//------------------------------------------------------------------------------
