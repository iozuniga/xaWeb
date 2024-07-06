/*
 * Proyect: XailerWeb framework
 * File: ZStyle.prg
 * Description: Style helper class por Nefele users
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 * Important: VALUES clause is necessary in order to assing style to parent
 * Note: This classs automatically updates de styles of its parent when its
         data is assigned. Be aware, that the value returned for this data
         members may not be accurate, since the style maybe changed on its
         parent directly
 */

#include "xailerweb.ch"
#include "error.ch"


CLASS ZStyle
EXPORTED:
   DATA oParent

   DATA cAccent_color   VALUES "*"
   DATA cAlign_content  VALUES "stretch", "center", "flex-start", "flex-end", "space-between", "space-around", "space-evenly", "initial", "inherit"
   DATA cAlign_items    VALUES "normal", "stretch", "center", "flex-start", "flex-end", "start", "end", "baseline", "initial", "inherit"
   DATA cAlign_self     VALUES "normal", "stretch", "center", "flex-start", "flex-end", "baseline", "initial", "inherit"
   DATA cAll            VALUES "initial", "inherit", "unset"
   DATA cAnimation      VALUES "animation-name", "animation-duration", "animation-timing-function", "animatrion-delay", "animation-iteration-count", "animation-direction", "animation-fill-mode", "animation-play-state", "initial", "inherit"
   DATA cAnimation_delay VALUES "*"
   DATA cAnimation_direction VALUES "normal", "reverse", "alternate", "alternate-reverse", "initial", "inherit"
   DATA cAnimation_duration VALUES "*"
   DATA cAnimation_fill_mode VALUES "none", "forwards", "backwards", "bouth", "initial", "inherit"
   DATA cAnimation_iteration_count VALUES "*"
   DATA cAnimation_name VALUES "*"
   DATA cAnimation_play_state VALUES "paused", "running", "initial", "inherit"
   DATA cAnimation_timing_function  VALUES "linear", "ease", "ease-in", "ease-out", "step-start", "step-end", "steps()", "cubic-bezier()", "initial", "inherit"

   DATA cBackground VALUES "*"
   DATA cBackground_attachment VALUES "*"
   DATA cBackground_blend_mode VALUES "normal", "multiply", "screen", "overlay", "darken", "lighten", "color-dodge", "saturation", "color", "luminosity"
   DATA cBackground_clip  VALUES "border-box", "padding-box", "content-box", "initial", "inherit"
   DATA cBackground_color VALUES "*"
   DATA cBackground_image VALUES "*"
   DATA cBackground_origin VALUES "*"
   DATA cBackground_position VALUES "*"
   DATA cBackground_repeat VALUES "repeat", "repeat-x", "repeat-y", "space", "round", "initial", "inherit"
   DATA cBackground_size   VALUES "auto", "length", "percentage", "cover", "contain", "initial", "inherit"
   DATA cBorder VALUES "*"
   DATA cBorder_bottom VALUES "*"
   DATA cBorder_bottom_color VALUES "*"
   DATA cBorder_bottom_left_radius VALUES "*"
   DATA cBorder_bottom_right_radius VALUES "*"
   DATA cBorder_bottom_style VALUES "*"
   DATA cBorder_bottom_width VALUES "*"
   DATA cBorder_collapse VALUES "*"
   DATA cBorder_color VALUES "*"
   DATA cBorder_image VALUES "*"
   DATA cBorder_image_outset VALUES "*"
   DATA cBorder_image_repeat VALUES "*"
   DATA cBorder_image_slice VALUES "*"
   DATA cBorder_image_source VALUES "*"
   DATA cBorder_image_width VALUES "*"
   DATA cBorder_left VALUES "*"
   DATA cBorder_left_color VALUES "*"
   DATA cBorder_left_style VALUES "*"
   DATA cBorder_left_width VALUES "*"
   DATA cBorder_radius VALUES "*"
   DATA cBorder_right VALUES "*"
   DATA cBorder_right_color VALUES "*"
   DATA cBorder_right_style VALUES "*"
   DATA cBorder_right_width VALUES "*"
   DATA cBorder_spacing VALUES "*"
   DATA cBorder_style VALUES "*"
   DATA cBorder_top VALUES "*"
   DATA cBorder_top_color VALUES "*"
   DATA cBorder_top_left_radius VALUES "*"
   DATA cBorder_top_right_radius VALUES "*"
   DATA cBorder_top_style VALUES "*"
   DATA cBorder_top_width VALUES "*"
   DATA cBorder_width VALUES "*"
   DATA cBottom VALUES "*"
   DATA cBox_decoration_break VALUES "*"
   DATA cBox_shadow VALUES "*"
   DATA cBox_sizing VALUES "*"
   DATA cBreak_after VALUES "*"
   DATA cBreak_before VALUES "*"
   DATA cBreak_inside VALUES "*"

   DATA cCaption_side VALUES "*"
   DATA cCaret_color VALUES "*"
   DATA cCharset VALUES "*"
   DATA cClear VALUES "*"
   DATA cClip VALUES "*"
   DATA cColor VALUES "*"
   DATA cColumn_count VALUES "*"
   DATA cColumn_fill VALUES "*"
   DATA cColumn_gap VALUES "*"
   DATA cColumn_rule VALUES "*"
   DATA cColumn_rule_color VALUES "*"
   DATA cColumn_rule_style VALUES "*"
   DATA cColumn_rule_width VALUES "*"
   DATA cColumn_span VALUES "*"
   DATA cColumn_width VALUES "*"
   DATA cColumns VALUES "*"
   DATA cContent VALUES "*"
   DATA cCounter_increment VALUES "*"
   DATA cCounter_reset VALUES "*"
   DATA cCursor VALUES "*"

   DATA cDirection VALUES "*"
   DATA cDisplay VALUES "*"

   DATA cEmpty_cells VALUES "*"

   DATA cFilter VALUES "*"
   DATA cFlex VALUES "*"
   DATA cFlex_basis VALUES "*"
   DATA cFlex_direction VALUES "*"
   DATA cFlex_flow VALUES "*"
   DATA cFlex_grow VALUES "*"
   DATA cFlex_shrink VALUES "*"
   DATA cFlex_wrap VALUES "*"
   DATA cFloat VALUES "*"
   DATA cFont VALUES "*"
   DATA cFont_face VALUES "*"
   DATA cFont_family VALUES "*"
   DATA cFont_feature_settings VALUES "*"
   DATA cFont_feature_values VALUES "*"
   DATA cFont_kerning VALUES "*"
   DATA cFont_language_override VALUES "*"
   DATA cFont_size VALUES "*"
   DATA cFont_size_adjust VALUES "*"
   DATA cFont_stretch VALUES "*"
   DATA cFont_style VALUES "*"
   DATA cFont_synthesis VALUES "*"
   DATA cFont_variant VALUES "*"
   DATA cFont_variant_alternates VALUES "*"
   DATA cFont_variant_caps VALUES "*"
   DATA cFont_variant_east_asian VALUES "*"
   DATA cFont_variant_ligatures VALUES "*"
   DATA cFont_variant_numeric VALUES "*"
   DATA cFont_variant_position VALUES "*"
   DATA cFont_weight VALUES "*"
   DATA cFont_variation_settings VALUES "*"

   DATA cGrid VALUES "*"
   DATA cGrid_area VALUES "*"
   DATA cGrid_auto_columns VALUES "*"
   DATA cGrid_auto_flow VALUES "*"
   DATA cGrid_auto_rows VALUES "*"
   DATA cGrid_column VALUES "*"
   DATA cGrid_column_end VALUES "*"
   DATA cGrid_column_gap VALUES "*"
   DATA cGrid_column_start VALUES "*"
   DATA cGrid_gap VALUES "*"
   DATA cGrid_row VALUES "*"
   DATA cGrid_row_end VALUES "*"
   DATA cGrid_row_gap VALUES "*"
   DATA cGrid_row_start VALUES "*"
   DATA cGrid_template VALUES "*"
   DATA cGrid_template_areas VALUES "*"
   DATA cGrid_template_columns VALUES "*"
   DATA cGrid_template_rows VALUES "*"

   DATA cHanging_punctuation VALUES "*"
   DATA cHeight VALUES "*"
   DATA cHyphens VALUES "*"

   DATA cImage_rendering VALUES "*"
   DATA cImport VALUES "*"
   DATA cIsolation VALUES "*"

   DATA cJustify_content VALUES "*"
   DATA cKeyframes VALUES "*"
   DATA cLeft VALUES "*"
   DATA cLetter_spacing VALUES "*"
   DATA cLine_break VALUES "*"
   DATA cLine_height VALUES "*"
   DATA cList_style VALUES "*"
   DATA cList_style_image VALUES "*"
   DATA cList_style_position VALUES "*"
   DATA cList_style_type VALUES "*"
   DATA cMargin VALUES "*"
   DATA cMargin_bottom VALUES "*"
   DATA cMargin_left VALUES "*"
   DATA cMargin_right VALUES "*"
   DATA cMargin_top VALUES "*"
   DATA cMax_height VALUES "*"
   DATA cMax_width VALUES "*"
   DATA cMin_height VALUES "*"
   DATA cMin_width VALUES "*"
   DATA cMix_blend_mode VALUES "*"
   DATA cObject_fit VALUES "*"
   DATA cObject_position VALUES "*"
   DATA cOpacity VALUES "*"
   DATA cOrder VALUES "*"
   DATA cOrphans VALUES "*"
   DATA cOutline VALUES "*"
   DATA cOutline_color VALUES "*"
   DATA cOutline_offset VALUES "*"
   DATA cOutline_style VALUES "*"
   DATA cOutline_width VALUES "*"
   DATA cOverflow VALUES "*"
   DATA cOverflow_wrap VALUES "*"
   DATA cOverflow_x VALUES "*"
   DATA cOverflow_y VALUES "*"

   DATA cPadding VALUES "*"
   DATA cPadding_bottom VALUES "*"
   DATA cPadding_left VALUES "*"
   DATA cPadding_right VALUES "*"
   DATA cPadding_top VALUES "*"
   DATA cPage_break_after VALUES "*"
   DATA cPage_break_before VALUES "*"
   DATA cPage_break_inside VALUES "*"
   DATA cPerspective VALUES "*"
   DATA cPerspective_origin VALUES "*"
   DATA cPointer_events VALUES "*"
   DATA cPosition VALUES "*"
   DATA cQuotes VALUES "*"
   DATA cResize VALUES "*"
   DATA cRight VALUES "*"

   DATA cScroll_behavior VALUES "*"
   DATA cTab_size VALUES "*"
   DATA cTable_layout VALUES "*"
   DATA cText_align VALUES "*"
   DATA cText_align_last VALUES "*"
   DATA cText_combine_upright VALUES "*"
   DATA cText_decoration VALUES "*"
   DATA cText_decoration_color VALUES "*"
   DATA cText_decoration_line VALUES "*"
   DATA cText_decoration_style VALUES "*"
   DATA cText_indent VALUES "*"
   DATA cText_justify VALUES "*"
   DATA cText_orientation VALUES "*"
   DATA cText_overflow VALUES "*"
   DATA cText_shadow VALUES "*"
   DATA cText_transform VALUES "*"
   DATA cText_underline_position VALUES "*"
   DATA cTop VALUES "*"
   DATA cTransform VALUES "*"
   DATA cTransform_origin VALUES "*"
   DATA cTransform_style VALUES "*"
   DATA cTransition VALUES "*"
   DATA cTransition_delay VALUES "*"
   DATA cTransition_duration VALUES "*"
   DATA cTransition_DATA VALUES "*"
   DATA cTransition_timing_function VALUES "*"
   DATA cUnicode_bidi VALUES "*"
   DATA cUser_select VALUES "*"
   DATA cVertical_align VALUES "*"
   DATA cVisibility VALUES "*"
   DATA cWhite_space VALUES "*"
   DATA cWidows VALUES "*"
   DATA cWidth VALUES "*"
   DATA cWord_break VALUES "*"
   DATA cWord_spacing VALUES "*"
   DATA cWord_wrap VALUES "*"
   DATA cWriting_mode VALUES "*"
   DATA cZ_index VALUES "*"

   METHOD New( oParent ) CONSTRUCTOR

RESERVED:
   METHOD SetStyle( cKey, cValue )
   //METHOD Preprocess()

END CLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZStyle

   ::oParent := oParent

RETURN Self

//------------------------------------------------------------------------------
// ya no es necesario, se hace directamente por la clausula VALUE de DATA

//METHOD Preprocess() CLASS ZStyle

//   LOCAL aVal
//
//   FOR EACH aVal IN __objGetProperties( Self, .T. )
//      IF Left( aVal[ 1 ], 1 ) == "C" .AND. aVal[ 2 ] != NIL
//         ::SetStyle( aVal[ 1 ], aVal[ 2 ] )
//      ENDIF
//   NEXT

//RETURN nil

//------------------------------------------------------------------------------

METHOD SetStyle( cKey, cValue ) CLASS ZStyle

   cKey := Lower( SubStr( cKey, 2 ) )
   cKey := StrTran( cKey, "_", "-" )

   IF !Empty( cValue )
      ::oParent:AddStyle( cKey + ":" + ToString( cValue ) + ";")
   ELSE
      ::oParent:DelStyle( cKey )
   ENDIF

RETURN NIL

//------------------------------------------------------------------------------

