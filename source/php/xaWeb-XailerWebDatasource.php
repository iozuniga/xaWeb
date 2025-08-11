<?php
/*
 * Xailer source code:
 *
 * wa_wdsMySql.php
 * PHP MySql module for TWebDataSource
 *
 * Copyright 2012, 2025 Ignacio Ortiz de Zuñiga (Oz Software)
 * Copyright 2003, 2025 Xailer.com
 * All rights reserved
 * Important note: This software is intented to be use solely in conjunction with
 * Xailer TWebDatasource. Any other use, even partially, is completely prohibited.
 *
 * To send emails via phpmailer. Set all the values and paths on XA_SendMail function
 *
 */

/*
You must set this values accordly with your application & BD
*/

$cDBUserName = '';
$cDBPassword = '';
$cCryptKey   = '';
$lDbLatin    = true;

// For xaWeb php-cli management
if (php_sapi_name() === 'cli') {
   parse_str(implode('&', array_slice($argv, 1)), $_GET);
   parse_str(stream_get_contents(STDIN), $_POST);
   $lDbLatin = false;
}

use PHPMailer\PHPMailer\PHPMailer;
use PHPMailer\PHPMailer\Exception;

date_default_timezone_set("Europe/Madrid");
setlocale(LC_ALL, 'es_ES.UTF-8');

/*
Own error handler
*/

error_reporting( 0 );
set_error_handler( 'xa_ErrorHandler' );
mysqli_report(MYSQLI_REPORT_STRICT);

/*
Init XML definition
*/

echo "<?xml version='1.0' encoding='iso-8859-1'?>\r\n";
echo "<Query>\r\n";

/*
Retrieve GET & POST values
*/

if ( isset( $_POST[ 'data' ] ) ) {
   $cPostData = pack('H*', $_POST[ 'data' ] );
} else {
   XA_Exit( 'PHP ERROR: POST data empty: Error on PostRequest statement' );
   return;
}

if ( isset( $_GET[ 'validate' ] ) ) {
   $nValid = $_GET[ 'validate' ];
} else {
   $nValid = 0;
}

/*
Validation control
*/

if ( XA_Validate($cPostData, $cCryptKey ) != $nValid ) {
   XA_Exit( 'PHP ERROR: Invalid POST request: Validation error' );
   return;
}

/*
If dbName is set, connect to it
*/

if ( isset( $_GET[ 'dbName' ] ) ) {
   $cDBName=$_GET[ 'dbName' ];
   $oDB = new mysqli( 'p:localhost', $cDBUserName, $cDBPassword, $cDBName );
   if ( $oDB->connect_errno ) {
      XA_Exit( 'PHP ERROR: Failed to connect to MySQL: (' . $oDB->connect_errno . ') ' . $oDB->connect_error );
      return;
   }
   if ( $lDbLatin ) {
      $oDB->set_charset('latin1');
   }
} else {
   $oDB = NULL;
}

/*
Command control
*/

if ( isset( $_GET[ 'command' ] ) ) {
   switch ( $_GET[ 'command' ] ) {
   case 'PING':
      XA_Ping( 'ping' );
      break;
   case 'MYIP':
      XA_MyIP();
      break;
   case 'MYLOCATION':
      XA_MyLocation();
      break;
   case 'EXECUTE':
      XA_Execute( $oDB, $cPostData );
      break;
   case 'BULKEXECUTE':
      XA_BulkExecute( $oDB, $cPostData );
      break;
   case 'QUERYDATASET':
      XA_QueryDataset( $oDB, $cPostData );
      break;
   case 'QUERYARRAY':
      XA_QueryArray( $oDB, $cPostData );
      break;
   case 'GETCATALOGS':
      XA_GetCatalogs( $oDB, $cDBUserName, $cDBPassword );
      break;
   case 'GETTABLES':
      XA_GetTables( $oDB, $cPostData );
      break;
   case 'UPLOADFILE':
      XA_UploadFile( $cPostData );
      break;
   case 'DELETEFILE':
      XA_DeleteFile( $cPostData );
      break;
   case 'CURRENTDIR':
      XA_CurrentDir( $cPostData );
      break;
   case 'ISDIR':
      XA_IsDir( $cPostData );
      break;
   case 'ISFILE':
      XA_IsFile( $cPostData );
      break;
   case 'MKDIR':
      XA_MkDir( $cPostData );
      break;
   case 'AFILES':
      XA_AFiles( $cPostData );
      break;
   case 'ADIRS':
      XA_ADirs( $cPostData );
      break;
   case 'SENDMAIL':
      XA_SendMail( $cPostData );
      break;
   default:
      XA_AddError( 'PHP ERROR: #Error: Invalid command: '.$_GET[ 'command' ] );
   }
}
else {
   XA_AddError( 'PHP ERROR: Command not defined' );
}

/*
Quit
*/

if ( isset( $oDB ) ) {
   //$oDB->close();
}

XA_Exit('');

//------------------------------------------------------------------------------
/*
 If you assign the TWebDataSource:OnValidate event on your own code,
 YOU MUST also modify this code accordly
*/

function XA_Validate( $cString, $cKey ) {
   $valid=sprintf( "%u", crc32( md5( $cString.$cKey ) ) );
   $valid+= strlen( $cString.$cKey );
   return ( $valid );
}

//------------------------------------------------------------------------------
// Formato: "to;name|lhtml|subject|body

function XA_SendMail( $cData ) {

   $aInfo    = explode( '|', $cData );
   $aTo      = explode( ';', $aInfo[ 0 ] );
   $cTo      = $aTo[ 0 ];
   $cName    = $aTo[ 1 ];
   $lHtml    = ( $aInfo[ 1 ] == '1' );
   $cSubject = $aInfo[ 2 ];
   $cBody    = $aInfo[ 3 ];

   require 'inscripcion/phpmailer/src/Exception.php';
	require 'inscripcion/phpmailer/src/PHPMailer.php';
	require 'inscripcion/phpmailer/src/SMTP.php';

   $mail = new PHPMailer(true);

   try {
      $mail->isSMTP();
      $mail->SMTPDebug  = 0;
      $mail->SMTPSecure = 'ssl';
      $mail->Host       = '';
      $mail->SMTPAuth   = TRUE;
      $mail->Port       = 465;
      $mail->Username   = '';
      $mail->Password   = '';

      $mail->setFrom( '', 'Mailer' );
      $mail->addAddress( $cTo, $cName );

      $mail->isHTML( $lHtml );
      $mail->Subject = utf8_decode( $cSubject );
      $mail->Body    = $cBody;

      if( $mail->send() )
         XA_AddData( 'SendMail', TRUE );
      else {
         XA_AddData( 'SendMail', FALSE );
         XA_AddData( 'SendError', $mail->ErrorInfo );
      }

   } catch (Exception $e) {
      XA_AddData( 'SendMail', FALSE );
      XA_AddData( 'SendError', $mail->ErrorInfo );
   }	

   return;
}

//------------------------------------------------------------------------------

function XA_Ping($cData) {
   XA_AddData( 'Pong', $cData );
   return;
}

//------------------------------------------------------------------------------

function XA_MyIP() {

   if (!empty($_SERVER['HTTP_CLIENT_IP'])) {
       $ip = $_SERVER['HTTP_CLIENT_IP'];
     } elseif (!empty($_SERVER['HTTP_X_FORWARDED_FOR'])) {
       $ip = $_SERVER['HTTP_X_FORWARDED_FOR'];
     } else {
       $ip = $_SERVER['REMOTE_ADDR'];
     }

   XA_AddData( 'ip', $ip );
   return;
}

//------------------------------------------------------------------------------

function XA_MyLocation() {

   if (!empty($_SERVER['HTTP_CLIENT_IP'])) {
       $ip = $_SERVER['HTTP_CLIENT_IP'];
     } elseif (!empty($_SERVER['HTTP_X_FORWARDED_FOR'])) {
       $ip = $_SERVER['HTTP_X_FORWARDED_FOR'];
     } else {
       $ip = $_SERVER['REMOTE_ADDR'];
     }

   $response = json_decode( @file_get_contents( "http://ipinfo.io/$ip/json" ) );
   XA_AddData( 'ip', $ip );
   XA_AddData( 'city', $response->city );
   XA_AddData( 'region', $response->region );
   XA_AddData( 'country', $response->country );
   return;
}

//------------------------------------------------------------------------------

function XA_Execute( $oDB, $cQuery ) {
   if ( !isset( $oDB ) ) {
      XA_Exit( 'PHP ERROR: Database name not set' );
      return;
   }

   $cQyery = $oDB->real_escape_string( $cQuery );
   $result = $oDB->query( $cQuery);
   if ( $result ) {
      XA_AddData( 'Affected_rows', $oDB->affected_rows );

      if ( stristr( $cQuery, 'INSERT ' ) <> FALSE ) {
         XA_AddData( 'Insert_Id', $oDB->insert_id );
      }

      if ( is_object( $result ) ) {
         $rows = Array();
       
         while ( $row = XA_FetchRow( $result ) ) {
            $rows[] = $row;
         }
      
         XA_AddData( 'Data', $rows );
         $result->free();
      }
   } else {
      XA_AddError( $oDB->error.' ('.$oDB->errno.')' );
   }
   return;
}

//------------------------------------------------------------------------------

function XA_BulkExecute( $oDB, $cQuery ) {
   if ( !isset( $oDB ) ) {
      XA_Exit( 'PHP ERROR: Database name not set' );
      return;
   }

   $cQyery = $oDB->real_escape_string( $cQuery );
   $oDB->autocommit( FALSE );
   $lError = FALSE;
   $nRows = 0;

   $oDB->multi_query( $cQuery );

   do {
      $result = $oDB->use_result();
      if ( $oDB->error ) {
         XA_AddError( $oDB->error.' [# '.$oDB->errno.']' );
         $oDB->rollback();
         $lError = TRUE;
         break;
      }
      $nRows += $oDB->affected_rows;
   } while( $oDB->more_results() && $oDB->next_result() );

   if ( !$lError ) {
      XA_AddData( 'Affected_rows', $nRows );
      $oDB->commit();
   }

   $oDB->autocommit( TRUE );
   return;
}

//------------------------------------------------------------------------------

function XA_QueryArray( $oDB, $cQuery ) {
   if ( !isset( $oDB ) ) {
      XA_Exit( 'PHP ERROR: Database name not set' );
      return;
   }

   $cQyery = $oDB->real_escape_string( $cQuery );
   $result = $oDB->query( $cQuery );
   if ( is_object( $result ) ) {
      XA_QueryInfo( $result );
      XA_AddData( 'Affected_rows', $oDB->affected_rows );
      $rows = Array();
      while ( $row = XA_FetchRow( $result ) ) {
         $rows[] = $row;
      }
      XA_AddData('Data', $rows );
      $result->free();
   } elseif( !$result ) {
      XA_AddError( $oDB->error.' ('.$oDB->errno.')' );
   }
   return;
}

//------------------------------------------------------------------------------

function XA_QueryDataset( $oDB, $cQuery ) {
   if ( !isset( $oDB ) ) {
      XA_Exit( 'PHP ERROR: Database name not set' );
      return;
   }

   $cQyery = $oDB->real_escape_string( $cQuery );
   $result = $oDB->query( $cQuery );
   if ( is_object( $result ) ) {
      $table = XA_GetPrimaryTable( $result );
      XA_AddData( "Primary_Table", $table );
      XA_AddData( "Primary_key", XA_GetPrimaryKeys( $oDB, $table ) );
      XA_QueryInfo( $result );
      XA_AddData( 'Affected_rows', $oDB->affected_rows );
      $rows = Array();
      while ($row = XA_FetchRow( $result ) ) {
         $rows[] = $row;
      }
      XA_AddData( 'Data', $rows );
      $result->free();
   } elseif ( !$result ) {
      XA_AddError( $oDB->error.' ('.$oDB->errno.')' );
   }
   return;
}

//------------------------------------------------------------------------------

function XA_GetCatalogs( $oDB, $cDBUserName, $cDBPassword ) {

   if ( !isset( $oDB ) ) {
      $oDB = new mysqli( 'p:localhost', $cDBUserName, $cDBPassword, NULL );
      if ( $oDB->connect_errno ) {
         XA_Exit( 'PHP ERROR: Failed to connect to MySQL: (' . $oDB->connect_errno . ') ' . $oDB->connect_error );
         return;
      }
   }

   $result = $oDB->query( 'SHOW databases' );
   if ( is_object( $result ) ) {
      $rows = Array();

      while ( $row = XA_FetchRow( $result ) ) {
         $rows[] = $row[ 0 ];
      }

      XA_AddData( 'Catalogs', $rows );
      $result->free();
   } elseif ( !$result ) {
      XA_AddError( $oDB->error.' ('.$oDB->errno.')' );
   }
   return;
}

//------------------------------------------------------------------------------

function XA_GetTables( $oDB, $cInfo ) {
   if ( !isset( $oDB ) ) {
      XA_Exit( 'PHP ERROR: Database name not set' );
      return;
   }

   $aInfo = explode( ",",  $cInfo );
   $cMask = str_replace( '*', '%', $aInfo[ 0 ] );
   $lView = ( $aInfo[ 1 ] == "1" );

   $cQuery = "SHOW FULL TABLES LIKE '$cMask'";
   $result = $oDB->query($cQuery);
   if ( is_object( $result ) ) {
      $rows = Array();

      while ($row = XA_FetchRow($result)) {
         if ( $lView || $row[ 1 ] == 'BASE TABLE' ) {
            $rows[] = $row[ 0 ];
         }
      }

      XA_AddData('Tables', $rows);
      $result->free();
   } elseif ( !$result ) {
      XA_AddError( $oDB->error.' ('.$oDB->errno.')' );
   }
   return;
}

//------------------------------------------------------------------------------

function XA_UploadFile( $cData ) {

   $pos = strpos( $cData, '|' );
   if( $pos > 0 ) {
      $cFilename = substr( $cData, 0, $pos );
      $cData = pack('H*', substr( $cData, $pos + 1 ) );
      $f = fopen( $cFilename, "w" );
      if( !$f ) {
         XA_AddError( 'Error writing file "'.$cFilename.'"' );
      } else {
         fwrite( $f, $cData );
         fclose( $f );
         XA_AddData( 'Upload', TRUE );
      }
   } else {
      XA_AddError( 'Error: unknown file to upload' );
   }

   XA_AddData( 'Upload', TRUE );
   return;
}

//------------------------------------------------------------------------------

function XA_DeleteFile( $cData ) {

   if( file_exists($cData) )
      if( unlink($cData)===false )
         XA_AddError( 'Error deleting file "'.$cData.'"' );
   return;
}

//------------------------------------------------------------------------------

function XA_CurrentDir( $cData ) {

   $dir = dirname(__FILE__);

   XA_AddData( 'Dir', $dir );
}

//------------------------------------------------------------------------------

function XA_IsFile( $cData ) {

   XA_AddData( 'File', is_file( $cData ) );
   return;
}

//------------------------------------------------------------------------------

function XA_IsDir( $cData ) {

   if( is_dir($cData) )
      XA_AddData( 'Dir', TRUE );
   else
      XA_AddData( 'Dir', FALSE );
   return;
}

//------------------------------------------------------------------------------

function XA_MkDir( $cData ) {

   if( mkdir($cData) )
      XA_AddData( 'Dir', TRUE );
   else
      XA_AddData( 'Dir', FALSE );
   return;
}

//------------------------------------------------------------------------------

function XA_AFiles( $cData ) {

   $dir    = dirname(__FILE__).'/'.$cData ;
   $aFiles = scandir( $dir );
   $result = array();

   foreach ($aFiles as $key => $value)
   {
      if (!in_array($value,array(".","..")))
      {
         if (is_file($dir . DIRECTORY_SEPARATOR . $value ) )
         {
            $result[] = $value;
         }
      }
   }

   XA_AddData('Dir', $dir);
   XA_AddData('Files', $result);

   return;
}

//------------------------------------------------------------------------------

function XA_ADirs( $cData ) {

   $dir    = dirname(__FILE__).'/'.$cData ;
   $aFiles = scandir( $dir );
   $result = array();

   foreach ($aFiles as $key => $value)
   {
      if (!in_array($value,array(".","..")))
      {
         if (is_dir($dir . DIRECTORY_SEPARATOR . $value ) )
         {
            $result[] = $value;
         }
      }
   }

   XA_AddData('Dir', $dir);
   XA_AddData('Folders', $result);

   return;
}

//------------------------------------------------------------------------------

function XA_FetchRow( $result ) {

   if ( !is_object( $result ) ) return NULL;

   $row = $result->fetch_row();

   if (!$row) return NULL;

   $ret = Array();
   $result->field_seek(0);

   foreach ($row as $value) {
      $oField = $result->fetch_field();
      $cType = XA_BasicType($oField->type);
      switch ($cType) {
      case "B":
         $ret[] = (bool) $value;
         break;
      case "N":
         $ret[] = (int) $value;
         break;
      case "F":
         $ret[] = (float) $value;
         break;
      case "D":
         $ret[] = '#Date->'.$value;
         break;
      case "C":
      case "M":
         $ret[] = $value;
         break;
      default:
         $ret[] = NULL;
      }
   }

   return $ret;
}

//------------------------------------------------------------------------------

function XA_AddData( $cCommand, $data ) {

   switch (gettype($data)) {
   case 'boolean':
      $cType = "L";
      if ( $data ) {
         $cData = 1;
      } else {
         $cData = 0;
      }
      break;
   case 'integer':
   case 'double':
      $cType = "N";
      $cData = $data;
      break;
   case 'string':
      if ( strncmp($data, '#Date->', 7) == 0 ) {
         $cType = "D";
         $cData = substr( $data, 7 );
      } else {
         $cType = "C";
         if ($cCommand == '') {
            $cData = bin2hex($data);
         } else {
            $cData = $data;
         }
         //$cData = bin2hex($data);
         //$cData = $data;
      }
      break;
   case 'array':
      $cType = "A";
      $cData = '';
      foreach ($data as $value) {
         $cData .= XA_AddData('', $value);
      }
      $cData = substr( $cData, 0, strlen($cData)-1);
      break;
   default:
      $cType = "C";
      $cData = '';
   }

   if ($cCommand != '' ) {
      $crlf = ( $cType == 'A' && count($data) > 0 && gettype($data[0]) == 'array' );
      $echo = "<$cCommand type='$cType'>";

      if ( $crlf ) {
         $cData = substr( $cData, 0, strlen($cData) - 4);
         $echo .= "\r\n   ".$cData."\r\n</$cCommand>\r\n";
      } else {
         $echo .= $cData."</$cCommand>\r\n";
      }

      echo $echo;

   } else {
      if ( $cType == "A" ) {
         $ret = $cType.'['.$cData."]\r\n   ";
      } else {
         $ret = $cType.'('.$cData.'),';
      }
      return $ret;
   }
}

//------------------------------------------------------------------------------

function XA_AddError( $data ) {
   XA_AddData('Error', bin2hex($data));
//   XA_AddData('cError', $data);
}

//------------------------------------------------------------------------------

function XA_QueryInfo( $query ) {

   $aInfo = $query->fetch_fields();

   echo( "<Fields_Info>\r\n");

   foreach ($aInfo as $aField) {

      echo "   <field";

      foreach ($aField as $name => $value) {
          echo " $name='$value'";
      }

      echo "/>\r\n";
   }

   echo( "</Fields_Info>\r\n");

   return;
}

//------------------------------------------------------------------------------

function XA_BasicType( $nType ) {

   switch ($nType) {
   case 1:
      $cType = "B";
      break;
   case 2:
   case 3:
   case 8:
   case 9:
      $cType = "N";
      break;
   case 0:
   case 4:
   case 5:
   case 246: // decimal
      $cType = "F";
      break;
   case 6:
      $cType = "X";
      break;
   case 7:
   case 10:
   case 11:
   case 12:
   case 13:
   case 14:
      $cType = "D";
      break;
   case 249:
   case 250:
   case 251:
   case 252:
      $cType = "M";
      break;
   case 253:
   case 254:
   case 255:
      $cType = "C";
      break;
   default:
      $cType = "";
   }

   return $cType;
}

//------------------------------------------------------------------------------

function XA_GetPrimaryKeys( $oDB, $table ){

   $aPK = Array();

   if ( $table == '' ) return $aPK;

   $qry = $oDB->query("SHOW INDEX FROM `$table` WHERE Key_name = 'PRIMARY'");

   while ($row = $qry->fetch_object()) {
      $aPK[] = $row->Column_name;
   }

   $qry->close();
   //return join(",", $aPK);
   return $aPK;
}

//------------------------------------------------------------------------------

function XA_GetPrimaryTable( $result ){

   $table = '';
   $result->field_seek(0);

   while ($oField = $result->fetch_field()) {
      $temp = $oField->orgtable;
      if ($temp != '') {
         if ( $table == '' ) {
            $table = $temp;
         } else {
            if ( $temp != $table ) {
               return '';
            }
         }
      }
   }

   return $table;
}

//------------------------------------------------------------------------------

function xa_ErrorHandler($code, $msg, $file, $line) {
   $file = basename($file);
   XA_Exit( "PHP ERROR: On line $line of file $file. Error: $msg ($code)" );
}

//------------------------------------------------------------------------------

function XA_Exit( $text ){

   if ($text != '' ) XA_AddError( $text );
   echo "</Query>\r\n";
   exit;
}

?>

