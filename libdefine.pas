unit LibDefine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdSSLOpenSSLHeaders, BassLibs;

implementation

initialization
  BASS_DLL_PATH := './shared-lib/';
  IdOpenSSLSetLibPath( './shared-lib/' );
end.

