//---------------------------------------------------------------------------

#include <vcl.h>
#include "qxml.hpp"
#pragma hdrstop

#include "main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
#define DELPHI_ANON(AType,ATypeName,Code) \
  class ATypeName:public TCppInterfacedObject<AType>\
	{\
	public:\
	  void __fastcall Invoke##Code\
	};

__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
DELPHI_ANON(TQXMLRttiFilterEventA,TMyEvent,
  (TQXMLNode* ASender, void * AObject, System::UnicodeString AName, System::Typinfo::PTypeInfo AType, bool &Accept, void * ATag)
  {
  if (AName=="Enabled")
	Accept=False;
  else
	Accept=True;
  }
);
TQXMLNode *AXML=new TQXMLNode;
AXML->AddObject("Button1",Button1,[&](TQXMLNode* ASender, void * AObject, System::UnicodeString AName, System::Typinfo::PTypeInfo AType, bool &Accept, void * ATag)
	{
	Accept=true;
	},
//new TMyEvent(),
	false);
ShowMessage(AXML->Encode(True,L" "));
delete AXML;
}
//---------------------------------------------------------------------------
