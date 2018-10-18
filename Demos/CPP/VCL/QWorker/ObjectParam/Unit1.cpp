//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DoJob(TQJob *AJob)
{
TQJson *AJson=(TQJson *)AJob->Data;
ShowMessage(AJson->AsJson);
}
void __fastcall TForm1::Button1Click(TObject *Sender)
{
TQJobGroup *AGroup=new TQJobGroup(false);
TQJson *AJson=new TQJson;
AJson->Add(L"Name")->AsString=L"Test Object";
AJson->Add(L"Id")->AsInteger=100;
AGroup->Add(DoJob,AJson,True,jdfFreeAsObject);
AGroup->MsgWaitFor();
delete AGroup;
}
//---------------------------------------------------------------------------

