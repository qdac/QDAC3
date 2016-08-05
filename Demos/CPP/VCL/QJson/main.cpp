//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "main.h"
#include "qjson.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm2 *Form2;
//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm2::Button1Click(TObject *Sender)
{
TQJson *QJson;
QJson=new TQJson();
TQJson *QArray=QJson->AddArray(L"Test");
TQJson *QItem=QArray->Add();
QItem->Add(L"Item1",0i64);
QItem->Add(L"Item2",true);
QItem=QArray->Add();
QItem->Add(L"Item1")->AsString=L"ÄãºÃ";
ShowMessage(QJson->Encode(True,False));
ShowMessage(QJson->Encode(True,True));
delete QJson;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::Button2Click(TObject *Sender)
{
TQJson *QJson=new TQJson();
QJson->SaveToFile("D:\\1.TXT",teUTF8,true);
delete QJson;
TQJson *QJson2=new TQJson();
QJson2->LoadFromFile("D:\\1.TXT",teUTF8);
delete QJson2;
}
//---------------------------------------------------------------------------

