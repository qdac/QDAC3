//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "main.h"
#include "qmsgpack.hpp"
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
TQMsgPack *AMsgPack=new TQMsgPack;
try
  {
  ClientDataSet1->CreateDataSet();
  ClientDataSet1->Append();
  ClientDataSet1->FieldByName(L"Id")->AsInteger=10;
  ClientDataSet1->FieldByName(L"Name")->AsString=L"Hello";
  ClientDataSet1->Post();
  TMemoryStream *AStream=new TMemoryStream;
  OleVariant V=ClientDataSet1->Data;//OleVariant继承自Variant，但是保护继承，因而无法直接转为父类
  AMsgPack->Add(L"Data")->AsVariant=*((Variant*)&V);
  mmResult->Text=AMsgPack->AsString;
  ClientDataSet1->Close();
  ClientDataSet1->Data=AMsgPack->ItemByName(L"Data")->AsVariant;
  }
__finally
  {
  delete AMsgPack;
  }
}
//---------------------------------------------------------------------------

